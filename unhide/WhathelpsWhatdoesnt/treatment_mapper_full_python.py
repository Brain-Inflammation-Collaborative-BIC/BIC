import pandas as pd
import re
import sys
import os

# Matching notes:
# - Longer, more specific phrases take priority over broader overlapping terms.
# - Keywords use whole-word/whole-phrase matching by default.
# - Add an optional MatchType column to the Mapping sheet and use "partial"
#   only for intentional stems such as "gabap" -> "gabapentin".
# - Common misspellings should be added as separate keyword rows mapped to the
#   same StandardTreatment. This is safer than automatically fuzzy-mapping text.

QUESTION_MAP = {
    "### Which treatments have **helped somewhat** with your symptoms, daily life, and overall health?": "MildlyEffectiveTreatments",
    "### Are there any treatments that **worked when you started** taking them but then **stopped working**?": "Worked then didn't",
    "### Which treatments have had **no effect** on your symptoms, daily life, or overall health?": "IneffectiveTreatments",
    "### Which treatments **worsened** your symptoms, quality of life, or overall health?": "DamagingTreatments",
    "### Please list any treatments that you had to discontinue due to **side effects**, even if the treatment helped certain symptoms.": "Side effects",
    "### Is there **anything else** that you would like to share about the treatments that have worked, what has not helped, and what your experience was like?": "Treatments narrative",
    "### Which treatments have **helped you the most** with your symptoms, daily life, and overall health?": "VeryEffectiveTreatments",
}

VISIBLE_SHEET_MAP = {
    "VeryEffectiveTreatments": "VeryEffective",
    "MildlyEffectiveTreatments": "MildlyEffective",
    "IneffectiveTreatments": "Ineffective",
    "DamagingTreatments": "Damaging",
    "Worked then didn't": "WorkedThenDidnt",
    "Side effects": "SideEffects",
    "Treatments narrative": "Narrative",
}

OUTPUT_SHEETS = {
    "overall": "Overall_Summary",
    "long": "Data_Long_Python",
    "coded": "Treatment_Coded_Python",
    "unmatched": "Unmatched_Python",
}

def normalize_text(s):
    if pd.isna(s):
        return ""
    s = str(s).strip().lower()
    replacements = {
        "‚Äôs": "'s",
        "‚Äô": "'",
        "’": "'",
        "“": '"',
        "”": '"',
        "‚Äú": '"',
        "‚Äù": '"',
        "‚Äì": "-",
        "‚Äî": "-",
        "–": "-",
        "—": "-",
        "\xa0": " ",
    }
    for old, new in replacements.items():
        s = s.replace(old, new)
    s = re.sub(r"\s+", " ", s).strip()
    return s

def clean_keyword(s):
    return normalize_text(s)

def load_raw_and_mapping(path):
    xl = pd.ExcelFile(path)
    required = {"Data", "Mapping"}
    missing = required - set(xl.sheet_names)
    if missing:
        raise ValueError(f"Missing required sheet(s): {sorted(missing)}. Found: {xl.sheet_names}")
    data = pd.read_excel(path, sheet_name="Data")
    mapping = pd.read_excel(path, sheet_name="Mapping")
    return data, mapping

def prepare_mapping(mapping):
    required_cols = ["keyword", "StandardTreatment", "Category", "SubCategory"]
    missing = [c for c in required_cols if c not in mapping.columns]
    if missing:
        raise ValueError(f"Mapping sheet missing columns: {missing}")

    m = mapping.copy()
    m["keyword"] = m["keyword"].astype(str).map(clean_keyword)
    m = m[m["keyword"].ne("")].copy()

    # Optional column. Default behavior is safe whole-word/whole-phrase matching.
    # Use MatchType = partial only for intentional stems, such as:
    # gabap -> gabapentin
    if "MatchType" not in m.columns:
        m["MatchType"] = "whole"
    else:
        m["MatchType"] = (
            m["MatchType"]
            .fillna("whole")
            .astype(str)
            .str.strip()
            .str.lower()
        )

    m["keyword_length"] = m["keyword"].str.len()
    m = m.sort_values(
        ["keyword_length", "keyword"],
        ascending=[False, True]
    ).reset_index(drop=True)
    return m

def build_long_data(data):
    if "participantidentifier" not in data.columns:
        raise ValueError("Data sheet must contain participantidentifier")

    long_df = data.melt(
        id_vars=["participantidentifier"],
        var_name="response_type_raw",
        value_name="raw_block"
    )

    long_df["response_type"] = long_df["response_type_raw"].map(QUESTION_MAP).fillna(long_df["response_type_raw"])
    long_df["raw_block"] = long_df["raw_block"].fillna("").astype(str).str.strip()
    long_df = long_df[long_df["raw_block"].ne("")].copy()

    rows = []
    for _, r in long_df.iterrows():
        block = str(r["raw_block"]).replace("\r\n", "\n").replace("\r", "\n")
        pieces = [p.strip() for p in re.split(r"\n+", block) if p.strip()]
        for piece in pieces:
            rows.append({
                "participantidentifier": r["participantidentifier"],
                "response_type": r["response_type"],
                "raw_text": piece,
                "clean_text": normalize_text(piece),
            })

    return pd.DataFrame(rows)

def find_matches(text, mapping):
    """
    Match treatments while preventing broad keywords from overriding specific ones.

    Examples:
      - "ot" matches standalone "OT", but not "protein" or "botox".
      - "physical therapy" wins over the broader overlapping keyword "therapy".
      - Non-overlapping treatments in the same response can still both be mapped.

    Poor spelling/grammar:
      - Text normalization handles capitalization, spacing, punctuation, and common
        encoding problems.
      - Add known misspellings as Mapping-sheet keyword rows (for example,
        "therepy" and "therpay") mapped to the intended StandardTreatment.
      - Use optional MatchType = "partial" only for intentional word stems.
    """
    candidates = []

    for _, row in mapping.iterrows():
        kw = row["keyword"]
        if not kw:
            continue

        match_type = str(row.get("MatchType", "whole")).strip().lower()

        if match_type in {"partial", "contains", "substring"}:
            pattern = re.escape(kw)
        else:
            # Whole word/phrase boundaries. This prevents "ot" matching "protein".
            pattern = r"(?<!\w)" + re.escape(kw) + r"(?!\w)"

        for hit in re.finditer(pattern, text, flags=re.IGNORECASE):
            candidates.append({
                "start": hit.start(),
                "end": hit.end(),
                "length": hit.end() - hit.start(),
                "matched_keyword": kw,
                "StandardTreatment": row["StandardTreatment"],
                "Category": row["Category"],
                "SubCategory": row["SubCategory"],
            })

    # Longest phrases first, then earliest position.
    candidates.sort(key=lambda x: (-x["length"], x["start"], x["matched_keyword"]))

    selected = []
    occupied_spans = []
    selected_treatments = set()

    for candidate in candidates:
        treatment_key = (
            candidate["StandardTreatment"],
            candidate["Category"],
            candidate["SubCategory"],
        )

        # Keep only the most specific keyword for the same standardized treatment.
        if treatment_key in selected_treatments:
            continue

        overlaps = any(
            candidate["start"] < existing_end
            and candidate["end"] > existing_start
            for existing_start, existing_end in occupied_spans
        )

        # A longer phrase already claimed this part of the response.
        # Example: keep "physical therapy" and suppress overlapping "therapy".
        if overlaps:
            continue

        selected.append({
            "matched_keyword": candidate["matched_keyword"],
            "StandardTreatment": candidate["StandardTreatment"],
            "Category": candidate["Category"],
            "SubCategory": candidate["SubCategory"],
        })
        occupied_spans.append((candidate["start"], candidate["end"]))
        selected_treatments.add(treatment_key)

    return selected

def code_treatments(long_df, mapping):
    coded_rows = []
    unmatched_rows = []

    for _, r in long_df.iterrows():
        matches = find_matches(r["clean_text"], mapping)
        if matches:
            for m in matches:
                coded_rows.append({
                    "participantidentifier": r["participantidentifier"],
                    "response_type": r["response_type"],
                    "raw_text": r["raw_text"],
                    "clean_text": r["clean_text"],
                    "matched_keyword": m["matched_keyword"],
                    "StandardTreatment": m["StandardTreatment"],
                    "Category": m["Category"],
                    "SubCategory": m["SubCategory"],
                    "Status": "Mapped",
                })
        else:
            unmatched_rows.append({
                "participantidentifier": r["participantidentifier"],
                "response_type": r["response_type"],
                "raw_text": r["raw_text"],
                "clean_text": r["clean_text"],
            })

    coded_df = pd.DataFrame(coded_rows)
    unmatched_df = pd.DataFrame(unmatched_rows)

    if coded_df.empty:
        coded_df = pd.DataFrame(columns=[
            "participantidentifier", "response_type", "raw_text", "clean_text",
            "matched_keyword", "StandardTreatment", "Category", "SubCategory",
            "Status"
        ])

    return coded_df, unmatched_df

def summarize_by_question(coded_df, response_type):
    df = coded_df[coded_df["response_type"] == response_type].copy()
    if df.empty:
        return pd.DataFrame(columns=[
            "Category", "StandardTreatment", "count_mentions", "unique_participants"
        ])

    summary = (
        df.groupby(["Category", "StandardTreatment"], dropna=False)
        .agg(
            count_mentions=("StandardTreatment", "size"),
            unique_participants=("participantidentifier", "nunique"),
        )
        .reset_index()
        .sort_values(
            ["count_mentions", "unique_participants", "Category", "StandardTreatment"],
            ascending=[False, False, True, True]
        )
    )
    return summary

def summarize_overall(coded_df):
    if coded_df.empty:
        return pd.DataFrame(columns=[
            "Category", "StandardTreatment", "count_mentions", "unique_participants"
        ])

    overall = (
        coded_df.groupby(["Category", "StandardTreatment"], dropna=False)
        .agg(
            count_mentions=("StandardTreatment", "size"),
            unique_participants=("participantidentifier", "nunique"),
        )
        .reset_index()
        .sort_values(
            ["count_mentions", "unique_participants", "Category", "StandardTreatment"],
            ascending=[False, False, True, True]
        )
    )
    return overall

def autofit_columns(writer, sheet_name, df, sample_rows=200):
    worksheet = writer.sheets[sheet_name]
    if df.empty:
        for i, col in enumerate(df.columns):
            worksheet.set_column(i, i, min(len(str(col)) + 2, 40))
        return

    for i, col in enumerate(df.columns):
        sample = df[col].head(sample_rows).fillna("").astype(str)
        max_len = max([len(str(col))] + [len(x) for x in sample.tolist()])
        worksheet.set_column(i, i, min(max_len + 2, 50))

def add_bar_chart(writer, workbook, sheet_name, df, title, value_col="unique_participants", category_col="StandardTreatment", insert_cell="G2"):
    if df.empty:
        return

    worksheet = writer.sheets[sheet_name]
    value_idx = df.columns.get_loc(value_col)
    category_idx = df.columns.get_loc(category_col)

    chart = workbook.add_chart({"type": "bar"})
    chart.add_series({
        "name": value_col,
        "categories": [sheet_name, 1, category_idx, len(df), category_idx],
        "values": [sheet_name, 1, value_idx, len(df), value_idx],
    })
    chart.set_title({"name": title})
    chart.set_x_axis({"name": value_col})
    chart.set_y_axis({"name": category_col})
    chart.set_legend({"none": True})

    worksheet.insert_chart(insert_cell, chart)

def write_output(output_path, long_df, coded_df, unmatched_df):
    question_summaries = {}
    for response_type, sheet_name in VISIBLE_SHEET_MAP.items():
        question_summaries[sheet_name] = summarize_by_question(coded_df, response_type)

    overall_summary = summarize_overall(coded_df)

    with pd.ExcelWriter(output_path, engine="xlsxwriter") as writer:
        # Visible tabs first
        overall_summary.to_excel(writer, sheet_name=OUTPUT_SHEETS["overall"], index=False)

        for sheet_name, df in question_summaries.items():
            df.to_excel(writer, sheet_name=sheet_name[:31], index=False)

        # Backend tabs
        long_df.to_excel(writer, sheet_name=OUTPUT_SHEETS["long"], index=False)
        coded_df.to_excel(writer, sheet_name=OUTPUT_SHEETS["coded"], index=False)
        unmatched_df.to_excel(writer, sheet_name=OUTPUT_SHEETS["unmatched"], index=False)

        # Autofit
        autofit_columns(writer, OUTPUT_SHEETS["overall"], overall_summary)

        for sheet_name, df in question_summaries.items():
            autofit_columns(writer, sheet_name[:31], df)

        autofit_columns(writer, OUTPUT_SHEETS["long"], long_df)
        autofit_columns(writer, OUTPUT_SHEETS["coded"], coded_df)
        autofit_columns(writer, OUTPUT_SHEETS["unmatched"], unmatched_df)

        workbook = writer.book

        # Charts
        add_bar_chart(
            writer, workbook,
            OUTPUT_SHEETS["overall"],
            overall_summary.head(15),
            title="Overall Top Treatments",
            value_col="unique_participants",
            category_col="StandardTreatment",
            insert_cell="G2"
        )

        for sheet_name, df in question_summaries.items():
            add_bar_chart(
                writer, workbook,
                sheet_name[:31],
                df.head(15),
                title=f"Top Treatments - {sheet_name}",
                value_col="unique_participants",
                category_col="StandardTreatment",
                insert_cell="G2"
            )

        # Hide backend tabs
        writer.sheets[OUTPUT_SHEETS["long"]].hide()
        writer.sheets[OUTPUT_SHEETS["coded"]].hide()
        writer.sheets[OUTPUT_SHEETS["unmatched"]].hide()

def main():

    input_path = "/Users/jameshunt/Desktop/BIC Data/Treatment Effectivness/July15thWHWD.xlsx"
    output_path = "/Users/jameshunt/Desktop/BIC Data/Treatment Effectivness/July15thWHWD_python_output.xlsx"

    if not os.path.exists(input_path):
        raise FileNotFoundError(f"File not found: {input_path}")
    

    data, mapping = load_raw_and_mapping(input_path)
    mapping = prepare_mapping(mapping)
    long_df = build_long_data(data)
    coded_df, unmatched_df = code_treatments(long_df, mapping)
    write_output(output_path, long_df, coded_df, unmatched_df)

    print(f"Done. Output saved to: {output_path}")
    print(f"Rows in long data: {len(long_df):,}")
    print(f"Mapped rows: {len(coded_df):,}")
    print(f"Unmatched rows: {len(unmatched_df):,}")

if __name__ == "__main__":
    main()
