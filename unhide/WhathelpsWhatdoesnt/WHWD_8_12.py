import pandas as pd
import re
import os
import time

try:
    from rich.console import Console
    from rich.progress import (
        Progress,
        SpinnerColumn,
        TextColumn,
        BarColumn,
        TaskProgressColumn,
        TimeElapsedColumn,
        TimeRemainingColumn,
    )
except ImportError as exc:
    raise ImportError(
        "The rich package is required. Install it with: pip3 install rich"
    ) from exc


console = Console()


# =======================================================
# QUESTION MAPPING
# =======================================================

QUESTION_MAP = {
    "### Which treatments have **helped somewhat** with your symptoms, daily life, and overall health?":
        "MildlyEffectiveTreatments",

    "### Are there any treatments that **worked when you started** taking them but then **stopped working**?":
        "Worked then didn't",

    "### Which treatments have had **no effect** on your symptoms, daily life, or overall health?":
        "IneffectiveTreatments",

    "### Which treatments **worsened** your symptoms, quality of life, or overall health?":
        "DamagingTreatments",

    "### Please list any treatments that you had to discontinue due to **side effects**, even if the treatment helped certain symptoms.":
        "Side effects",

    "### Is there **anything else** that you would like to share about the treatments that have worked, what has not helped, and what your experience was like?":
        "Treatments narrative",

    "### Which treatments have **helped you the most** with your symptoms, daily life, and overall health?":
        "VeryEffectiveTreatments",
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
    "excluded": "Excluded_Responses",
}


# =======================================================
# TEXT CLEANING
# =======================================================

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


# =======================================================
# LOAD WORKBOOK
# =======================================================

def load_raw_and_mapping(path):

    xl = pd.ExcelFile(path)

    required = {"Data", "Mapping"}

    missing = required - set(xl.sheet_names)

    if missing:
        raise ValueError(
            f"Missing required sheet(s): {sorted(missing)}. "
            f"Found: {xl.sheet_names}"
        )

    data = pd.read_excel(
        path,
        sheet_name="Data"
    )

    mapping = pd.read_excel(
        path,
        sheet_name="Mapping"
    )

    # Clean column headers
    data.columns = data.columns.astype(str).str.strip()
    mapping.columns = mapping.columns.astype(str).str.strip()

    return data, mapping


# =======================================================
# PREPARE MAPPING
# =======================================================

def prepare_mapping(mapping):

    m = mapping.copy()

    # ---------------------------------------------------
    # Clean column names
    # ---------------------------------------------------

    m.columns = m.columns.astype(str).str.strip()


    # ---------------------------------------------------
    # Backward compatibility for old SubCategory column
    # ---------------------------------------------------

    if (
        "SubCategory" in m.columns
        and "SubCategory 1" not in m.columns
    ):
        m = m.rename(
            columns={
                "SubCategory": "SubCategory 1"
            }
        )


    # ---------------------------------------------------
    # Handle capitalization variation
    # ---------------------------------------------------

    if (
        "SubCategory 2" in m.columns
        and "Subcategory 2" not in m.columns
    ):
        m = m.rename(
            columns={
                "SubCategory 2": "Subcategory 2"
            }
        )


    # ---------------------------------------------------
    # Required columns
    # ---------------------------------------------------

    required_cols = [
        "keyword",
        "StandardTreatment",
        "Category",
    ]

    missing = [
        c for c in required_cols
        if c not in m.columns
    ]

    if missing:
        raise ValueError(
            f"Mapping sheet missing columns: {missing}. "
            f"Found columns: {m.columns.tolist()}"
        )


    # ---------------------------------------------------
    # Optional columns
    # ---------------------------------------------------

    optional_columns = [
        "SubCategory 1",
        "Subcategory 2",
        "Notes",
    ]

    for col in optional_columns:

        if col not in m.columns:
            m[col] = ""


    # ---------------------------------------------------
    # IMPORTANT FIX:
    # Force taxonomy fields to text.
    #
    # This prevents Excel dates/numbers from crashing
    # pandas during grouping.
    # ---------------------------------------------------

    text_columns = [
        "keyword",
        "StandardTreatment",
        "Category",
        "SubCategory 1",
        "Subcategory 2",
        "Notes",
    ]

    for col in text_columns:

        if col in m.columns:

            m[col] = (
                m[col]
                .fillna("")
                .astype(str)
                .str.strip()
            )


    # ---------------------------------------------------
    # Normalize keywords
    # ---------------------------------------------------

    m["keyword"] = (
        m["keyword"]
        .map(clean_keyword)
    )

    m = (
        m[
            m["keyword"].ne("")
        ]
        .copy()
    )


    # ---------------------------------------------------
    # MatchType
    #
    # whole = normal safe matching
    # partial = intentional partial/stem matching
    # ---------------------------------------------------

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


    # ---------------------------------------------------
    # Longer keywords first
    # ---------------------------------------------------

    m["keyword_length"] = (
        m["keyword"]
        .str.len()
    )

    m = (
        m
        .sort_values(
            [
                "keyword_length",
                "keyword"
            ],
            ascending=[
                False,
                True
            ]
        )
        .reset_index(drop=True)
    )

    return m


# =======================================================
# BUILD LONG DATA
# =======================================================

def build_long_data(data):

    if "participantidentifier" not in data.columns:

        raise ValueError(
            "Data sheet must contain participantidentifier"
        )


    long_df = data.melt(

        id_vars=[
            "participantidentifier"
        ],

        var_name="response_type_raw",

        value_name="raw_block"
    )


    long_df["response_type"] = (
        long_df["response_type_raw"]
        .map(QUESTION_MAP)
        .fillna(
            long_df["response_type_raw"]
        )
    )


    long_df["raw_block"] = (
        long_df["raw_block"]
        .fillna("")
        .astype(str)
        .str.strip()
    )


    long_df = (
        long_df[
            long_df["raw_block"].ne("")
        ]
        .copy()
    )


    rows = []


    with Progress(

        SpinnerColumn(),

        TextColumn(
            "[bold cyan]{task.description}"
        ),

        BarColumn(),

        TaskProgressColumn(),

        TimeElapsedColumn(),

        TimeRemainingColumn(),

        console=console,

    ) as progress:


        task = progress.add_task(

            "Splitting participant responses",

            total=len(long_df)

        )


        for _, r in long_df.iterrows():

            block = (
                str(r["raw_block"])
                .replace("\r\n", "\n")
                .replace("\r", "\n")
            )


            pieces = [

                p.strip()

                for p in re.split(
                    r"\n+",
                    block
                )

                if p.strip()

            ]


            for piece in pieces:

                rows.append({

                    "participantidentifier":
                        r["participantidentifier"],

                    "response_type":
                        r["response_type"],

                    "raw_text":
                        piece,

                    "clean_text":
                        normalize_text(piece),

                })


            progress.advance(task)


    return pd.DataFrame(rows)


# =======================================================
# FIND TREATMENT MATCHES
# =======================================================

def find_matches(text, mapping):

    """
    Matching rules:

    1. Whole word / whole phrase matching is default.

       Example:
       "ot" matches "OT"
       but does NOT match "protein"

    2. Longer phrases win over shorter overlapping phrases.

       Example:
       "physical therapy" wins over "therapy"

    3. Multiple separate treatments can still be found
       in one participant response.

    4. Optional MatchType = partial can be used for
       intentional stems such as:

       gabap -> gabapentin

    5. Known misspellings should be added as keyword rows
       in the Mapping sheet.
    """


    candidates = []


    for _, row in mapping.iterrows():

        kw = row["keyword"]

        if not kw:
            continue


        match_type = (
            str(
                row.get(
                    "MatchType",
                    "whole"
                )
            )
            .strip()
            .lower()
        )


        # -----------------------------------------------
        # Partial/stem match
        # -----------------------------------------------

        if match_type in {
            "partial",
            "contains",
            "substring"
        }:

            pattern = re.escape(kw)


        # -----------------------------------------------
        # Safe whole word / phrase
        # -----------------------------------------------

        else:

            pattern = (
                r"(?<!\w)"
                + re.escape(kw)
                + r"(?!\w)"
            )


        for hit in re.finditer(
            pattern,
            text,
            flags=re.IGNORECASE
        ):


            candidates.append({

                "start":
                    hit.start(),

                "end":
                    hit.end(),

                "length":
                    hit.end()
                    - hit.start(),

                "matched_keyword":
                    kw,

                "StandardTreatment":
                    str(
                        row["StandardTreatment"]
                    ).strip(),

                "Category":
                    str(
                        row["Category"]
                    ).strip(),

                "SubCategory 1":
                    str(
                        row.get(
                            "SubCategory 1",
                            ""
                        )
                    ).strip(),

                "Subcategory 2":
                    str(
                        row.get(
                            "Subcategory 2",
                            ""
                        )
                    ).strip(),

                "Notes":
                    str(
                        row.get(
                            "Notes",
                            ""
                        )
                    ).strip(),

            })


    # ---------------------------------------------------
    # Longer/more-specific phrases first
    # ---------------------------------------------------

    candidates.sort(

        key=lambda x: (

            -x["length"],

            x["start"],

            x["matched_keyword"]

        )

    )


    selected = []

    occupied_spans = []

    selected_treatments = set()


    for candidate in candidates:


        treatment_key = (

            candidate[
                "StandardTreatment"
            ],

            candidate[
                "Category"
            ],

            candidate[
                "SubCategory 1"
            ],

            candidate[
                "Subcategory 2"
            ],

        )


        # -----------------------------------------------
        # Don't duplicate same standardized treatment
        # in the same response
        # -----------------------------------------------

        if treatment_key in selected_treatments:
            continue


        # -----------------------------------------------
        # Check overlapping phrase
        # -----------------------------------------------

        overlaps = any(

            candidate["start"]
            < existing_end

            and

            candidate["end"]
            > existing_start

            for (
                existing_start,
                existing_end
            )

            in occupied_spans

        )


        # Example:
        #
        # physical therapy
        #
        # suppress:
        # therapy
        # -----------------------------------------------

        if overlaps:
            continue


        selected.append({

            "matched_keyword":
                candidate[
                    "matched_keyword"
                ],

            "StandardTreatment":
                candidate[
                    "StandardTreatment"
                ],

            "Category":
                candidate[
                    "Category"
                ],

            "SubCategory 1":
                candidate[
                    "SubCategory 1"
                ],

            "Subcategory 2":
                candidate[
                    "Subcategory 2"
                ],

            "Notes":
                candidate[
                    "Notes"
                ],

        })


        occupied_spans.append(

            (
                candidate["start"],
                candidate["end"]
            )

        )


        selected_treatments.add(
            treatment_key
        )


    return selected


# =======================================================
# CODE TREATMENTS
# =======================================================

def code_treatments(
    long_df,
    mapping
):


    coded_rows = []

    unmatched_rows = []


    with Progress(

        SpinnerColumn(),

        TextColumn(
            "[bold magenta]{task.description}"
        ),

        BarColumn(),

        TaskProgressColumn(),

        TimeElapsedColumn(),

        TimeRemainingColumn(),

        console=console,

    ) as progress:


        task = progress.add_task(

            "Matching treatments",

            total=len(long_df)

        )


        for _, r in long_df.iterrows():


            matches = find_matches(

                r["clean_text"],

                mapping

            )


            if matches:


                for m in matches:


                    notes = (
                        str(
                            m.get(
                                "Notes",
                                ""
                            )
                        )
                        .strip()
                    )


                    # -----------------------------------
                    # PI exclusion rule:
                    #
                    # Anything where Notes contains
                    # "delete" is excluded from analysis.
                    # -----------------------------------

                    status = (

                        "Excluded"

                        if "delete"
                        in notes.lower()

                        else "Mapped"

                    )


                    coded_rows.append({

                        "participantidentifier":
                            r[
                                "participantidentifier"
                            ],

                        "response_type":
                            r[
                                "response_type"
                            ],

                        "raw_text":
                            r[
                                "raw_text"
                            ],

                        "clean_text":
                            r[
                                "clean_text"
                            ],

                        "matched_keyword":
                            m[
                                "matched_keyword"
                            ],

                        "StandardTreatment":
                            m[
                                "StandardTreatment"
                            ],

                        "Category":
                            m[
                                "Category"
                            ],

                        "SubCategory 1":
                            m[
                                "SubCategory 1"
                            ],

                        "Subcategory 2":
                            m[
                                "Subcategory 2"
                            ],

                        "Notes":
                            notes,

                        "Status":
                            status,

                    })


            else:


                unmatched_rows.append({

                    "participantidentifier":
                        r[
                            "participantidentifier"
                        ],

                    "response_type":
                        r[
                            "response_type"
                        ],

                    "raw_text":
                        r[
                            "raw_text"
                        ],

                    "clean_text":
                        r[
                            "clean_text"
                        ],

                })


            progress.advance(task)


    coded_df = pd.DataFrame(
        coded_rows
    )


    unmatched_df = pd.DataFrame(
        unmatched_rows
    )


    if coded_df.empty:


        coded_df = pd.DataFrame(

            columns=[

                "participantidentifier",

                "response_type",

                "raw_text",

                "clean_text",

                "matched_keyword",

                "StandardTreatment",

                "Category",

                "SubCategory 1",

                "Subcategory 2",

                "Notes",

                "Status",

            ]

        )


    if unmatched_df.empty:


        unmatched_df = pd.DataFrame(

            columns=[

                "participantidentifier",

                "response_type",

                "raw_text",

                "clean_text",

            ]

        )


    return (
        coded_df,
        unmatched_df
    )


# =======================================================
# CLEAN TAXONOMY BEFORE SUMMARIES
# =======================================================

def clean_summary_columns(df):

    """
    Force taxonomy columns to strings before groupby.

    This specifically prevents errors such as:

    TypeError:
    '<' not supported between instances of
    'int' and 'datetime.datetime'
    """


    df = df.copy()


    text_columns = [

        "Category",

        "SubCategory 1",

        "Subcategory 2",

        "StandardTreatment",

        "participantidentifier",

    ]


    for col in text_columns:


        if col in df.columns:


            df[col] = (

                df[col]
                .fillna("")
                .astype(str)
                .str.strip()

            )


    return df


# =======================================================
# SUMMARY BY QUESTION
# =======================================================

def summarize_by_question(
    coded_df,
    response_type
):


    df = coded_df[

        (
            coded_df["response_type"]
            == response_type
        )

        &

        (
            coded_df["Status"]
            == "Mapped"
        )

    ].copy()


    if df.empty:


        return pd.DataFrame(

            columns=[

                "Category",

                "SubCategory 1",

                "Subcategory 2",

                "StandardTreatment",

                "count_mentions",

                "unique_participants",

            ]

        )


    # IMPORTANT:
    # force Excel dates/numbers to text

    df = clean_summary_columns(df)


    summary = (

        df

        .groupby(

            [

                "Category",

                "SubCategory 1",

                "Subcategory 2",

                "StandardTreatment",

            ],

            dropna=False,

            sort=False,

        )

        .agg(

            count_mentions=(

                "StandardTreatment",

                "size"

            ),

            unique_participants=(

                "participantidentifier",

                "nunique"

            ),

        )

        .reset_index()

        .sort_values(

            [

                "count_mentions",

                "unique_participants",

                "Category",

                "StandardTreatment",

            ],

            ascending=[

                False,

                False,

                True,

                True,

            ]

        )

    )


    return summary


# =======================================================
# OVERALL SUMMARY
# =======================================================

def summarize_overall(
    coded_df
):


    if coded_df.empty:


        return pd.DataFrame(

            columns=[

                "Category",

                "SubCategory 1",

                "Subcategory 2",

                "StandardTreatment",

                "count_mentions",

                "unique_participants",

            ]

        )


    overall_source = coded_df[

        coded_df["Status"]
        == "Mapped"

    ].copy()


    if overall_source.empty:


        return pd.DataFrame(

            columns=[

                "Category",

                "SubCategory 1",

                "Subcategory 2",

                "StandardTreatment",

                "count_mentions",

                "unique_participants",

            ]

        )


    # IMPORTANT:
    # force Excel dates/numbers to text

    overall_source = (
        clean_summary_columns(
            overall_source
        )
    )


    overall = (

        overall_source

        .groupby(

            [

                "Category",

                "SubCategory 1",

                "Subcategory 2",

                "StandardTreatment",

            ],

            dropna=False,

            sort=False,

        )

        .agg(

            count_mentions=(

                "StandardTreatment",

                "size"

            ),

            unique_participants=(

                "participantidentifier",

                "nunique"

            ),

        )

        .reset_index()

        .sort_values(

            [

                "count_mentions",

                "unique_participants",

                "Category",

                "StandardTreatment",

            ],

            ascending=[

                False,

                False,

                True,

                True,

            ]

        )

    )


    return overall


# =======================================================
# AUTOFIT EXCEL COLUMNS
# =======================================================

def autofit_columns(
    writer,
    sheet_name,
    df,
    sample_rows=200
):


    worksheet = writer.sheets[
        sheet_name
    ]


    if df.empty:


        for i, col in enumerate(
            df.columns
        ):


            worksheet.set_column(

                i,

                i,

                min(
                    len(
                        str(col)
                    ) + 2,

                    40

                )

            )


        return


    for i, col in enumerate(
        df.columns
    ):


        sample = (

            df[col]

            .head(
                sample_rows
            )

            .fillna("")

            .astype(str)

        )


        max_len = max(

            [
                len(
                    str(col)
                )
            ]

            +

            [
                len(x)

                for x in
                sample.tolist()
            ]

        )


        worksheet.set_column(

            i,

            i,

            min(

                max_len + 2,

                50

            )

        )


# =======================================================
# ADD BAR CHART
# =======================================================

def add_bar_chart(
    writer,
    workbook,
    sheet_name,
    df,
    title,
    value_col="unique_participants",
    category_col="StandardTreatment",
    insert_cell="H2"
):


    if df.empty:
        return


    worksheet = writer.sheets[
        sheet_name
    ]


    value_idx = (
        df.columns
        .get_loc(
            value_col
        )
    )


    category_idx = (
        df.columns
        .get_loc(
            category_col
        )
    )


    chart = workbook.add_chart({

        "type": "bar"

    })


    chart.add_series({

        "name":
            value_col,

        "categories": [

            sheet_name,

            1,

            category_idx,

            len(df),

            category_idx

        ],

        "values": [

            sheet_name,

            1,

            value_idx,

            len(df),

            value_idx

        ],

    })


    chart.set_title({

        "name":
            title

    })


    chart.set_x_axis({

        "name":
            value_col

    })


    chart.set_y_axis({

        "name":
            category_col

    })


    chart.set_legend({

        "none": True

    })


    worksheet.insert_chart(

        insert_cell,

        chart

    )


# =======================================================
# WRITE OUTPUT WORKBOOK
# =======================================================

def write_output(
    output_path,
    long_df,
    coded_df,
    unmatched_df
):


    console.print(
        "[cyan]Building question summaries...[/cyan]"
    )


    question_summaries = {}


    for (
        response_type,
        sheet_name
    ) in VISIBLE_SHEET_MAP.items():


        question_summaries[
            sheet_name
        ] = summarize_by_question(

            coded_df,

            response_type

        )


    console.print(
        "[cyan]Building overall summary...[/cyan]"
    )


    overall_summary = (
        summarize_overall(
            coded_df
        )
    )


    excluded_df = coded_df[

        coded_df["Status"]
        == "Excluded"

    ].copy()


    console.print(
        f"[yellow]Excluded responses: {len(excluded_df):,}[/yellow]"
    )


    with pd.ExcelWriter(

        output_path,

        engine="xlsxwriter"

    ) as writer:


        # ===============================================
        # VISIBLE TABS
        # ===============================================

        overall_summary.to_excel(

            writer,

            sheet_name=
                OUTPUT_SHEETS[
                    "overall"
                ],

            index=False

        )


        for (
            sheet_name,
            df
        ) in question_summaries.items():


            df.to_excel(

                writer,

                sheet_name=
                    sheet_name[:31],

                index=False

            )


        # ===============================================
        # BACKEND / QA TABS
        # ===============================================

        long_df.to_excel(

            writer,

            sheet_name=
                OUTPUT_SHEETS[
                    "long"
                ],

            index=False

        )


        coded_df.to_excel(

            writer,

            sheet_name=
                OUTPUT_SHEETS[
                    "coded"
                ],

            index=False

        )


        unmatched_df.to_excel(

            writer,

            sheet_name=
                OUTPUT_SHEETS[
                    "unmatched"
                ],

            index=False

        )


        excluded_df.to_excel(

            writer,

            sheet_name=
                OUTPUT_SHEETS[
                    "excluded"
                ],

            index=False

        )


        # ===============================================
        # AUTOFIT
        # ===============================================

        autofit_columns(

            writer,

            OUTPUT_SHEETS[
                "overall"
            ],

            overall_summary

        )


        for (
            sheet_name,
            df
        ) in question_summaries.items():


            autofit_columns(

                writer,

                sheet_name[:31],

                df

            )


        autofit_columns(

            writer,

            OUTPUT_SHEETS[
                "long"
            ],

            long_df

        )


        autofit_columns(

            writer,

            OUTPUT_SHEETS[
                "coded"
            ],

            coded_df

        )


        autofit_columns(

            writer,

            OUTPUT_SHEETS[
                "unmatched"
            ],

            unmatched_df

        )


        autofit_columns(

            writer,

            OUTPUT_SHEETS[
                "excluded"
            ],

            excluded_df

        )


        workbook = writer.book


        # ===============================================
        # OVERALL CHART
        # ===============================================

        add_bar_chart(

            writer,

            workbook,

            OUTPUT_SHEETS[
                "overall"
            ],

            overall_summary.head(15),

            title=
                "Overall Top Treatments",

            value_col=
                "unique_participants",

            category_col=
                "StandardTreatment",

            insert_cell=
                "H2"

        )


        # ===============================================
        # QUESTION CHARTS
        # ===============================================

        for (
            sheet_name,
            df
        ) in question_summaries.items():


            add_bar_chart(

                writer,

                workbook,

                sheet_name[:31],

                df.head(15),

                title=
                    f"Top Treatments - {sheet_name}",

                value_col=
                    "unique_participants",

                category_col=
                    "StandardTreatment",

                insert_cell=
                    "H2"

            )


        # ===============================================
        # HIDE BACKEND TABS
        # ===============================================

        writer.sheets[
            OUTPUT_SHEETS["long"]
        ].hide()


        writer.sheets[
            OUTPUT_SHEETS["coded"]
        ].hide()


        writer.sheets[
            OUTPUT_SHEETS["unmatched"]
        ].hide()


        writer.sheets[
            OUTPUT_SHEETS["excluded"]
        ].hide()


# =======================================================
# MAIN
# =======================================================

def main():


    start_time = (
        time.perf_counter()
    )


    # ===================================================
    # INPUT / OUTPUT PATHS
    # ===================================================

    input_path = (
        "/Users/jameshunt/Desktop/BIC Data/"
        "Treatment Effectivness/"
        "WHWD Map and Data.xlsx"
    )


    output_path = (
        "/Users/jameshunt/Desktop/BIC Data/"
        "Treatment Effectivness/"
        "August12thWHWD_python_output.xlsx"
    )


    # ===================================================
    # CHECK FILE
    # ===================================================

    if not os.path.exists(
        input_path
    ):


        raise FileNotFoundError(

            f"File not found: {input_path}"

        )


    # ===================================================
    # START
    # ===================================================

    console.rule(

        "[bold]Treatment Mapping Run"

    )


    console.print(

        f"[bold]Input:[/bold] "
        f"{input_path}"

    )


    console.print(

        f"[bold]Output:[/bold] "
        f"{output_path}\n"

    )


    # ===================================================
    # LOAD WORKBOOK
    # ===================================================

    with console.status(

        "[bold cyan]"
        "Reading Data and Mapping sheets..."
    ):


        data, mapping = (
            load_raw_and_mapping(
                input_path
            )
        )


    console.print(

        f"[green]✓[/green] Workbook loaded: "
        f"{len(data):,} data rows and "
        f"{len(mapping):,} mapping rows"

    )


    # ===================================================
    # PREPARE MAPPING
    # ===================================================

    with console.status(

        "[bold cyan]"
        "Preparing mapping keywords..."
    ):


        mapping = (
            prepare_mapping(
                mapping
            )
        )


    console.print(

        f"[green]✓[/green] "
        f"Prepared "
        f"{len(mapping):,} "
        f"mapping keywords"

    )


    # ===================================================
    # LONG DATA
    # ===================================================

    long_df = (
        build_long_data(
            data
        )
    )


    console.print(

        f"[green]✓[/green] "
        f"Created "
        f"{len(long_df):,} "
        f"response rows"

    )


    # ===================================================
    # MAP TREATMENTS
    # ===================================================

    coded_df, unmatched_df = (
        code_treatments(

            long_df,

            mapping

        )
    )


    mapped_count = (

        coded_df[
            coded_df["Status"]
            == "Mapped"
        ]

        .shape[0]

    )


    excluded_count = (

        coded_df[
            coded_df["Status"]
            == "Excluded"
        ]

        .shape[0]

    )


    console.print(

        f"[green]✓[/green] "
        f"Mapping complete"

    )


    console.print(

        f"  Mapped rows: "
        f"{mapped_count:,}"

    )


    console.print(

        f"  Excluded rows: "
        f"{excluded_count:,}"

    )


    console.print(

        f"  Unmatched rows: "
        f"{len(unmatched_df):,}"

    )


    # ===================================================
    # WRITE EXCEL
    # ===================================================

    with console.status(

        "[bold cyan]"
        "Creating summaries, charts, "
        "and Excel workbook..."
    ):


        write_output(

            output_path,

            long_df,

            coded_df,

            unmatched_df

        )


    # ===================================================
    # FINISH
    # ===================================================

    elapsed = (

        time.perf_counter()
        - start_time

    )


    console.rule(

        "[bold green]"
        "Run Complete"

    )


    console.print(

        "[bold green]"
        "✓ Output saved to:"
        "[/bold green] "
        f"{output_path}"

    )


    console.print(

        f"Rows in long data: "
        f"{len(long_df):,}"

    )


    console.print(

        f"Mapped rows: "
        f"{mapped_count:,}"

    )


    console.print(

        f"Excluded rows: "
        f"{excluded_count:,}"

    )


    console.print(

        f"Unmatched rows: "
        f"{len(unmatched_df):,}"

    )


    console.print(

        f"Total run time: "
        f"{elapsed:,.1f} seconds"

    )


# =======================================================
# RUN
# =======================================================

if __name__ == "__main__":

    main()
