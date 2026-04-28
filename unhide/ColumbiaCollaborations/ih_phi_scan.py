from pathlib import Path
import re
import pandas as pd

# --------------------------------------------------
# FILE PATH
# --------------------------------------------------
DATA_DIR = Path(
    "/Users/kelseyaguirre/Library/Mobile Documents/"
    "com~apple~CloudDocs/Documents/"
    "BrainInflammationCollaborative/Unhide/Mara_Columbia/"
    "Shared with Mara"
)

input_file = DATA_DIR / "4_23_26_SurveyandBFQDatav3.3.xlsx"

# --------------------------------------------------
# COLUMNS TO CHECK: ILLNESS HISTORY
# --------------------------------------------------
columns_to_check = [
    "illnesshistory_category",
    "illnesshistory_circumstances",
    "illnesshistory_covid",
    "illnesshistory_covidvaxx",
    "illnesshistory_ebv",
    "illnesshistory_explain",
    "illnesshistory_flares",
    "illnesshistory_flaresinfxn",
    "illnesshistory_flaresinfxn_type",
    "illnesshistory_fluvaxx",
    "illnesshistory_infxn",
    "illnesshistory_infxnhistory_recover",
    "illnesshistory_infxnhistory_severe",
    "illnesshistory_infxnhistory_sick",
    "illnesshistory_lyme",
    "illnesshistory_severity",
    "illnesshistory_severity_change",
    "illnesshistory_status",
    "illnesshistory_strep",
    "illnesshistory_sx",
    "illnesshistory_sx_date",
    "illnesshistory_sx_total",
    "illnesshistory_treatment",
    "illnesshistory_trigger",
]

# --------------------------------------------------
# PATTERNS TO FLAG POSSIBLE PHI
# --------------------------------------------------
patterns = {
    # Direct identifiers
    "email": r"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b",
    "phone": r"\b(?:\+?1[-.\s]?)?(?:\(?\d{3}\)?[-.\s]?\d{3}[-.\s]?\d{4})\b",
    "ssn": r"\b\d{3}-\d{2}-\d{4}\b",

    # Full address only — removed plain ZIP because illness dates/counts can trigger noisy 5-digit matches
    "address": r"\b\d{1,6}\s+[A-Za-z0-9.\- ]+\s(?:Street|St|Avenue|Ave|Road|Rd|Boulevard|Blvd|Lane|Ln|Drive|Dr|Court|Ct|Way|Place|Pl)\b",

    # More specific date formats only
    "date_numeric": r"\b(?:0?[1-9]|1[0-2])[/-](?:0?[1-9]|[12]\d|3[01])[/-](?:\d{4})\b",

    # Stronger name phrases only
    "name_phrase": r"\b(?:my name is|patient name is|child's name is|childs name is)\s+[A-Z][a-z]+(?:\s+[A-Z][a-z]+)?\b",

    # Family member names only when there is an explicit relationship + likely name
    "family_name": r"\b(?:my|our)\s+(?:mom|mother|dad|father|sister|brother|son|daughter|husband|wife|partner|child)\s+(?:is\s+|was\s+|named\s+)?[A-Z][a-z]+\b",

    # Provider/person title only
    "title_name": r"\b(?:Dr|Mr|Mrs|Ms|Prof)\.?\s+[A-Z][a-z]+\b",
}
# --------------------------------------------------
# READ INPUT
# --------------------------------------------------
df = pd.read_excel(input_file, sheet_name="Illness History")

print("Loaded columns:")
print(df.columns.tolist())

# --------------------------------------------------
# CHECK FUNCTION
# --------------------------------------------------
def check_phi(text):
    if pd.isna(text):
        return []

    text = str(text)
    hits = []

    for label, pattern in patterns.items():
        if re.search(pattern, text, re.IGNORECASE):
            hits.append(label)

    return hits

# --------------------------------------------------
# RUN SCAN — PRINT ONLY, NO OUTPUT FILE
# --------------------------------------------------
total_flags = 0
missing_cols = [col for col in columns_to_check if col not in df.columns]

for idx, row in df.iterrows():
    for col in columns_to_check:
        if col in df.columns:
            hits = check_phi(row[col])

            if hits:
                total_flags += 1

                print("\n--- FLAG FOUND ---")
                print(f"Row: {idx + 2}")
                print(f"Participant: {row.get('participantidentifier', 'N/A')}")
                print(f"Column: {col}")
                print(f"Type(s): {', '.join(hits)}")
                print(f"Value: {row[col]}")

print("\n======================")
print(f"Total flagged cells: {total_flags}")
print("\nReview these flagged cells:")
print("======================")

if missing_cols:
    print("\nColumns not found in file:")
    for col in missing_cols:
        print(f"- {col}")