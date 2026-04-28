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
# COLUMNS TO CHECK: FAMILY MEDICAL HISTORY
# --------------------------------------------------
columns_to_check = [
    "fmh_mentalHealth",
    "fmh_add_ADHD",
    "fmh_anorexia",
    "fmh_arfid",
    "fmh_bipolar",
    "fmh_bulimia",
    "fmh_depression",
    "fmh_ocd",
    "fmh_ptsd",
    "fmh_schizophrenia",
    "fmh_otherAnxiety",
    "fmh_otherChildhood",
    "fmh_otherAdult",
    "fmh_healthHistory",
    "fmh_addisons",
    "fmh_cushings",
    "fmh_allergies",
    "fmh_ankylosing_spondylitis",
    "fmh_asthma",
    "fmh_autoimmune_encephalitis",
    "fmh_blood_anemia",
    "fmh_blood_hemochromatosis",
    "fmh_cancer",
    "fmh_celiac_disease",
    "fmh_chronic_lyme",
    "fmh_copd",
    "fmh_crohns_disease",
    "fmh_dementia_alzheimers",
    "fmh_dermatologic_skin_conditions",
    "fmh_diabetes_type_1",
    "fmh_diabetes_type_2",
    "fmh_dysautonomia",
    "fmh_eczema",
    "fmh_eds",
    "fmh_fibromyalgia",
    "fmh_frequent_or_chronic_strep_infections",
    "fmh_frequent_or_chronic_infections_other_than_strep25",
    "fmh_chf",
    "fmh_chd",
    "fmh_irregular_heartbeats",
    "fmh_heart_attack",
    "fmh_heart_mitral_valve_prolapse",
    "fmh_heart_conditions_other",
    "fmh_hypertension",
    "fmh_hypotension",
    "fmh_immune_deficiency",
    "fmh_ibs",
    "fmh_joint_hypermobility",
    "fmh_juvenile_rheumatoid_arthritis",
    "fmh_long_covid",
    "fmh_lupus",
    "fmh_chronic_kidney_disease",
    "fmh_marfan",
    "fmh_mcas",
    "fmh_migraines",
    "fmh_multiple_chemical_sensitivities",
    "fmh_ms",
    "fmh_myasthenia_gravis",
    "fmh_narcolepsy",
    "fmh_osetoarthritis",
    "fmh_pandas",
    "fmh_parkinsons",
    "fmh_polymyositis_inflammation",
    "fmh_pots",
    "fmh_psoriasis",
    "fmh_psoriatic_arthritis",
    "fmh_endometriosis",
    "fmh_rregular_periods",
    "fmh_rheumatic_scarlet_fever",
    "fmh_rheumatoid_arthritis",
    "fmh_sjogrens_syndrome",
    "fmh_sleep_apnea",
    "fmh_stroke_or_transient_ischemic_attack",
    "fmh_tmj",
    "fmh_hyperthyroidism",
    "fmh_hypothyroid",
    "fmh_tourettes",
    "fmh_ulcerative_colitis",
    "fmh_other_autoimmune_illness",
    "fmh_2nd_other_autoimmune_illness",
    "fmh_other_infection_associated_chronic_condition_or_illness",
    "fmh_other_endocrine_disorders_e_g_diabetes_other_hormonal_problems",
    "fmh_other_neurological_issue_not_included_above",
    "fmh_other_significant_illness",
    "fmh_2nd_other_significant_illness",
    "fmh_3rd_other_significant_illness",
]

# --------------------------------------------------
# PATTERNS TO FLAG POSSIBLE PHI
# --------------------------------------------------
patterns = {
    # --------------------------------------------------
    # DIRECT IDENTIFIERS
    # --------------------------------------------------
    "email": r"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b",
    "phone": r"\b(?:\+?1[-.\s]?)?(?:\(?\d{3}\)?[-.\s]?\d{3}[-.\s]?\d{4})\b",
    "ssn": r"\b\d{3}-\d{2}-\d{4}\b",
    "zip": r"\b\d{5}(?:-\d{4})?\b",

    # --------------------------------------------------
    # DATES (expanded slightly)
    # --------------------------------------------------
    "date_numeric": r"\b(?:0?[1-9]|1[0-2])[/-](?:0?[1-9]|[12]\d|3[01])[/-](?:\d{2}|\d{4})\b",
    "date_written": r"\b(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)[a-z]*\.?\s+\d{1,2}(?:,\s*\d{4})?\b",

    # --------------------------------------------------
    # ADDRESS (slightly more flexible)
    # --------------------------------------------------
    "address": r"\b\d{1,6}\s+[A-Za-z0-9.\- ]+\s(?:Street|St|Avenue|Ave|Road|Rd|Boulevard|Blvd|Lane|Ln|Drive|Dr|Court|Ct|Way|Place|Pl)\b",

    # --------------------------------------------------
    # NAME / SELF-IDENTIFICATION PHRASES (expanded)
    # --------------------------------------------------
    "name_phrase": r"\b(?:my name is|i am|i'm|this is|name's|it is)\s+[A-Z][a-z]+(?:\s+[A-Z][a-z]+)?\b",

    # --------------------------------------------------
    # FAMILY MEMBER + NAME (common survey leak)
    # --------------------------------------------------
    "family_name": r"\b(?:my|our)\s+(?:mom|mother|dad|father|sister|brother|son|daughter|husband|wife|partner)\s+[A-Z][a-z]+\b",

    # --------------------------------------------------
    # TITLES (Dr., Mr., etc.)
    # --------------------------------------------------
    "title_name": r"\b(?:Dr|Mr|Mrs|Ms|Prof)\.?\s+[A-Z][a-z]+\b",

    # --------------------------------------------------
    # HEALTHCARE / LOCATION REFERENCES
    # (not PHI alone, but strong signals)
    # --------------------------------------------------
    "facility": r"\b(?:hospital|clinic|medical center|urgent care|ER|emergency room)\b",

    # --------------------------------------------------
    # SIMPLE NAME DETECTION (controlled)
    # catches "John Smith" but avoids all-caps noise
    # --------------------------------------------------
    "possible_name": r"\b[A-Z][a-z]+\s+[A-Z][a-z]+\b",
}

# --------------------------------------------------
# READ INPUT
# --------------------------------------------------
df = pd.read_excel(input_file, sheet_name="Family Medical History")

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
print("======================")

if missing_cols:
    print("\nColumns not found in file:")
    for col in missing_cols:
        print(f"- {col}")


