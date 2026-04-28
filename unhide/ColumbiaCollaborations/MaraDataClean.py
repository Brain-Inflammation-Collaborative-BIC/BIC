from pathlib import Path
import pandas as pd
import sys

print(sys.executable)

DATA_DIR = Path(
    "/Users/kelseyaguirre/Library/Mobile Documents/"
    "com~apple~CloudDocs/Documents/"
    "BrainInflammationCollaborative/Unhide/Mara_Columbia/" 
    "Shared with Mara"
)

# List Excel files
xlsx_files = list(DATA_DIR.glob("*.xlsx"))
print(xlsx_files)

file_path = DATA_DIR / "4_23_26_SurveyandBFQDatav3.xlsx"

# Load workbook
BFQ = pd.ExcelFile(file_path)

# Get sheet names
sheet_names = BFQ.sheet_names
print(sheet_names)

# Second tab = index 1 (your reference)
master_sheet = sheet_names[1]

# Read second tab
master_df = pd.read_excel(file_path, sheet_name=master_sheet)

# Get master IDs
master_ids = (
    master_df["participantidentifier"]
    .dropna()
    .astype(str)
    .str.strip()
    .unique()
)

master_ids_set = set(master_ids)
print(len(master_ids))

# Output file
output_path = DATA_DIR / "cleaned_file.xlsx"

with pd.ExcelWriter(output_path, engine="openpyxl") as writer:

    for sheet in sheet_names:
        df = pd.read_excel(file_path, sheet_name=sheet)

        # --- AGE CALCULATION (only for Basic Information) ---
        if sheet == "Basic Information" and "participant_date_of_birth" in df.columns:
            
            df["participant_date_of_birth"] = pd.to_datetime(
                df["participant_date_of_birth"], errors="coerce"
            )
            
            today = pd.Timestamp.today()

            df["currentage"] = (
                today.year - df["participant_date_of_birth"].dt.year
                - (
                    (today.month < df["participant_date_of_birth"].dt.month) |
                    (
                        (today.month == df["participant_date_of_birth"].dt.month) &
                        (today.day < df["participant_date_of_birth"].dt.day)
                    )
                )
            )

            # Move currentage next to DOB
            dob_index = df.columns.get_loc("participant_date_of_birth")
            df.insert(dob_index + 1, "currentage", df.pop("currentage"))

            # Drop DOB column
            df = df.drop(columns=["participant_date_of_birth"])

            print("Processed age and removed DOB in Basic Information")

        # --- ID FILTERING ---
        if "participantidentifier" in df.columns:
            col = "participantidentifier"
        elif "participant_id" in df.columns:
            col = "participant_id"
        else:
            print(f"Skipping {sheet}: no participant ID column")
            df.to_excel(writer, sheet_name=sheet, index=False)
            continue

        df[col] = df[col].astype(str).str.strip()

        original_len = len(df)
        df = df[df[col].isin(master_ids_set)]
        new_len = len(df)

        print(f"{sheet}: removed {original_len - new_len} rows")

        # Save cleaned sheet
        df.to_excel(writer, sheet_name=sheet, index=False)

print(f"\nCleaned file saved to: {output_path}")
