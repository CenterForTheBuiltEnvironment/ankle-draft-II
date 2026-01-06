import os
import pandas as pd

from constants import MyConstants

subject_metadata = pd.read_csv(
    os.path.join(os.getcwd(), MyConstants.DIR_RAW_DATA_META, "onboarding_survey.csv")
)

string_cols = subject_metadata.select_dtypes(include=["object"]).columns.tolist()

# Clean string columns
for col in string_cols:
    subject_metadata[col] = subject_metadata[col].astype(str).str.lower().str.strip()

subject_metadata.to_csv(
    os.path.join(
        os.getcwd(), MyConstants.DIR_PROCESS_DATA_META, "subject_metadata.csv"
    ),
    index=False,
)
