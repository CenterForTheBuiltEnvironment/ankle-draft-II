# Description: Import survey data from TXT files, process, and save combined results.

# ==================== IMPORTS AND SETUP ====================
import pandas as pd
import numpy as np
import re
import logging
from pathlib import Path

# Import constants
from constants import MyConstants
from survey_config import question_mapping

# Set working directory
source_folder = Path(MyConstants.DIR_RAW_DATA_SURVEY.value)
save_folder = Path(MyConstants.DIR_PROCESS_DATA_SURVEY.value)


# ==================== CLASS DEFINITION ====================
class SurveyProcessor:
    def __init__(
        self, source_folder_path=None, save_folder_path=None, log_level=logging.INFO
    ):
        self.source_folder = (
            source_folder_path if source_folder_path is not None else source_folder
        )
        self.save_folder = (
            save_folder_path if save_folder_path is not None else save_folder
        )
        self._setup_logging(log_level)

    def _setup_logging(self, log_level):
        for handler in logging.root.handlers[:]:
            logging.root.removeHandler(handler)
        logging.basicConfig(
            level=log_level, format="%(asctime)s - %(levelname)s - %(message)s"
        )
        self.logger = logging.getLogger(self.__class__.__name__)

    def extract_session_id(self, filename):
        # Extract session ID (e.g., "001")
        match = re.search(r"^(\d+)_", filename)
        return match.group(1) if match else "unknown"

    def extract_session_date(self, filename):
        # Extract session date (e.g., "11132025")
        match = re.search(r"^\d+_(\d{8})_", filename)
        if match:
            date_str = match.group(1)
            # Convert from MMDDYYYY to datetime
            try:
                return pd.to_datetime(date_str, format="%m%d%Y")
            except:
                return None
        return None

    def extract_workstation(self, filename):
        # Extract workstation (e.g., "adaptation")
        match = re.search(r"^\d+_\d+_(\w+)", filename)
        return match.group(1) if match else "unknown"

    def process_file(self, file_path):
        try:
            print(f"Processing file: {file_path.name}")

            # Extract metadata from filename
            session_id = self.extract_session_id(file_path.name)
            session_date = self.extract_session_date(file_path.name)
            workstation = self.extract_workstation(file_path.name)

            if session_date is None:
                self.logger.warning(f"Could not extract date from {file_path.name}")
                return pd.DataFrame()

            # Read the tab-separated file with subject_no as string
            df = pd.read_csv(
                file_path,
                sep="\t",
                header=None,
                names=[
                    "timestamp",
                    "subject_no",
                    "subject_initials",
                    "question_cat",
                    "question_num",
                    "response_value",
                ],
                dtype={"subject_no": str},
                encoding="utf-8",
                engine="python",
            )

            # Convert timestamp to datetime
            df["timestamp"] = pd.to_datetime(df["timestamp"], errors="coerce")

            # Pad subject_no to 3 digits with leading zeros
            df["subject_no"] = df["subject_no"].astype(str).str.zfill(3)

            # Create subject_id by concatenating subject_initials and subject_no
            df[MyConstants.COL_USER_ID] = df["subject_initials"].astype(str) + df[
                "subject_no"
            ].astype(str)

            # Create question column by concatenating question_cat and question_num
            df["question"] = (
                df["question_cat"].astype(str) + "-" + df["question_num"].astype(str)
            )

            # Determine if response is open text (non-numeric)
            df["is_open_text"] = pd.to_numeric(
                df["response_value"], errors="coerce"
            ).isna()

            # Round numeric responses to 1 decimal place
            numeric_mask = ~df["is_open_text"]
            df.loc[numeric_mask, "response_value"] = pd.to_numeric(
                df.loc[numeric_mask, "response_value"], errors="coerce"
            ).round(1)

            # Add metadata columns
            df["session_id"] = session_id
            df["session_date"] = session_date
            df["workstation"] = workstation

            # Select and reorder columns
            df_result = df[
                [
                    "session_id",
                    "session_date",
                    "workstation",
                    MyConstants.COL_USER_ID,
                    "timestamp",
                    "question",
                    "response_value",
                    "is_open_text",
                ]
            ].copy()

            # Drop rows with invalid timestamps
            df_result = df_result.dropna(subset=["timestamp"])
            df_result["question"] = df_result["question"].map(question_mapping)

            return df_result

        except Exception as e:
            self.logger.exception(f"process_file failed for {file_path.name}: {e}")
            return pd.DataFrame()

    def save_processed_data(self, dataframe, filename_prefix="survey_combined"):
        self.save_folder.mkdir(parents=True, exist_ok=True)
        output_csv = self.save_folder / f"{filename_prefix}.csv"
        output_csv_gz = self.save_folder / f"{filename_prefix}.csv.gzip"
        dataframe.to_csv(output_csv, index=False)
        dataframe.to_csv(output_csv_gz, index=False, compression="gzip")
        self.logger.info(f"Saved to {output_csv} and {output_csv_gz}")
        return output_csv, output_csv_gz

    def import_survey_files(self):
        if not self.source_folder.exists():
            self.logger.error("Source folder does not exist")
            return pd.DataFrame()

        print("Looking in:", self.source_folder.resolve())

        # Find all txt files matching the pattern
        files = list(self.source_folder.glob("*_*_*.txt"))
        print(f"Found {len(files)} survey files")

        processed_dataframes = []
        for file_path in files:
            df_processed = self.process_file(file_path)
            print(f"Processed {file_path.name}, records: {len(df_processed)}")
            if not df_processed.empty:
                processed_dataframes.append(df_processed)

        if not processed_dataframes:
            self.logger.warning("No valid survey files found")
            return pd.DataFrame()

        # Combine all dataframes
        df_combined = pd.concat(processed_dataframes, ignore_index=True)

        # Sort by session_id, user_id, and timestamp
        df_final = df_combined.sort_values(
            ["session_id", MyConstants.COL_USER_ID, "timestamp"]
        ).reset_index(drop=True)

        # Save the processed data
        self.save_processed_data(df_final)

        return df_final


# ==================== INITIALIZE ====================
processor = SurveyProcessor()


# ==================== PROCESS FILES ====================
df_survey = processor.import_survey_files()


# ==================== SUMMARY ====================
if not df_survey.empty:
    print(f"\n=== Summary ===")
    print(f"Total records: {len(df_survey)}")
    print(f"Unique subjects: {df_survey[MyConstants.COL_USER_ID].nunique()}")
    print(f"Unique sessions: {df_survey['session_id'].nunique()}")
    print(f"Unique workstations: {df_survey['workstation'].nunique()}")
    print(f"Unique questions: {df_survey['question'].nunique()}")
    print(
        f"Date range: {df_survey['timestamp'].min()} to {df_survey['timestamp'].max()}"
    )
    print(f"\nWorkstations: {df_survey['workstation'].unique().tolist()}")
else:
    print("No data processed")
