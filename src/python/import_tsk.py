# Description: This script processes iButton temperature data files, extracts relevant information,
#              and saves the cleaned data into CSV files. It handles various date formats and ensures
#              proper timezone localization.

# ==================== IMPORTS AND SETUP ====================
import pandas as pd
import re
import logging
from pathlib import Path

# Import constants
try:
    from constants import MyConstants
except ImportError:
    from constants import MyConstants

# Set working directory
source_folder = Path(MyConstants.DIR_RAW_DATA_TSK.value)
save_folder = Path(MyConstants.DIR_PROCESS_DATA_TSK.value)


def add_tsk_prefix(folder_path: str, recursive: bool = False):
    """
    Adds 'tsk_' prefix to all files in the given folder.
    Skips files that already start with 'tsk_'.
    """
    folder = Path(folder_path)
    if not folder.exists():
        print(f"Folder not found: {folder}")
        return

    files = folder.rglob("*") if recursive else folder.glob("*")
    renamed = 0

    for f in files:
        if f.is_file():
            if not f.name.startswith("tsk_"):
                new_name = f.with_name(f"tsk_{f.name}")
                f.rename(new_name)
                print(f"Renamed: {f.name} → {new_name.name}")
                renamed += 1
            else:
                print(f"Skipped (already prefixed): {f.name}")

    print(f"\nDone. Renamed {renamed} file(s).")


# ==================== CLASS DEFINITION ====================
class IButtonProcessor:
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
        # Extract the session ID (e.g., "001")
        match = re.search(r"tsk_(\d+)_", filename)
        return match.group(1) if match else "unknown"

    def extract_subject_id(self, filename):
        match = re.search(r"tsk_\d+_\d+_(\w+)_", filename)
        return match.group(1) if match else "unknown"

    def extract_sensing_location(self, filename):
        # Extract the sensing location (e.g., "A")
        match = re.search(r"_([A-Z])(?:\.|_|$)", filename)
        return match.group(1) if match else "unknown"

    def process_file(self, file_path):
        try:
            session_id = self.extract_session_id(file_path.name)
            subject_id = self.extract_subject_id(file_path.name)
            tsk_sensing_location = self.extract_sensing_location(file_path.name)

            # Peek at the first line to decide how to read
            with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
                first_line = f.readline()

            df = pd.read_csv(
                file_path,
                skiprows=20,
                names=["Date", "Time", "Unit", "Value"],
                engine="python",
                encoding="utf-8",
                index_col=False,
            )

            df["timestamp"] = (
                df["Date"].astype(str).str.strip()
                + " "
                + df["Time"].astype(str).str.strip()
            )

            df["timestamp"] = (
                df["timestamp"]
                .str.replace("\u202f", " ", regex=False)  # narrow no-break space
                .str.replace("\u00a0", " ", regex=False)  # regular NBSP
                .str.replace(r"\s+", " ", regex=True)
                .str.strip()
            )

            df["timestamp"] = pd.to_datetime(
                df["timestamp"], format="%m/%d/%y %I:%M:%S %p", errors="coerce"
            )

            df["Value"] = pd.to_numeric(df["Value"], errors="coerce")

            df_result = pd.DataFrame(
                {
                    MyConstants.COL_USER_ID: subject_id,
                    MyConstants.COL_SESSION_ID: session_id,
                    MyConstants.COL_SENSING_LOCATION: tsk_sensing_location,
                    MyConstants.COL_DATE_TIME: df["timestamp"],
                    MyConstants.COL_TSK: df["Value"].round(2),
                }
            )

            return df_result.dropna(
                subset=[MyConstants.COL_DATE_TIME, MyConstants.COL_TSK]
            )

        except Exception as e:
            self.logger.exception(f"process_file failed for {file_path.name}: {e}")
            return pd.DataFrame()

    def save_processed_data(self, dataframe, filename_prefix="tsk_combined"):
        self.save_folder.mkdir(parents=True, exist_ok=True)
        output_csv = self.save_folder / f"{filename_prefix}.csv"
        output_csv_gz = self.save_folder / f"{filename_prefix}.csv.gzip"
        dataframe.to_csv(output_csv, index=False)
        dataframe.to_csv(output_csv_gz, index=False, compression="gzip")
        return output_csv, output_csv_gz

    def import_ibutton_files(self):
        if not self.source_folder.exists():
            return pd.DataFrame()

        files = list(self.source_folder.glob("**/*.csv"))
        tsk_files = [f for f in files if "tsk_" in f.name]

        if not tsk_files:
            return pd.DataFrame()

        dataframes = []
        for file_path in tsk_files:
            df = self.process_file(file_path)
            if not df.empty:
                dataframes.append(df)

        if not dataframes:
            return pd.DataFrame()

        df_combined = pd.concat(dataframes, ignore_index=True)
        df_final = df_combined.sort_values(
            [
                MyConstants.COL_USER_ID,
                MyConstants.COL_SENSING_LOCATION,
                MyConstants.COL_DATE_TIME,
            ]
        ).reset_index(drop=True)
        self.save_processed_data(df_final)
        return df_final


# ==================== INITIALIZE ====================
processor = IButtonProcessor()


# ==================== PROCESS FILES ====================
add_tsk_prefix(source_folder, recursive=False)

df_ibutton = processor.import_ibutton_files()


# ==================== SUMMARY ====================
if not df_ibutton.empty:
    print(f"Records: {len(df_ibutton)}")
    print(f"Users: {df_ibutton[MyConstants.COL_USER_ID].nunique()}")
    print(
        f"Temperature range: {df_ibutton[MyConstants.COL_TSK].min():.1f} - {df_ibutton[MyConstants.COL_TSK].max():.1f} °C"
    )
    print(
        f"Date range: {df_ibutton[MyConstants.COL_DATE_TIME].min()} to {df_ibutton[MyConstants.COL_DATE_TIME].max()}"
    )
else:
    print("No data processed")
