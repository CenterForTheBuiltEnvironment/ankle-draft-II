# Description: Import air flow measurement data from TXT files, process, and save combined results.

# ==================== IMPORTS AND SETUP ====================
import pandas as pd
import numpy as np
import os
import re
import logging
from pathlib import Path


# Import constants
try:
    from constants import MyConstants
except ImportError:
    from constants import MyConstants

# Set working directory
source_folder = Path(MyConstants.DIR_RAW_DATA_AIRFLOW.value)
save_folder = Path(MyConstants.DIR_PROCESS_DATA_AIRFLOW.value)


# ==================== CLASS DEFINITION ====================
class AirFlowProcessor:
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

    def extract_date(self, filename):
        # Extract date (e.g., "111325" from "111325_dot1.txt")
        match = re.search(r"^(\d{6})_", filename)
        if match:
            date_str = match.group(1)
            # Convert from MMDDYY to datetime
            try:
                return pd.to_datetime(date_str, format="%m%d%y")
            except:
                return None
        return None

    def extract_point(self, filename):
        # Extract measurement point (e.g., "dot1" from "111325_dot1.txt")
        match = re.search(r"_(\w+)\.txt", filename)
        return match.group(1) if match else "unknown"

    def process_file(self, file_path):
        try:
            print(f"Processing file: {file_path.name}")

            # Extract metadata from filename
            measurement_date = self.extract_date(file_path.name)
            measurement_point = self.extract_point(file_path.name)

            if measurement_date is None:
                self.logger.warning(f"Could not extract date from {file_path.name}")
                return pd.DataFrame()

            # Read the header line (line 13, which is index 12)
            with open(file_path, "r", encoding="utf-8") as f:
                lines = f.readlines()
                header_line = lines[12].strip()

            # Split header by tab to preserve column names with spaces
            column_names = [
                col.strip() for col in header_line.split("\t") if col.strip()
            ]

            # Read the data starting from line 14 (skiprows=13)
            df = pd.read_csv(
                file_path,
                sep=r"\s+",
                skiprows=13,
                names=column_names,
                encoding="utf-8",
                engine="python",
                on_bad_lines="warn",
            )

            # Remove completely empty columns
            df = df.dropna(axis=1, how="all")
            df = df.loc[:, (df != "").any(axis=0)]

            # Remove rows that are completely empty
            df = df.dropna(how="all")

            # Add metadata columns
            df["measurement_date"] = measurement_date
            df["measurement_point"] = measurement_point

            # Create timestamp column
            df["datetime_string"] = (
                df["measurement_date"].dt.strftime("%Y-%m-%d")
                + " "
                + df["SN"].astype(str).str.strip()
                + " "
                + df["Time"].astype(str).str.strip()
            )
            # Convert datetime_string to datetime format
            df["timestamp"] = pd.to_datetime(
                df["datetime_string"], format="%Y-%m-%d %I:%M:%S %p"
            )
            df.drop(
                columns=["SN", "Time", "datetime_string", "measurement_date"],
                inplace=True,
            )

            # Move metadata columns to the front
            metadata_cols = ["timestamp", "measurement_point"]
            other_cols = [col for col in df.columns if col not in metadata_cols]
            df = df[metadata_cols + other_cols]

            # Rename columns to simplify
            df.rename(
                columns={
                    "Elapsing Time [s]": "time_elapsed_s",
                    "P-7:Va [m/s]": "low_v_air_s",
                    "P-9:Va [m/s]": "med_v_air_s",
                    "P-11:Va [m/s]": "high_v_air_s",
                    "P-7:ta [degC]": "low_t_air_C",
                    "P-9:ta [degC]": "med_t_air_C",
                    "P-11:ta [degC]": "high_t_air_C",
                    "P-7:SD [m/s]": "low_v_air_SD_m_s",
                    "P-9:SD [m/s]": "med_v_air_SD_m_s",
                    "P-11:SD [m/s]": "high_v_air_SD_m_s",
                    "P-7:Tu [%]": "low_turbulence_per_cent",
                    "P-9:Tu [%]": "med_turbulence_per_cent",
                    "P-11:Tu [%]": "high_turbulence_per_cent",
                    "P-7:DR [%]": "low_dynamic_range_per_cent",
                    "P-9:DR [%]": "med_dynamic_range_per_cent",
                    "P-11:DR [%]": "high_dynamic_range_per_cent",
                },
                inplace=True,
            )

            return df

        except Exception as e:
            self.logger.exception(f"process_file failed for {file_path.name}: {e}")
            return pd.DataFrame()

    def save_processed_data(self, dataframe, filename_prefix="airflow_combined"):
        self.save_folder.mkdir(parents=True, exist_ok=True)
        output_csv = self.save_folder / f"{filename_prefix}.csv"
        output_csv_gz = self.save_folder / f"{filename_prefix}.csv.gzip"
        dataframe.to_csv(output_csv, index=False)
        dataframe.to_csv(output_csv_gz, index=False, compression="gzip")
        self.logger.info(f"Saved to {output_csv} and {output_csv_gz}")
        return output_csv, output_csv_gz

    def import_airflow_files(self):
        if not self.source_folder.exists():
            print("CWD =", os.getcwd())
            print("SOURCE_FOLDER =", repr(source_folder))
            print("EXISTS? =", Path(source_folder).exists())
            self.logger.error("Source folder does not exist")
            return pd.DataFrame()

        print("Looking in:", self.source_folder.resolve())

        # Find all txt files matching the pattern
        files = list(self.source_folder.glob("*_*.txt"))
        print(f"Found {len(files)} air flow measurement files")

        processed_dataframes = []
        for file_path in files:
            df_processed = self.process_file(file_path)
            print(f"Processed {file_path.name}, records: {len(df_processed)}")
            if not df_processed.empty:
                processed_dataframes.append(df_processed)

        if not processed_dataframes:
            self.logger.warning("No valid air flow files found")
            return pd.DataFrame()

        # Combine all dataframes
        df_combined = pd.concat(processed_dataframes, ignore_index=True)

        # Sort by measurement_date and measurement_point
        df_final = df_combined.sort_values(
            ["timestamp", "measurement_point"]
        ).reset_index(drop=True)

        # Save the processed data
        self.save_processed_data(df_final)

        return df_final


# ==================== INITIALIZE ====================
processor = AirFlowProcessor()


# ==================== PROCESS FILES ====================
df_airflow = processor.import_airflow_files()


# ==================== SUMMARY ====================
if not df_airflow.empty:
    print(f"\n=== Summary ===")
    print(f"Total records: {len(df_airflow)}")
    print(f"Unique measurement points: {df_airflow['measurement_point'].nunique()}")
    print(
        f"Date range: {df_airflow['timestamp'].min()} to {df_airflow['timestamp'].max()}"
    )
    print(
        f"\nMeasurement points: {sorted(df_airflow['measurement_point'].unique().tolist())}"
    )
    print(f"\nColumn names: {df_airflow.columns.tolist()}")
else:
    print("No data processed")
