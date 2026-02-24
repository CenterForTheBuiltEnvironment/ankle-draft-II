import os
import glob
import pandas as pd

from constants import MyConstants


# Path to folder containing the CSV files
DATA_FOLDER = MyConstants.DIR_RAW_DATA_ENV


column_mapping = {
    "dtm": "timestamp",
    "co2, ppm": "co2_ppm",
    "pm1, μg/m³": "pm1_ug_m3",
    "pm4, μg/m³": "pm4_ug_m3",
    "pm10, μg/m³": "pm10_ug_m3",
    "pm25, μg/m³": "pm25_ug_m3",
    "voc, ppm": "voc_ppm",
    "ch2o, ppm": "ch2o_ppm",
    "noise, db": "noise_db",
    "light, lux": "light_lux",
    "temperature, °C": MyConstants.COL_ENV_T,
    "humidity, %": MyConstants.COL_ENV_RH,
    "abs_humidity, g/m³": "abs_humidity_g_m3",
    "pressure, mbar": "pressure_mbar",
    "vocindex": "voc_index",
    "noxindex": "nox_index",
    "avti": "avt_index",
    "eiaqi": "eiaq_index",
    "tci": "tc_index",
    "iaqi": "iaq_index",
}


def load_and_combine_data(data_folder: str) -> pd.DataFrame:
    """Load all CSV files, tag by monitor, and combine into one dataframe."""
    all_files = glob.glob(os.path.join(data_folder, "*.csv"))

    if not all_files:
        raise FileNotFoundError(f"No CSV files found in {data_folder}")

    dataframes = []
    tz_name = "America/Los_Angeles"

    for file_path in all_files:
        # Detect monitor name
        if "atmocube-001" in file_path:
            monitor = "atmocube-001"
            convert_temp = False
        elif "atmocube-002" in file_path:
            monitor = "atmocube-002"
            convert_temp = False
        elif "chamber-sensor" in file_path:
            monitor = "chamber-sensor"
            convert_temp = True
        else:
            monitor = "unknown"

        df = pd.read_csv(file_path)

        if convert_temp:
            df[MyConstants.COL_ENV_T] = ((df["t_air_f"] - 32) * 5 / 9).round(
                2
            )  # Convert F to C

        df.rename(columns=column_mapping, inplace=True)

        if "timestamp" not in df.columns:
            raise ValueError(f"'timestamp' column not found in {file_path}")

        ts = pd.to_datetime(df["timestamp"], errors="coerce", format="mixed", utc=False)

        # figure out which ones are tz-aware vs tz-naive
        def _localize_if_naive(x):
            if pd.isna(x):
                return x
            if x.tzinfo is None:
                return x.tz_localize(tz_name)
            return x

        ts = ts.map(_localize_if_naive)

        ts = ts.dt.tz_convert(tz_name)

        df["timestamp"] = ts
        df.set_index("timestamp", inplace=True)

        # add monitor identifier
        df["monitor"] = monitor

        dataframes.append(df)

    # Combine everything and sort by datetime index
    combined_df = pd.concat(dataframes, axis=0)
    combined_df.sort_index(inplace=True)

    return combined_df


def merge_monitors(df: pd.DataFrame, resample_rule: str = "1min") -> pd.DataFrame:
    """
    Combine all monitors' measurements by timestamp (averaging).
    Optionally resample to a uniform interval.
    """

    df_numeric = df.drop(columns=["monitor"])

    df_resampled = df_numeric.resample(resample_rule).mean()

    return df_resampled


if __name__ == "__main__":
    combined_data = load_and_combine_data(DATA_FOLDER)

    combined_data.to_csv("/Users/lyujunmeng/Documents/ankle-draft-II-main/data/00-raw/env/combined_env_data_raw.csv")

    # --- Optional: combine readings from all monitors ---
    merge_option = True

    if merge_option:
        merged_data = merge_monitors(combined_data, resample_rule="1min")
        merged_data.to_csv("/Users/lyujunmeng/Documents/ankle-draft-II-main/data/01-processed/env/df_env.csv")
        merged_data.to_csv("/Users/lyujunmeng/Documents/ankle-draft-II-main/data/01-processed/env/df_env.csv.gzip", compression="gzip")
