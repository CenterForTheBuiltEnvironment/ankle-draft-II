import os

import pandas as pd

# import Qualtrics surveys
df_q1 = pd.read_csv("data/01_processed/qualtrics/survey_start.csv")
df_q2 = pd.read_csv("data/01_processed/qualtrics/survey_middle.csv")
df_q3 = pd.read_csv("data/01_processed/qualtrics/survey_end.csv")

q1_col_map = {
    "timestamp": "timestamp",
    "user_id": "subject_id",
    "transit": "transport_mode",
    "transit_5_TEXT": "transport_mode_other",
    "transit-time": "transport_time",
    "shower": "shower",
    "meal_1": "last_meal_time",
    "meal_2": "last_meal_food",
    "medication": "medication",
    "medication_2_TEXT": "medication_other",
    "sleep_1": "sleep_enter_bed",
    "sleep_2": "sleep_fall_asleep",
    "sleep_3": "sleep_wake_up",
    "sleep_4": "sleep_left_bed",
    "sleep_5": "sleep_disruption",
    "groningen_1": "groningen_1",
    "groningen_2": "groningen_2",
    "groningen_3": "groningen_3",
    "groningen_4": "groningen_4",
    "groningen_5": "groningen_5",
    "groningen_6": "groningen_6",
    "groningen_7": "groningen_7",
    "groningen_8": "groningen_8",
    "groningen_9": "groningen_9",
    "groningen_10": "groningen_10",
    "groningen_11": "groningen_11",
    "groningen_12": "groningen_12",
    "groningen_13": "groningen_13",
    "groningen_14": "groningen_14",
    "groningen_15": "groningen_15",
    "panas_1": "panas_1",
    "panas_2": "panas_2",
    "panas_3": "panas_3",
    "panas_4": "panas_4",
    "panas_5": "panas_5",
    "panas_6": "panas_6",
    "panas_7": "panas_7",
    "panas_8": "panas_8",
    "panas_9": "panas_9",
    "panas_10": "panas_10",
    "panas_11": "panas_11",
    "panas_12": "panas_12",
    "panas_13": "panas_13",
    "panas_14": "panas_14",
    "panas_15": "panas_15",
    "panas_16": "panas_16",
    "panas_17": "panas_17",
    "panas_18": "panas_18",
    "panas_19": "panas_19",
    "panas_20": "panas_20",
    "panas_21": "panas_21",
    "panas_22": "panas_22",
    "panas_23": "panas_23",
    "panas_24": "panas_24",
    "panas_25": "panas_25",
    "panas_26": "panas_26",
    "thermal-sensation": "thermal_sensation",
    "thermal-preference": "thermal_preference",
    "air-speed-preference": "air_speed_preference",
    "air-quality_1": "air_quality",
    "odor": "odor",
    "acceptability_1": "acceptability_thermal",
    "acceptability_2": "acceptability_air_speed",
    "acceptability_3": "acceptability_humidity",
    "acceptability_4": "acceptability_air_quality",
    "discomfort": "discomfort_cause",
    "discomfort_5_TEXT": "discomfort_cause_other",
    "body-irritation_1": "body_irritation_eyes",
    "body-irritation_2": "body_irritation_nose_throat",
    "body-irritation_3": "body_irritation_breathing_difficulty",
    "body-irritation_4": "body_irritation_skin",
    "body-irritation_5": "body_irritation_headache",
    "body-irritation_6": "body_irritation_dizziness",
    "body-irritation_7": "body_irritation_fatigue",
    "body-irritation_8": "body_irritation_sleepiness",
    "body-irritation_9": "body_irritation_concentration_difficulty",
    "other-symptoms": "symptoms_other",
    "clarity_1": "clarity",
    "concentration_1": "concentration",
    "feeling_1": "mood",
    "karolinska-kss": "karolinska_kss",
}

q2_col_map = {
    "timestamp": "timestamp",
    "user_id": "subject_id",
    "perform-self-assess_1": "task_perception_1",
    "perform-self-assess_2": "task_perception_2",
    "perform-self-assess_3": "task_perception_3",
    "perform-self-assess_4": "task_perception_4",
    "perform-self-assess_5": "task_perception_5",
    "thermal-sensation": "thermal_sensation",
    "thermal-preference": "thermal_preference",
    "air-speed-preference": "air_speed_preference",
    "air-quality_1": "air_quality",
    "odor": "odor",
    "acceptability_1": "acceptability_thermal",
    "acceptability_2": "acceptability_air_speed",
    "acceptability_3": "acceptability_humidity",
    "acceptability_4": "acceptability_air_quality",
    "discomfort": "discomfort_cause",
    "discomfort_5_TEXT": "discomfort_cause_other",
    "body-irritation_1": "body_irritation_eyes",
    "body-irritation_2": "body_irritation_nose_throat",
    "body-irritation_3": "body_irritation_breathing_difficulty",
    "body-irritation_4": "body_irritation_skin",
    "body-irritation_5": "body_irritation_headache",
    "body-irritation_6": "body_irritation_dizziness",
    "body-irritation_7": "body_irritation_fatigue",
    "body-irritation_8": "body_irritation_sleepiness",
    "body-irritation_9": "body_irritation_concentration_difficulty",
    "other-symptoms": "symptoms_other",
    "clarity_1": "thinking",
    "concentration_1": "concentration",
    "feeling_1": "mood",
    "karolinska-kss": "karolinska_kss",
}

q3_col_map = {
    "timestamp": "timestamp",
    "user_id": "subject_id",
    "perform-self-assess_1": "task_perception_1",
    "perform-self-assess_2": "task_perception_2",
    "perform-self-assess_3": "task_perception_3",
    "perform-self-assess_4": "task_perception_4",
    "perform-self-assess_5": "task_perception_5",
    "clarity_1": "thinking",
    "concentration_1": "concentration",
    "feeling_1": "mood",
    "panas_1": "panas_1",
    "panas_2": "panas_2",
    "panas_3": "panas_3",
    "panas_4": "panas_4",
    "panas_5": "panas_5",
    "panas_6": "panas_6",
    "panas_7": "panas_7",
    "panas_8": "panas_8",
    "panas_9": "panas_9",
    "panas_10": "panas_10",
    "panas_11": "panas_11",
    "panas_12": "panas_12",
    "panas_13": "panas_13",
    "panas_14": "panas_14",
    "panas_15": "panas_15",
    "panas_16": "panas_16",
    "panas_17": "panas_17",
    "panas_18": "panas_18",
    "panas_19": "panas_19",
    "panas_20": "panas_20",
    "panas_21": "panas_21",
    "panas_22": "panas_22",
    "panas_23": "panas_23",
    "panas_24": "panas_24",
    "panas_25": "panas_25",
    "panas_26": "panas_26",
    "thermal-sensation": "thermal_sensation",
    "thermal-preference": "thermal_preference",
    "air-speed-preference": "air_speed_preference",
    "air-quality_1": "air_quality",
    "odor": "odor",
    "acceptability_1": "acceptability_thermal",
    "acceptability_2": "acceptability_air_speed",
    "acceptability_3": "acceptability_humidity",
    "acceptability_4": "acceptability_air_quality",
    "discomfort": "discomfort_cause",
    "discomfort_5_TEXT": "discomfort_cause_other",
    "body-irritation_1": "body_irritation_eyes",
    "body-irritation_2": "body_irritation_nose_throat",
    "body-irritation_3": "body_irritation_breathing_difficulty",
    "body-irritation_4": "body_irritation_skin",
    "body-irritation_5": "body_irritation_headache",
    "body-irritation_6": "body_irritation_dizziness",
    "body-irritation_7": "body_irritation_fatigue",
    "body-irritation_8": "body_irritation_sleepiness",
    "body-irritation_9": "body_irritation_concentration_difficulty",
    "other-symptoms": "symptoms_other",
}

# map column names
df_q1.columns = df_q1.columns.map(q1_col_map)
df_q2.columns = df_q2.columns.map(q2_col_map)
df_q3.columns = df_q3.columns.map(q3_col_map)

# set index and add survey_id
df_q1 = df_q1.set_index(["timestamp"])
df_q1["survey_id"] = "start"

df_q2 = df_q2.set_index(["timestamp"])
df_q2["survey_id"] = "middle"

df_q3 = df_q3.set_index(["timestamp"])
df_q3["survey_id"] = "end"

df_q = pd.concat([df_q1, df_q2, df_q3])

# remove dash in subject_id and reorder columns
df_q["subject_id"] = df_q["subject_id"].astype(str).str.replace("-", "", regex=False)
id_cols = ["subject_id", "survey_id"]
remaining = [col for col in df_q.columns if col not in id_cols]
df_q = df_q[id_cols + remaining]

df_q.to_csv(
    os.path.join(os.getcwd(), "data/01_processed/qualtrics", "survey_combined.csv"),
)

df_q.to_csv(
    os.path.join(
        os.getcwd(), "data/01_processed/qualtrics", "survey_combined.csv.gzip"
    ),
    compression="gzip",
)
