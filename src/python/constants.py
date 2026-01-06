from enum import Enum


class MyConstants(str, Enum):
    """Class to hold constants for the project."""

    # Define constants here
    PROJECT_NAME = "Ankle Draft II"

    DIR_RAW_DATA_ENV = (
        r"/Users/toby/Desktop/Repositories/ankle-draft-II/data/00-raw/env"
    )
    DIR_RAW_DATA_SURVEY = (
        r"/Users/toby/Desktop/Repositories/ankle-draft-II/data/00-raw/survey"
    )
    DIR_RAW_DATA_AIRFLOW = (
        r"/Users/toby/Desktop/Repositories/ankle-draft-II/data/00-raw/air_flow"
    )
    DIR_RAW_DATA_TSK = (
        r"/Users/toby/Desktop/Repositories/ankle-draft-II/data/00-raw/physio"
    )
    DIR_RAW_DATA_META = (
        r"/Users/toby/Desktop/Repositories/ankle-draft-II/data/00-raw/metadata"
    )

    DIR_PROCESS_DATA_ENV = (
        r"/Users/toby/Desktop/Repositories/ankle-draft-II/data/01-processed/env"
    )
    DIR_PROCESS_DATA_SURVEY = (
        r"/Users/toby/Desktop/Repositories/ankle-draft-II/data/01-processed/survey"
    )
    DIR_PROCESS_DATA_AIRFLOW = (
        r"/Users/toby/Desktop/Repositories/ankle-draft-II/data/01-processed/air_flow"
    )
    DIR_PROCESS_DATA_TSK = (
        r"/Users/toby/Desktop/Repositories/ankle-draft-II/data/01-processed/tsk"
    )
    DIR_PROCESS_DATA_META = (
        r"/Users/toby/Desktop/Repositories/ankle-draft-II/data/01-processed/metadata"
    )

    DIR_PROCESS_DATA = (
        r"/Users/toby/Desktop/Repositories/ankle-draft-II/data/01-processed"
    )
    DIR_EXPORT_DATA = r"/Users/toby/Desktop/Repositories/ankle-draft-II/02-export"

    TIMEZONE = "America/Los_Angeles"
    COL_DATE_TIME = "timestamp"
    COL_USER_ID = "subject_id"
    COL_SESSION_ID = "session_id"
    COL_SENSING_LOCATION = "tsk_sensing_location"

    # Column names for IEQ data
    COL_ENV_T = "t_air_C"
    COL_ENV_RH = "rh_percent"
    COL_ENV_VA = "v_air_m_s"

    # Column names for IBUT data
    COL_TSK = "tsk_C"
