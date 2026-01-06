"""
Local configuration file for survey processing.
Fill in the values as needed.
"""

# Create a mapping dictionary
question_mapping = {
    "100-0": "thermal_acceptability",  # Please rate your acceptance of the current thermal environment
    "101-1": "thermal_sensation",  # How do you feel right now?
    "101-2": "thermal_preference",  # Right now, would you prefer to be...?
    "101-3": "thermal_comfort",  # Right now, do you find this environment...?
    "300-1": "thermal_sensation_ankles",  # Right now, around my ANKLES, I feel...?
    "300-2": "thermal_preference_ankles",  # Right now, around my ANKLES, I would prefer to be...?
    "300-3": "air_movement_acceptability_ankles",  # Please rate your acceptance of air movement at your ANKLES
    "300-4": "air_movement_preference_ankles",  # Right now, around my ANKLES, I would prefer...?
    "400-0": "iaq_preference",  # Please rate your acceptance of the current air quality in the room
    #! There was a problem for WS-2 in session 001
    # '400-1': 'preference_temperature',
    # '400-2': 'preference_air_movement',
    # '400-3': 'preference_humidity',
    # '400-4': 'preference_lighting',
    # '405-1': 'acceptability_temperature',
    # '405-2': 'acceptability_air_movement',
    # '405-3': 'acceptability_humidity',
    # '405-4': 'acceptability_lighting',
    # '500-0': 'satisfaction_overall',
    "500-1": "thermal_satisfaction",  # How satisfied are you with the thermal environment right now?
    "500-2": "thermal_pleasantness",  # Right now, do you find this thermal environment...?
    "701-0": "clothing_change",  # Did you adjust your upper body clothing?
    "700-0": "clothing_change_reason",  # Why did you change your clothing?
}
