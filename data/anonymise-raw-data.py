import json
import uuid

def map_prolific_ids(file_path):
    # Load the existing data
    with open(file_path, 'r') as file:
        data = json.load(file)

    # Dictionary to map old PIDs to new UUIDs
    pid_to_new_id = {}

    # Traverse through the data and replace PIDs
    for key in data:
        entries = data[key]
        for entry_key, entry_value in entries.items():
            old_pid = entry_value.pop('PROLIFIC_PID', None)  # Remove the old key
            entry_value.pop('Referer', None)  # Remove 'Referer' entry because it contains prolific PID

            if old_pid:
                if old_pid not in pid_to_new_id:
                    # Generate a new UUID if this PID hasn't been encountered before
                    pid_to_new_id[old_pid] = str(uuid.uuid4())
                # Insert the new key with the UUID value
                entry_value['ANON_PID'] = pid_to_new_id[old_pid]

    # Save the updated data to a new file
    with open('data/raw/anon-results.json', 'w') as file:
        json.dump(data, file, indent=4)

# Path to your JSON file
file_path = 'data/raw/non-anonymised/results.json'
map_prolific_ids(file_path)
