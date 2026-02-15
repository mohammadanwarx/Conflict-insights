import pandas as pd
import requests

# Replace with your actual API key and email
api_key = '=='
email = '=='

# Base URL for the API
base_url = 'https://api.acleddata.com/acled/read/'

# Parameters for the request
params = {
    'key': api_key,
    'email': email,
    'country': 'Kenya',  # Filter for Kenya
    'start_date': '2015-01-01',  # Start date
    'end_date': '2020-12-31',  # End date
    'format': 'json'  # Request data in JSON format
}

# Make the GET request to fetch the data
response = requests.get(base_url, params=params)

# Check if the response is successful
if response.status_code == 200:
    try:
        # Load the JSON data from the response
        data_json = response.json()
        
        # Extract the actual data from the 'data' field in the JSON response
        data = pd.DataFrame(data_json['data'])

        # Convert 'event_date' to datetime format
        data['event_date'] = pd.to_datetime(data['event_date'], errors='coerce')

        # Filter the data to include only events from 2015 to 2020
        data_filtered = data[(data['event_date'] >= '2015-01-01') & (data['event_date'] <= '2020-12-31')]

        # Select only the relevant columns: event_date, event_type, latitude, longitude, and fatalities
        data_filtered = data_filtered[['event_date', 'event_type', 'latitude', 'longitude', 'fatalities']]

        # Export the filtered data to a CSV file
        data_filtered.to_csv('kenya_events_2015_2020.csv', index=False)

        # Confirm successful export
        print("Data successfully exported to 'kenya_events_2015_2020.csv'")

    except Exception as e:
        print(f"Error while processing the JSON content: {e}")
        print("Response content (first 500 characters):", response.text[:500])
else:
    print(f"Error: {response.status_code}, {response.text}")