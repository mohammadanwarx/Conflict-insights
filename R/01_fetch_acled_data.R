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



#============ 
# fetch population data


#===================================
# R CODE: Fetch Sudan ADM2 via API
#===================================
# Run this section in R (not Python)

# Install required packages if needed
if (!require("geodata")) install.packages("geodata")
if (!require("sf")) install.packages("sf")
if (!require("ggplot2")) install.packages("ggplot2")

library(geodata)
library(sf)
library(ggplot2)

# Set working directory to project root
project_root <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(project_root)

# Create output directory
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

#--- Download Sudan ADM2 from GADM ---
cat("=== DOWNLOADING SUDAN ADM2 ===\n")
sudan_adm2 <- gadm(country = "SDN", level = 2, path = "data/raw")

# Convert to sf object for easier manipulation
sudan_adm2_sf <- st_as_sf(sudan_adm2)

cat("ADM2 downloaded successfully!\n")
cat("Number of ADM2 regions:", nrow(sudan_adm2_sf), "\n")
cat("Columns:", paste(names(sudan_adm2_sf), collapse = ", "), "\n")

# Display ADM2 names
cat("\n=== ADM2 NAMES ===\n")
adm2_info <- sudan_adm2_sf %>%
  st_drop_geometry() %>%
  select(NAME_1, NAME_2) %>%
  arrange(NAME_1, NAME_2)

print(adm2_info, n = 50)

#--- Visualize Sudan ADM2 with Labels ---
cat("\n=== VISUALIZING ADM2 ===\n")

# Create map with ADM2 boundaries and labels
sudan_adm2_map <- ggplot(sudan_adm2_sf) +
  geom_sf(aes(fill = NAME_1), color = "black", linewidth = 0.3, alpha = 0.6) +
  geom_sf_text(aes(label = NAME_2), size = 2, check_overlap = TRUE) +
  theme_minimal() +
  labs(
    title = "Sudan Administrative Divisions (ADM2)",
    subtitle = "Source: GADM Database",
    fill = "State (ADM1)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text = element_text(size = 8),
    legend.position = "right",
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 9)
  )

# Display the plot
print(sudan_adm2_map)

# Save the map
dir.create("output", showWarnings = FALSE)
ggsave("output/Sudan_ADM2_map.png", sudan_adm2_map, width = 14, height = 16, dpi = 300)
cat("Map saved to: output/Sudan_ADM2_map.png\n")

# Save ADM2 shapefile for later use
st_write(sudan_adm2_sf, "data/raw/sudan_adm2.gpkg", delete_layer = TRUE)
cat("ADM2 shapefile saved to: data/raw/sudan_adm2.gpkg\n")
