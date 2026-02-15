# Script 02: Process ACLED Data for Sudan
# Purpose: Load raw data, filter to Sudan only, remove zero fatalities
# Output: Processed data saved to data/processed/

library(tidyverse)
library(readxl)
library(lubridate)

# Load raw data from Excel file
load_acled_data <- function(file_path) {
  data <- read_excel(file_path)
  cat("Loaded", nrow(data), "records from", file_path, "\n")
  return(data)
}

# Process ACLED data - filter Sudan and remove zero fatalities
process_acled_data <- function(raw_data) {
  processed <- raw_data %>%
    # Convert to tibble if needed
    as_tibble() %>%
    # Filter to Sudan only (COUNTRY column is uppercase)
    filter(tolower(COUNTRY) == "sudan") %>%
    # Remove zero fatalities (keep only events with fatalities > 0)
    filter(FATALITIES > 0) %>%
    # Rename columns to lowercase for consistency
    rename(
      week = WEEK,
      region = REGION,
      country = COUNTRY,
      admin1 = ADMIN1,
      event_type = EVENT_TYPE,
      sub_event_type = SUB_EVENT_TYPE,
      events = EVENTS,
      fatalities = FATALITIES,
      population_exposure = POPULATION_EXPOSURE,
      disorder_type = DISORDER_TYPE,
      id = ID,
      latitude = CENTROID_LATITUDE,
      longitude = CENTROID_LONGITUDE
    ) %>%
    # Clean event types
    mutate(
      event_type = tolower(event_type)
    ) %>%
    # Remove duplicates
    distinct() %>%
    # Arrange by week
    arrange(week)
  
  cat("After filtering: Sudan =", nrow(processed), "events with fatalities > 0\n")
  return(processed)
}

# ===== MAIN EXECUTION =====

# Load raw data
raw_data <- load_acled_data("data/raw/Africa_aggregated_data_up_to-2026-01-31.xlsx")

# Process and filter
processed_data <- process_acled_data(raw_data)

# Save processed data
output_file <- "data/processed/Sudan_fatalities_filtered.csv"
write_csv(processed_data, output_file)
cat("Processed data saved to:", output_file, "\n")

# Display summary
cat("\n=== SUMMARY ===\n")
cat("Total events in Sudan with fatalities > 0:", nrow(processed_data), "\n")
cat("Week range:", min(processed_data$week), "to", max(processed_data$week), "\n")
cat("Event types:", n_distinct(processed_data$event_type), "unique types\n")
cat("Total fatalities:", sum(processed_data$fatalities, na.rm = TRUE), "\n")
