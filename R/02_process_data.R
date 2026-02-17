# Script 02: Process ACLED Data for Sudan
# Purpose: Load raw data, filter to Sudan only, remove zero fatalities
# Output: Processed data saved to data/processed/

#===================================
# Set Working Directory
#===================================
# Set working directory to project root
# Try to detect if running in RStudio first
project_root <- tryCatch({
  if (require("rstudioapi", quietly = TRUE)) {
    script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
    dirname(script_dir)
  } else {
    # Fallback: assume current directory is the project root
    getwd()
  }
}, error = function(e) {
  # If anything goes wrong, use current working directory
  getwd()
})

setwd(project_root)
cat("Working directory set to:", getwd(), "\n\n")

#===================================
# Install Required Libraries
#===================================

# Set up user library if it doesn't exist
user_lib_path <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library", 
                            paste0(R.version$major, ".", R.version$minor))
if (!dir.exists(user_lib_path)) {
  dir.create(user_lib_path, recursive = TRUE, showWarnings = FALSE)
  cat(sprintf("Created user library at: %s\n", user_lib_path))
}

# Add user library to search path
.libPaths(c(user_lib_path, .libPaths()))
cat(sprintf("Using library path: %s\n\n", user_lib_path))

# Function to install packages if not already installed
install_if_needed <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      cat(sprintf("Installing: %s...\n", pkg))
      install.packages(pkg, lib = user_lib_path, dependencies = TRUE, quiet = TRUE)
      library(pkg, character.only = TRUE)
      cat(sprintf("✓ Installed and loaded: %s\n", pkg))
    } else {
      cat(sprintf("✓ Package already loaded: %s\n", pkg))
    }
  }
}

# List of required packages
required_packages <- c(
  "tidyverse",
  "readxl",
  "lubridate",
  "sf",
  "ggplot2"
)

# Install and load all required packages
cat("=== Installing Required Libraries ===\n")
install_if_needed(required_packages)

cat("\n=== All libraries loaded successfully! ===\n\n")

# Individual library calls for explicit use
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(ggplot2)

#===================================
# Create Required Output Directories
#===================================
required_dirs <- c("output", "data/processed", "data/raw")
for (dir in required_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    cat(sprintf("✓ Created directory: %s\n", dir))
  }
}
cat("\n")

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


#===================================
# Dealing with Shapefile - Load, Visualize, and Display ADM1 Names
#===================================

# Load shapefile from zip
# Use absolute path to avoid issues with relative paths
shapefile_path <- file.path(project_root, "data/raw/SudanADM1.zip")
cat("\n=== LOADING SHAPEFILE ===\n")
cat("Looking for shapefile at:", shapefile_path, "\n")

if (!file.exists(shapefile_path)) {
  stop(sprintf("Shapefile not found at: %s", shapefile_path))
}

# Extract zip to a specific directory
extract_dir <- file.path(project_root, "data/raw/SudanADM1_extracted")
if (dir.exists(extract_dir)) {
  unlink(extract_dir, recursive = TRUE)  # Remove old extraction
}
dir.create(extract_dir, showWarnings = FALSE, recursive = TRUE)

# Use utils::unzip to extract
result <- utils::unzip(shapefile_path, exdir = extract_dir)
cat("Zip file extracted successfully\n")
cat("Extracted files:", length(result), "files\n")

# Find the .shp file
shp_files <- list.files(extract_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)

if(length(shp_files) == 0) {
  # Debug: show what files were actually extracted
  cat("\nDEBUG: Files extracted from zip:\n")
  all_files <- list.files(extract_dir, full.names = TRUE, recursive = TRUE)
  print(all_files)
  stop("No .shp file found in the zip archive")
}

# Read the first shapefile found
shapefile_to_read <- shp_files[1]
cat("Reading shapefile:", shapefile_to_read, "\n")
sudan_shapefile <- st_read(shapefile_to_read, quiet = TRUE)

cat("\n=== SHAPEFILE INFO ===\n")
cat("Shapefile loaded successfully\n")
cat("CRS:", st_crs(sudan_shapefile)$input, "\n")
cat("Number of features:", nrow(sudan_shapefile), "\n")
cat("\nColumn names:", paste(names(sudan_shapefile), collapse = ", "), "\n")

# Display ADM1 names
cat("\n=== ADM1 NAMES ===\n")

# Find columns containing "ADM1" or "admin1" (case-insensitive)
adm1_cols <- names(sudan_shapefile)[grepl("ADM1|admin1", names(sudan_shapefile), ignore.case = TRUE)]

if(length(adm1_cols) > 0) {
  cat("ADM1 column found:", adm1_cols[1], "\n")
  adm1_names <- sudan_shapefile %>%
    st_drop_geometry() %>%
    pull(adm1_cols[1]) %>%
    unique() %>%
    sort()
  
  cat("ADM1 Regions:\n")
  print(adm1_names)
} else {
  cat("No ADM1 column found. Available columns:\n")
  print(names(sudan_shapefile))
}

# Visualize the shapefile
cat("\n=== VISUALIZING SHAPEFILE ===\n")
plot_shapefile <- ggplot(sudan_shapefile) +
  geom_sf(aes(fill = NULL), color = "black", alpha = 0.7) +
  geom_sf_text(aes(label = adm1_name), size = 3, fontface = "bold", check_overlap = TRUE) +
  theme_minimal() +
  labs(
    title = "Sudan Administrative Divisions (ADM1)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 9),
    legend.position = "none"
  )

# Display the plot
print(plot_shapefile)

# Save the plot
ggsave("output/Sudan_ADM1_map.png", plot_shapefile, width = 10, height = 12, dpi = 300, create.dir = TRUE)
cat("Map saved to: output/Sudan_ADM1_map.png\n") 

