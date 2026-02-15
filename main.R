# Main Analysis Script
# This script orchestrates the entire workflow: fetch -> process -> visualize

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Source scripts
source("R/01_fetch_acled_data.R")
source("R/02_process_data.R")
source("R/03_visualize_data.R")

# ===== WORKFLOW =====

# 1. Fetch data (optional - API may require authentication)
# acled_raw <- fetch_acled_data(limit = 5000, year = 2024)
# saveRDS(acled_raw, "data/raw/acled_data.rds")

# 2. Load and process data
acled_raw <- readRDS("data/raw/acled_data.rds")
acled_processed <- process_acled_data(acled_raw)
write_csv(acled_processed, "data/processed/acled_processed.csv")

# 3. Create visualizations
plot_events_over_time(acled_processed)
plot_event_types(acled_processed, top_n = 10)

cat("Analysis complete! Check output/ folder for results.\n")
