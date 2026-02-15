# Project Structure

## Directory Overview

```
Conflict-insights/
├── data/
│   ├── raw/              # Raw ACLED data (downloaded)
│   └── processed/        # Cleaned and processed data
├── R/                    # R scripts
│   ├── 01_fetch_acled_data.R    # API calls and data collection
│   ├── 02_process_data.R        # Data cleaning and transformation
│   └── 03_visualize_data.R      # Visualization functions
├── output/               # Generated plots and results
├── docs/                 # Documentation and notes
├── main.R                # Main workflow script
├── requirements.txt      # R package dependencies
├── README.md             # Project overview
├── LICENSE               # Project license
└── .gitignore            # Git ignore patterns
```

## Workflow

1. **Fetch Data** (`01_fetch_acled_data.R`)
   - Query ACLED API
   - Download raw conflict data
   - Save to `data/raw/`

2. **Process Data** (`02_process_data.R`)
   - Clean and validate data
   - Parse dates and formats
   - Handle missing values
   - Save to `data/processed/`

3. **Visualize Data** (`03_visualize_data.R`)
   - Time series plots
   - Event type distributions
   - Geographic maps
   - Save to `output/`

## Quick Start

1. Install dependencies:
   ```r
   install.packages(c("tidyverse", "ggplot2", "httr", "jsonlite", "leaflet"))
   ```

2. Run the main script:
   ```r
   source("main.R")
   ```

## Notes

- ACLED API may require authentication - check their documentation
- Raw data files should be stored in `data/raw/` (add to .gitignore)
- Generated outputs are saved automatically to `output/`
