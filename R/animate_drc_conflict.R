# DRC (Democratic Republic of Congo) Conflict Animation
# Purpose: Create animated visualization of conflict fatalities over time
# Output: output/drc_conflict_animated.gif
#
# Data sources:
#   - data/raw/Africa_aggregated_data_up_to-2026-02-07.xlsx (ACLED conflict data)
#   - data/raw/drc_adm2.gpkg (DRC administrative boundaries - auto-downloaded)

#===================================
# Fix PROJ database conflict (PostgreSQL/PostGIS)
#===================================
Sys.unsetenv("PROJ_LIB")
Sys.unsetenv("PROJ_DATA")

#===================================
# Set Working Directory
#===================================
project_root <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  for (arg in args) {
    if (startsWith(arg, "--file=")) {
      script_path <- normalizePath(sub("--file=", "", arg))
      return(dirname(dirname(script_path)))
    }
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    return(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
  }
  getwd()
}, error = function(e) getwd())

setwd(project_root)
cat("Working directory:", getwd(), "\n")

#===================================
# Install packages if needed
#===================================
user_lib_path <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library", 
                            paste0(R.version$major, ".", R.version$minor))
if (!dir.exists(user_lib_path)) {
  dir.create(user_lib_path, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib_path, .libPaths()))

required_packages <- c("tidyverse", "lubridate", "sf", "gganimate", "gifski", "patchwork", "magick", "readxl", "geodata")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing:", pkg, "\n")
    install.packages(pkg, lib = user_lib_path, repos = "https://cloud.r-project.org", quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

library(tidyverse)
library(lubridate)
library(sf)
library(gganimate)
library(gifski)
library(patchwork)
library(magick)
library(readxl)
library(geodata)

#===================================
# Download DRC ADM2 boundaries if needed
#===================================
drc_gpkg_path <- "data/raw/drc_adm2.gpkg"

if (!file.exists(drc_gpkg_path)) {
  cat("Downloading DRC ADM2 boundaries...\n")
  drc_adm2_raw <- gadm(country = "COD", level = 2, path = "data/raw")
  drc_adm2 <- st_as_sf(drc_adm2_raw)
  st_write(drc_adm2, drc_gpkg_path, delete_dsn = TRUE)
  cat("Saved DRC boundaries to:", drc_gpkg_path, "\n")
} else {
  drc_adm2 <- st_read(drc_gpkg_path, quiet = TRUE)
}
cat("Loaded DRC ADM2 with", nrow(drc_adm2), "regions\n")

#===================================
# Load and process DRC data from Excel
#===================================
excel_path <- "data/raw/Africa_aggregated_data_up_to-2026-02-07.xlsx"

cat("Loading data from Excel...\n")
raw_data <- read_excel(excel_path)
cat("Total records in Excel:", nrow(raw_data), "\n")

# Rename columns to lowercase for consistency
names(raw_data) <- tolower(names(raw_data))
names(raw_data)[names(raw_data) == "centroid_latitude"] <- "latitude"
names(raw_data)[names(raw_data) == "centroid_longitude"] <- "longitude"

# Filter for DRC only
data <- raw_data %>%
  filter(country == "Democratic Republic of Congo") %>%
  mutate(week = as.Date(week)) %>%
  filter(week >= as.Date("2009-01-01")) %>%
  mutate(
    fatalities = as.numeric(fatalities),
    events = as.numeric(events),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  filter(fatalities > 0)

cat("DRC records (2009+, fatalities > 0):", nrow(data), "\n")

# Save processed data
write_csv(data, "data/processed/DRC_fatalities_filtered.csv")
cat("Saved processed data to: data/processed/DRC_fatalities_filtered.csv\n")

#===================================
# Prepare data for animation
#===================================
anim_data <- data %>%
  mutate(
    year_month = floor_date(week, "month"),
    date_label = format(year_month, "%B %Y")
  ) %>%
  filter(tolower(event_type) %in% c("battles", "violence against civilians", "explosions/remote violence")) %>%
  mutate(event_type = tolower(event_type)) %>%
  group_by(year_month, date_label, event_type, latitude, longitude) %>%
  summarise(
    total_fatalities = sum(fatalities, na.rm = TRUE),
    total_events = sum(events, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  arrange(year_month)

# Create ordered factor for chronological animation
date_order <- anim_data %>%
  distinct(year_month, date_label) %>%
  arrange(year_month) %>%
  pull(date_label)

anim_data <- anim_data %>%
  mutate(date_label = factor(date_label, levels = date_order))

cat("Prepared", nrow(anim_data), "aggregated records for animation\n")

# Prepare data for line chart - ensure all months have all event types
line_data_raw <- data %>%
  mutate(
    year_month = floor_date(week, "month"),
    date_label = format(year_month, "%B %Y")
  ) %>%
  filter(tolower(event_type) %in% c("battles", "violence against civilians", "explosions/remote violence")) %>%
  mutate(event_type = tolower(event_type)) %>%
  group_by(year_month, date_label, event_type) %>%
  summarise(
    total_fatalities = sum(fatalities, na.rm = TRUE),
    .groups = "drop"
  )

# Create complete grid of all months × all event types
all_months <- line_data_raw %>% distinct(year_month, date_label)
all_event_types <- tibble(event_type = c("battles", "violence against civilians", "explosions/remote violence"))
complete_grid <- crossing(all_months, all_event_types)

# Fill missing combinations with 0 fatalities
line_data <- complete_grid %>%
  left_join(line_data_raw, by = c("year_month", "date_label", "event_type")) %>%
  mutate(total_fatalities = replace_na(total_fatalities, 0)) %>%
  arrange(year_month) %>%
  mutate(date_label = factor(date_label, levels = date_order))

line_data_points <- line_data %>%
  mutate(state_label = date_label)

cat("Prepared", nrow(line_data), "records for line chart\n")

# Prepare cumulative fatalities data for line chart
cumulative_data <- line_data %>%
  group_by(event_type) %>%
  arrange(year_month) %>%
  mutate(cumulative_fatalities = cumsum(total_fatalities)) %>%
  ungroup() %>%
  mutate(state_label = date_label)

total_cumulative <- cumulative_data %>%
  group_by(year_month, date_label) %>%
  summarise(cumulative_fatalities = sum(cumulative_fatalities), .groups = "drop") %>%
  mutate(event_type = "Total",
         state_label = date_label)

counter_data <- bind_rows(cumulative_data, total_cumulative) %>%
  mutate(
    state_label = date_label,
    y_pos = case_when(
      event_type == "battles" ~ 4,
      event_type == "violence against civilians" ~ 3,
      event_type == "explosions/remote violence" ~ 2,
      event_type == "Total" ~ 1
    ),
    label_text = case_when(
      event_type == "battles" ~ "Battles",
      event_type == "violence against civilians" ~ "Violence Against Civilians",
      event_type == "explosions/remote violence" ~ "Explosions/Remote Violence",
      event_type == "Total" ~ "TOTAL FATALITIES"
    ),
    text_color = case_when(
      event_type == "battles" ~ "#E41A1C",
      event_type == "violence against civilians" ~ "#377EB8",
      event_type == "explosions/remote violence" ~ "#FF7F00",
      event_type == "Total" ~ "black"
    )
  )

cat("Prepared cumulative data for animated counters\n")

#===================================
# Define color palette
#===================================
event_colors <- c(
  "battles" = "#E41A1C",
  "violence against civilians" = "#377EB8",
  "explosions/remote violence" = "#FF7F00"
)

#===================================
# Create animated map (same style as Sudan)
#===================================
p_map <- ggplot() +
  # DRC ADM2 boundaries as background
  geom_sf(data = drc_adm2, fill = NA, color = "gray40", linewidth = 0.3) +
  # ADM2 labels 
  geom_sf_text(data = drc_adm2, aes(label = NAME_2), size = 2, color = "black", fontface = "bold", check_overlap = TRUE) +
  # Conflict points
  geom_point(
    data = anim_data,
    aes(x = longitude, y = latitude, 
        size = total_fatalities, 
        color = event_type),
    alpha = 0.4
  ) +
  scale_color_manual(
    values = event_colors, 
    name = "Event Type",
    guide = guide_legend(override.aes = list(size = 6, alpha = 0.8))
  ) +
  scale_size_continuous(
    range = c(3, 18),
    name = "Fatalities",
    labels = scales::comma,
    guide = guide_legend(override.aes = list(fill = NA, color = "black", stroke = 1.5, alpha = 1, shape = 21))
  ) +
  labs(
    title = "DRC Conflict Fatalities",
    subtitle = "{closest_state}",
    caption = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5, color = "darkblue", margin = margin(b = 2)),
    plot.subtitle = element_text(size = 20, hjust = 1, color = "darkred", face = "bold", margin = margin(t = -5, b = 5)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.key.size = unit(1.2, "lines"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(5, 5, 0, 5)
  ) +
  # DRC coordinate bounds
  coord_sf(xlim = c(11.5, 32), ylim = c(-14, 6), expand = FALSE) +
  transition_states(
    date_label,
    transition_length = 1,
    state_length = 2
  ) +
  enter_fade() +
  exit_fade()

#===================================
# Create animated line chart (cumulative fatalities)
#===================================
p_line <- ggplot(cumulative_data, aes(x = year_month, y = cumulative_fatalities, color = event_type, group = event_type)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 2.5) +
  scale_color_manual(values = event_colors, name = "Event Type") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
  labs(
    title = "Cumulative Fatalities by Event Type",
    x = NULL,
    y = "Cumulative Fatalities",
    caption = "Data Source: ACLED | Author: Mo Anwar"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5, margin = margin(t = 0, b = 2)),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 9),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 5, 0, 5)
  ) +
  transition_reveal(year_month)

#===================================
# Create counter frames
#===================================
counter_plot_data <- counter_data %>%
  mutate(
    value_label = as.character(scales::comma(cumulative_fatalities)),
    label_text = as.character(label_text),
    text_color = as.character(text_color)
  )

create_counter_frame <- function(state) {
  frame_data <- counter_plot_data %>% filter(state_label == state)
  
  p <- ggplot(frame_data, aes(y = y_pos)) +
    geom_text(aes(x = 0, label = label_text, color = text_color),
              hjust = 0, size = 2.8, fontface = "bold") +
    geom_text(aes(x = 1, label = value_label, color = text_color),
              hjust = 1, size = 3.8, fontface = "bold") +
    scale_color_identity() +
    scale_x_continuous(limits = c(-0.05, 1.05)) +
    scale_y_continuous(limits = c(0.5, 4.5)) +
    labs(title = "Cumulative Fatalities") +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5, margin = margin(t = 0, b = 5)),
      plot.margin = margin(0, 3, 0, 3),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  return(p)
}

#===================================
# Render animations
#===================================
cat("Rendering map animation...\n")

# Calculate number of states for frame allocation
n_states <- length(unique(anim_data$date_label))
cat("Number of monthly time states:", n_states, "\n")

# Use 2 frames per state for current speed
n_anim_frames <- n_states * 2

anim_map <- gganimate::animate(
  p_map,
  nframes = n_anim_frames,
  fps = 8,
  width = 900,
  height = 750,
  renderer = gifski_renderer()
)

cat("Rendering line chart animation...\n")
anim_line <- gganimate::animate(
  p_line,
  nframes = n_anim_frames,
  fps = 8,
  width = 650,
  height = 250,
  renderer = gifski_renderer()
)

# Create counter frames
cat("Creating counter frames...\n")
temp_dir <- tempdir()
states <- levels(counter_plot_data$state_label)

map_gif <- image_read(anim_map)
actual_map_frames <- length(map_gif)
cat("Map has", actual_map_frames, "frames for", n_states, "states\n")

frames_per_state <- actual_map_frames / n_states

counter_frames <- list()
for (i in seq_along(states)) {
  p <- create_counter_frame(states[i])
  temp_file <- file.path(temp_dir, sprintf("counter_%04d.png", i))
  ggsave(temp_file, p, width = 250/96, height = 250/96, dpi = 96, bg = "white")
  img <- image_read(temp_file)
  
  start_frame <- round((i - 1) * frames_per_state) + 1
  end_frame <- round(i * frames_per_state)
  n_frames_for_state <- end_frame - start_frame + 1
  
  for (j in 1:n_frames_for_state) {
    counter_frames[[length(counter_frames) + 1]] <- img
  }
}

counter_gif <- do.call(c, counter_frames)
cat("Counter frames generated:", length(counter_gif), "\n")

#===================================
# Combine animations
#===================================
cat("Combining animations...\n")
line_gif <- image_read(anim_line)

cat("Map frames:", length(map_gif), "\n")
cat("Line frames:", length(line_gif), "\n")
cat("Counter frames:", length(counter_gif), "\n")

n_frames <- min(length(map_gif), length(line_gif), length(counter_gif))

# Combine: map on top, line chart and counters side by side below
combined_frames <- lapply(1:n_frames, function(i) {
  bottom_row <- image_append(c(line_gif[i], counter_gif[i]), stack = FALSE)
  image_append(c(map_gif[i], bottom_row), stack = TRUE)
})
combined_gif <- do.call(c, combined_frames)

# Save combined animation
output_path <- "output/drc_conflict_animated.gif"
image_write(combined_gif, path = output_path)
cat("Animation saved to:", output_path, "\n")
