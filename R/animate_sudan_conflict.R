# Sudan Conflict Animation
# Purpose: Create animated visualization of conflict fatalities over time
# Output: output/sudan_conflict_animated.gif
#
# Data sources:
#   - data/processed/Sudan_fatalities_filtered.csv (ACLED conflict data)
#   - data/raw/sudan_adm2.gpkg (Sudan administrative boundaries)

#===================================
# Set Working Directory
#===================================
# Auto-detect project root from script location
project_root <- tryCatch({
  # When run via Rscript
  args <- commandArgs(trailingOnly = FALSE)
  for (arg in args) {
    if (startsWith(arg, "--file=")) {
      script_path <- normalizePath(sub("--file=", "", arg))
      return(dirname(dirname(script_path)))
    }
  }
  # When run in RStudio
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    return(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
  }
  # Fallback: assume current directory is project root
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

required_packages <- c("tidyverse", "lubridate", "sf", "gganimate", "gifski", "patchwork", "magick")
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

# Load processed Sudan data
data <- read_csv("data/processed/Sudan_fatalities_filtered.csv", show_col_types = FALSE)
cat("Loaded", nrow(data), "records\n")

# Load Sudan ADM2 boundaries from our downloaded shapefile
sudan_adm2 <- st_read("data/raw/sudan_adm2.gpkg", quiet = TRUE)
cat("Loaded Sudan ADM2 with", nrow(sudan_adm2), "regions\n")

# Prepare data for animation - aggregate by month and event type
anim_data <- data %>%
  mutate(
    year_month = floor_date(week, "month"),
    date_label = format(year_month, "%B %Y")
  ) %>%
  # Filter to only show data after January 2023
  filter(year_month >= as.Date("2023-01-01")) %>%
  # Filter to only show major event types
  filter(event_type %in% c("battles", "violence against civilians", "explosions/remote violence")) %>%
  group_by(year_month, date_label, event_type, latitude, longitude) %>%
  summarise(
    total_fatalities = sum(fatalities, na.rm = TRUE),
    total_events = sum(events, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  arrange(year_month)

# Create ordered factor for chronological animation (oldest to newest)
date_order <- anim_data %>%
  distinct(year_month, date_label) %>%
  arrange(year_month) %>%
  pull(date_label)

anim_data <- anim_data %>%
  mutate(date_label = factor(date_label, levels = date_order))

cat("Prepared", nrow(anim_data), "aggregated records for animation\n")

# Prepare data for line chart - aggregate by month and event type only
line_data <- data %>%
  mutate(
    year_month = floor_date(week, "month"),
    date_label = format(year_month, "%B %Y")
  ) %>%
  filter(year_month >= as.Date("2023-01-01")) %>%
  filter(event_type %in% c("battles", "violence against civilians", "explosions/remote violence")) %>%
  group_by(year_month, date_label, event_type) %>%
  summarise(
    total_fatalities = sum(fatalities, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year_month) %>%
  mutate(date_label = factor(date_label, levels = date_order))

# Create data for current point indicator (for animation sync)
line_data_points <- line_data %>%
  mutate(state_label = date_label)

cat("Prepared", nrow(line_data), "records for line chart\n")

# Prepare cumulative fatalities data for animated counters
cumulative_data <- line_data %>%
  group_by(event_type) %>%
  arrange(year_month) %>%
  mutate(cumulative_fatalities = cumsum(total_fatalities)) %>%
  ungroup() %>%
  select(year_month, date_label, event_type, cumulative_fatalities)

# Calculate total cumulative fatalities across all event types
total_cumulative <- cumulative_data %>%
  group_by(year_month, date_label) %>%
  summarise(cumulative_fatalities = sum(cumulative_fatalities), .groups = "drop") %>%
  mutate(event_type = "Total",
         state_label = date_label)

# Combine for animation
counter_data <- bind_rows(cumulative_data, total_cumulative) %>%
  mutate(
    state_label = date_label,
    # Position for each counter (y-axis)
    y_pos = case_when(
      event_type == "battles" ~ 4,
      event_type == "violence against civilians" ~ 3,
      event_type == "explosions/remote violence" ~ 2,
      event_type == "Total" ~ 1
    ),
    # Labels
    label_text = case_when(
      event_type == "battles" ~ "Battles",
      event_type == "violence against civilians" ~ "Violence Against Civilians",
      event_type == "explosions/remote violence" ~ "Explosions/Remote Violence",
      event_type == "Total" ~ "TOTAL FATALITIES"
    ),
    # Colors
    text_color = case_when(
      event_type == "battles" ~ "#E41A1C",
      event_type == "violence against civilians" ~ "#377EB8",
      event_type == "explosions/remote violence" ~ "#FF7F00",
      event_type == "Total" ~ "black"
    )
  )

cat("Prepared cumulative data for animated counters\n")

# Define color palette for 3 event types
event_colors <- c(
  "battles" = "#E41A1C",
  "violence against civilians" = "#377EB8",
  "explosions/remote violence" = "#FF7F00"
)

# Create animated map
p_map <- ggplot() +
  # Sudan ADM2 boundaries as background (transparent with visible lines)
  geom_sf(data = sudan_adm2, fill = NA, color = "gray40", linewidth = 0.3) +
  # ADM2 labels (clearer)
  geom_sf_text(data = sudan_adm2, aes(label = NAME_2), size = 2.5, color = "black", fontface = "bold", check_overlap = TRUE) +
  # Conflict points
  geom_point(
    data = anim_data,
    aes(x = longitude, y = latitude, 
        size = total_fatalities, 
        color = event_type),
    alpha = 0.4
  ) +
  # Color scale with larger legend
  scale_color_manual(
    values = event_colors, 
    name = "Event Type",
    guide = guide_legend(
      override.aes = list(size = 6, alpha = 0.8)
    )
  ) +
  # Size scale with transparent circles (outline only)
  scale_size_continuous(
    range = c(3, 18),
    name = "Fatalities",
    labels = scales::comma,
    guide = guide_legend(
      override.aes = list(fill = NA, color = "black", stroke = 1.5, alpha = 1, shape = 21)
    )
  ) +
  # Labels
  labs(
    title = "Sudan Conflict Fatalities",
    subtitle = "{closest_state}",
    caption = ""
  ) +
  # Theme
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
  # Coordinate limits for Sudan (tighter fit)
  coord_sf(xlim = c(21.5, 38.5), ylim = c(8.5, 22.5), expand = FALSE) +
  # Animation
  transition_states(
    date_label,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_fade()

# Create animated line chart (synchronized with map)
p_line <- ggplot() +
  # Full line (static background)
  geom_line(data = line_data, aes(x = year_month, y = total_fatalities, color = event_type, group = event_type), 
            linewidth = 1.2, alpha = 0.3) +
  # Animated points showing current month
  geom_point(data = line_data_points, aes(x = year_month, y = total_fatalities, color = event_type), size = 5) +
  # Vertical line indicator for current date
  geom_vline(data = line_data_points %>% distinct(year_month, state_label), 
             aes(xintercept = year_month), 
             color = "darkred", linewidth = 1, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = event_colors, name = "Event Type") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  labs(
    title = "Monthly Fatalities by Event Type",
    x = NULL,
    y = "Fatalities",
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
  # Animation - use same transition_states as map
  transition_states(
    state_label,
    transition_length = 2,
    state_length = 1
  )

# Create animated counters panel manually (frame by frame)
# Prepare counter data with pre-formatted labels
counter_plot_data <- counter_data %>%
  mutate(
    value_label = as.character(scales::comma(cumulative_fatalities)),
    label_text = as.character(label_text),
    text_color = as.character(text_color)
  )

# Function to create a single counter frame
create_counter_frame <- function(state) {
  frame_data <- counter_plot_data %>% filter(state_label == state)
  
  p <- ggplot(frame_data, aes(y = y_pos)) +
    geom_text(aes(x = 0, label = label_text, color = text_color),
              hjust = 0, size = 3.2, fontface = "bold") +
    geom_text(aes(x = 1, label = value_label, color = text_color),
              hjust = 1, size = 4.5, fontface = "bold") +
    scale_color_identity() +
    scale_x_continuous(limits = c(-0.05, 1.05)) +
    scale_y_continuous(limits = c(0.5, 4.5)) +
    labs(title = "Cumulative Fatalities") +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5, margin = margin(t = 0, b = 5)),
      plot.margin = margin(0, 5, 0, 5),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  return(p)
}

# Render all animations separately
cat("Rendering map animation...\n")
anim_map <- animate(
  p_map,
  nframes = 200,
  fps = 5,
  width = 900,
  height = 750,
  renderer = gifski_renderer()
)

cat("Rendering line chart animation...\n")
anim_line <- animate(
  p_line,
  nframes = 200,
  fps = 5,
  width = 550,
  height = 200,
  renderer = gifski_renderer()
)

# Create counter frames manually
cat("Creating counter frames...\n")
temp_dir <- tempdir()
states <- levels(counter_plot_data$state_label)
n_states <- length(states)

# Map uses transition_length=2, state_length=1, so ratio is 3 frames per state
# But gganimate calculates nframes differently - let's match the actual map frame count
# First render map to get actual frame count
map_gif <- image_read(anim_map)
actual_map_frames <- length(map_gif)
cat("Map has", actual_map_frames, "frames for", n_states, "states\n")

# Calculate frames per state to match map timing
frames_per_state <- actual_map_frames / n_states

counter_frames <- list()
for (i in seq_along(states)) {
  p <- create_counter_frame(states[i])
  
  # Save temporary PNG
  temp_file <- file.path(temp_dir, sprintf("counter_%04d.png", i))
  ggsave(temp_file, p, width = 350/96, height = 200/96, dpi = 96, bg = "white")
  
  # Read image
  img <- image_read(temp_file)
  
  # Calculate how many frames this state should have
  start_frame <- round((i - 1) * frames_per_state) + 1
  end_frame <- round(i * frames_per_state)
  n_frames_for_state <- end_frame - start_frame + 1
  
  # Add frames for this state
  for (j in 1:n_frames_for_state) {
    counter_frames[[length(counter_frames) + 1]] <- img
  }
}

# Combine counter frames into a gif
counter_gif <- do.call(c, counter_frames)
cat("Counter frames generated:", length(counter_gif), "\n")

# Read line animation
cat("Combining animations...\n")
line_gif <- image_read(anim_line)

cat("Map frames:", length(map_gif), "\n")
cat("Line frames:", length(line_gif), "\n")
cat("Counter frames:", length(counter_gif), "\n")

# Ensure same number of frames
n_frames <- min(length(map_gif), length(line_gif), length(counter_gif))

# Combine: map on top, line chart and counters side by side below
combined_frames <- lapply(1:n_frames, function(i) {
  # Combine line chart and counters horizontally
  bottom_row <- image_append(c(line_gif[i], counter_gif[i]), stack = FALSE)
  # Stack map on top of the bottom row
  image_append(c(map_gif[i], bottom_row), stack = TRUE)
})
combined_gif <- do.call(c, combined_frames)

# Save combined animation
image_write(combined_gif, path = "output/sudan_conflict_animated.gif")
cat("Animation saved to: output/sudan_conflict_animated.gif\n")
