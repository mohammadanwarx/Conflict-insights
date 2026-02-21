# DRC Conflict Animation - Publication Quality
# Purpose: Create LinkedIn-optimized animated visualization
# Output: output/drc_conflict_publication.mp4 (1200x1200px)
#
# Panel Layout:
#   - Map Panel: Upper-left (780x780)
#   - Line Chart: Bottom-left (780x300)
#   - Text Panel: Right column (360x1080)

#===================================
# Setup
#===================================
Sys.unsetenv("PROJ_LIB")
Sys.unsetenv("PROJ_DATA")

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
# Packages
#===================================
user_lib_path <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library", 
                            paste0(R.version$major, ".", R.version$minor))
if (!dir.exists(user_lib_path)) {
  dir.create(user_lib_path, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib_path, .libPaths()))

required_packages <- c("tidyverse", "lubridate", "sf", "gganimate", "gifski", 
                       "magick", "readxl", "geodata", "maptiles", "tidyterra", 
                       "terra", "scales", "av")
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
library(magick)
library(readxl)
library(geodata)
library(maptiles)
library(tidyterra)
library(terra)
library(scales)
library(av)

#===================================
# Configuration
#===================================
CANVAS_WIDTH <- 1200
CANVAS_HEIGHT <- 1200
FPS <- 10
TARGET_DURATION <- 12  # seconds
TARGET_FRAMES <- FPS * TARGET_DURATION  # 120 frames

# Panel dimensions
MAP_WIDTH <- 780
MAP_HEIGHT <- 780
LINE_WIDTH <- 780
LINE_HEIGHT <- 300
TEXT_WIDTH <- 360
TEXT_HEIGHT <- 1080

# Colors (accessibility-friendly)
event_colors <- c(
  "battles" = "#B33A3A",                    # Muted red
  "violence against civilians" = "#3A6FB3", # Blue
  "explosions/remote violence" = "#D97B2A"  # Orange
)

bg_color <- "#F5F5F5"  # Light neutral background

#===================================
# Load DRC boundaries
#===================================
drc_gpkg_path <- "data/raw/drc_adm2.gpkg"

if (!file.exists(drc_gpkg_path)) {
  cat("Downloading DRC ADM2 boundaries...\n")
  drc_adm2_raw <- gadm(country = "COD", level = 2, path = "data/raw")
  drc_adm2 <- st_as_sf(drc_adm2_raw)
  st_write(drc_adm2, drc_gpkg_path, delete_dsn = TRUE)
} else {
  drc_adm2 <- st_read(drc_gpkg_path, quiet = TRUE)
}
cat("Loaded DRC ADM2 with", nrow(drc_adm2), "regions\n")

# Get country outline
drc_outline <- drc_adm2 %>% st_union()

#===================================
# Download basemap tiles
#===================================
cat("Downloading basemap tiles...\n")
tiles_cache <- "data/raw/tiles"
if (!dir.exists(tiles_cache)) dir.create(tiles_cache, recursive = TRUE)

# Extended bbox for full canvas coverage
drc_bbox <- st_bbox(drc_adm2)
extended_bbox <- st_bbox(c(
  xmin = 8,
  xmax = 36,
  ymin = -16,
  ymax = 8
), crs = st_crs(drc_adm2))

extended_area <- st_as_sfc(extended_bbox)

drc_tiles <- get_tiles(
  x = extended_area,
  provider = "CartoDB.Positron",
  zoom = 5,
  crop = TRUE,
  cachedir = tiles_cache,
  verbose = FALSE
)
cat("Basemap tiles ready\n")

#===================================
# Load and process data
#===================================
cat("Loading data from Excel...\n")
raw_data <- read_excel("data/raw/Africa_aggregated_data_up_to-2026-02-07.xlsx")
names(raw_data) <- tolower(names(raw_data))
names(raw_data)[names(raw_data) == "centroid_latitude"] <- "latitude"
names(raw_data)[names(raw_data) == "centroid_longitude"] <- "longitude"

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

#===================================
# Prepare animation data
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

# Create ordered factor
date_order <- anim_data %>%
  distinct(year_month, date_label) %>%
  arrange(year_month) %>%
  pull(date_label)

anim_data <- anim_data %>%
  mutate(date_label = factor(date_label, levels = date_order))

n_states <- length(unique(anim_data$date_label))
cat("Time states:", n_states, "\n")

# Calculate frames per state for target duration
frames_per_state <- max(1, round(TARGET_FRAMES / n_states))
total_frames <- n_states * frames_per_state
cat("Target frames:", total_frames, "at", FPS, "fps =", total_frames/FPS, "seconds\n")

#===================================
# Prepare cumulative data
#===================================
line_data_raw <- data %>%
  mutate(
    year_month = floor_date(week, "month"),
    date_label = format(year_month, "%B %Y")
  ) %>%
  filter(tolower(event_type) %in% c("battles", "violence against civilians", "explosions/remote violence")) %>%
  mutate(event_type = tolower(event_type)) %>%
  group_by(year_month, date_label, event_type) %>%
  summarise(total_fatalities = sum(fatalities, na.rm = TRUE), .groups = "drop")

# Complete grid for all months × event types
all_months <- line_data_raw %>% distinct(year_month, date_label)
all_event_types <- tibble(event_type = c("battles", "violence against civilians", "explosions/remote violence"))
complete_grid <- crossing(all_months, all_event_types)

line_data <- complete_grid %>%
  left_join(line_data_raw, by = c("year_month", "date_label", "event_type")) %>%
  mutate(total_fatalities = replace_na(total_fatalities, 0)) %>%
  arrange(year_month)

# Calculate cumulative
cumulative_data <- line_data %>%
  group_by(event_type) %>%
  arrange(year_month) %>%
  mutate(
    cumulative_fatalities = cumsum(total_fatalities),
    state_label = factor(date_label, levels = date_order)
  ) %>%
  ungroup()

# Total cumulative
total_cumulative <- cumulative_data %>%
  group_by(year_month, date_label) %>%
  summarise(cumulative_fatalities = sum(cumulative_fatalities), .groups = "drop") %>%
  mutate(event_type = "Total")

counter_data <- bind_rows(
  cumulative_data %>% select(year_month, date_label, event_type, cumulative_fatalities),
  total_cumulative
) %>%
  mutate(
    state_label = factor(date_label, levels = date_order),
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
      event_type == "battles" ~ event_colors["battles"],
      event_type == "violence against civilians" ~ event_colors["violence against civilians"],
      event_type == "explosions/remote violence" ~ event_colors["explosions/remote violence"],
      event_type == "Total" ~ "black"
    )
  )

cat("Data preparation complete\n")

#===================================
# Create static basemap with cartographic elements
#===================================
cat("Creating static basemap...\n")

# Pre-render map base layer (tiles + boundaries) once
p_map_base <- ggplot() +
  geom_spatraster_rgb(data = drc_tiles) +
  geom_sf(data = drc_adm2, fill = NA, color = "gray50", linewidth = 0.2) +
  geom_sf(data = drc_outline, fill = NA, color = "#333333", linewidth = 0.8) +
  coord_sf(xlim = c(11, 32), ylim = c(-14, 6), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = bg_color, color = NA)
  )

map_base_path <- file.path(tempdir(), "map_base.png")
ggsave(map_base_path, p_map_base, width = MAP_WIDTH/96, height = MAP_HEIGHT/96, dpi = 96, bg = bg_color)
map_base_img <- image_read(map_base_path)
cat("Map base layer cached\n")

# Get coordinate transformation for points
map_xlim <- c(11, 32)
map_ylim <- c(-14, 6)

#===================================
# Create map panel frame function (optimized - only renders points)
#===================================
create_map_points <- function(state_label_value) {
  frame_data <- anim_data %>% filter(date_label == state_label_value)
  
  # Only render points on transparent background
  p <- ggplot() +
    geom_point(
      data = frame_data,
      aes(x = longitude, y = latitude, size = total_fatalities, color = event_type),
      alpha = 0.7
    ) +
    scale_color_manual(values = event_colors, guide = "none") +
    scale_size_continuous(range = c(6, 40), guide = "none") +
    scale_x_continuous(limits = map_xlim, expand = c(0, 0)) +
    scale_y_continuous(limits = map_ylim, expand = c(0, 0)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  return(p)
}

# Create title overlay function
create_map_title <- function(state_label_value) {
  p <- ggplot() +
    annotate("text", x = 0.5, y = 0.95, label = "DRC Conflict Fatalities", 
             size = 6, fontface = "bold", color = "#1a1a1a", hjust = 0.5) +
    annotate("text", x = 0.5, y = 0.88, label = state_label_value, 
             size = 5.5, fontface = "bold", color = "#B33A3A", hjust = 0.5) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      plot.margin = margin(0, 0, 0, 0)
    )
  
  return(p)
}

#===================================
# Create line chart frame function
#===================================
create_line_frame <- function(state_label_value) {
  current_date <- cumulative_data %>% 
    filter(state_label == state_label_value) %>% 
    pull(year_month) %>% 
    unique()
  
  visible_data <- cumulative_data %>%
    filter(year_month <= current_date)
  
  current_point <- visible_data %>%
    filter(year_month == current_date)
  
  p <- ggplot() +
    geom_line(data = visible_data, 
              aes(x = year_month, y = cumulative_fatalities, color = event_type, group = event_type),
              linewidth = 1.2, alpha = 0.9) +
    geom_point(data = current_point, 
               aes(x = year_month, y = cumulative_fatalities, color = event_type),
               size = 3) +
    scale_color_manual(values = event_colors, guide = "none") +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0.02, 0.1))) +
    scale_x_date(date_labels = "%Y", date_breaks = "3 years", 
                 limits = range(cumulative_data$year_month)) +
    labs(
      title = "Cumulative Fatalities by Event Type",
      x = NULL,
      y = "Cumulative Fatalities"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5, color = "#1a1a1a",
                                margin = margin(t = 5, b = 5)),
      axis.text.x = element_text(size = 9, color = "#333333"),
      axis.text.y = element_text(size = 9, color = "#333333"),
      axis.title.y = element_text(size = 10, color = "#333333"),
      panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA),
      plot.margin = margin(10, 15, 10, 15)
    )
  
  return(p)
}

#===================================
# Create text panel frame function
#===================================
create_text_frame <- function(state_label_value) {
  frame_data <- counter_data %>% filter(state_label == state_label_value)
  
  p <- ggplot(frame_data, aes(y = y_pos)) +
    # Category boxes
    geom_rect(aes(xmin = 0, xmax = 1, ymin = y_pos - 0.4, ymax = y_pos + 0.4),
              fill = "#ffffff", color = "#cccccc", linewidth = 0.5) +
    # Labels
    geom_text(aes(x = 0.05, label = label_text, color = text_color),
              hjust = 0, size = 4, fontface = "bold") +
    # Values
    geom_text(aes(x = 0.95, label = scales::comma(cumulative_fatalities), color = text_color),
              hjust = 1, size = 5.5, fontface = "bold") +
    scale_color_identity() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0.3, 4.7)) +
    labs(title = "Cumulative Fatalities") +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = "#1a1a1a",
                                margin = margin(t = 10, b = 15)),
      plot.background = element_rect(fill = bg_color, color = NA),
      plot.margin = margin(10, 15, 10, 15)
    )
  
  return(p)
}

#===================================
# Create legend panel
#===================================
create_legend <- function() {
  legend_data <- tibble(
    event_type = c("Battles", "Violence Against Civilians", "Explosions/Remote Violence"),
    color = c(event_colors["battles"], event_colors["violence against civilians"], 
              event_colors["explosions/remote violence"]),
    y = c(3, 2, 1)
  )
  
  p <- ggplot(legend_data, aes(y = y)) +
    geom_point(aes(x = 0.1, color = color), size = 5, alpha = 0.8) +
    geom_text(aes(x = 0.2, label = event_type), hjust = 0, size = 3.5, color = "#333333") +
    scale_color_identity() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0.5, 3.5)) +
    labs(title = "Event Types") +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5, color = "#1a1a1a",
                                margin = margin(t = 5, b = 10)),
      plot.background = element_rect(fill = bg_color, color = NA),
      plot.margin = margin(5, 10, 5, 10)
    )
  
  return(p)
}

#===================================
# Render all frames
#===================================
cat("Rendering frames...\n")
temp_dir <- file.path(tempdir(), "drc_frames")
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

states <- levels(anim_data$date_label)

# Create legend once
legend_plot <- create_legend()
legend_file <- file.path(temp_dir, "legend.png")
ggsave(legend_file, legend_plot, width = TEXT_WIDTH/96, height = 150/96, dpi = 96, bg = bg_color)
legend_img <- image_read(legend_file)

frame_count <- 0
for (i in seq_along(states)) {
  state <- states[i]
  
  # Create panels (optimized: only render points, not base map)
  points_plot <- create_map_points(state)
  title_plot <- create_map_title(state)
  line_plot <- create_line_frame(state)
  text_plot <- create_text_frame(state)
  
  # Save panels
  points_file <- file.path(temp_dir, sprintf("points_%04d.png", i))
  title_file <- file.path(temp_dir, sprintf("title_%04d.png", i))
  line_file <- file.path(temp_dir, sprintf("line_%04d.png", i))
  text_file <- file.path(temp_dir, sprintf("text_%04d.png", i))
  
  ggsave(points_file, points_plot, width = MAP_WIDTH/96, height = MAP_HEIGHT/96, dpi = 96, bg = "transparent")
  ggsave(title_file, title_plot, width = MAP_WIDTH/96, height = 80/96, dpi = 96, bg = "transparent")
  ggsave(line_file, line_plot, width = LINE_WIDTH/96, height = LINE_HEIGHT/96, dpi = 96, bg = bg_color)
  ggsave(text_file, text_plot, width = TEXT_WIDTH/96, height = 400/96, dpi = 96, bg = bg_color)
  
  # Read images
  points_img <- image_read(points_file)
  title_img <- image_read(title_file)
  line_img <- image_read(line_file)
  text_img <- image_read(text_file)
  
  # Compose frame
  # Start with background
  frame <- image_blank(CANVAS_WIDTH, CANVAS_HEIGHT, color = bg_color)
  
  # Add map base (cached - no re-rendering)
  frame <- image_composite(frame, map_base_img, offset = "+0+70")
  
  # Add points overlay
  frame <- image_composite(frame, points_img, offset = "+0+70")
  
  # Add title
  frame <- image_composite(frame, title_img, offset = "+0+0")
  
  # Add other panels
  frame <- image_composite(frame, line_img, offset = "+0+850")
  frame <- image_composite(frame, text_img, offset = "+820+200")
  frame <- image_composite(frame, legend_img, offset = "+820+600")
  
  # Add north arrow (top-right of map)
  frame <- image_annotate(frame, "N", location = "+730+85", size = 18, 
                         color = "#444444", font = "Arial", weight = 700)
  frame <- image_annotate(frame, "▲", location = "+727+68", size = 20, 
                         color = "#444444")
  
  # Add footer
  frame <- image_annotate(frame, "Data: ACLED | Visualization: Mo Anwar", 
                         location = "+450+1175", size = 14, color = "#666666")
  
  # Duplicate frame for smoother animation
  for (j in 1:frames_per_state) {
    frame_count <- frame_count + 1
    frame_path <- file.path(temp_dir, sprintf("frame_%05d.png", frame_count))
    image_write(frame, frame_path)
  }
  
  if (i %% 20 == 0) cat("Processed state", i, "of", length(states), "\n")
}

cat("Total frames:", frame_count, "\n")

#===================================
# Create MP4 video
#===================================
cat("Creating MP4...\n")
frame_files <- list.files(temp_dir, pattern = "^frame_.*\\.png$", full.names = TRUE)
frame_files <- sort(frame_files)

output_mp4 <- "output/drc_conflict_publication.mp4"
av_encode_video(frame_files, output = output_mp4, framerate = FPS)
cat("MP4 saved to:", output_mp4, "\n")

#===================================
# Create GIF (smaller file)
#===================================
cat("Creating GIF...\n")
all_frames <- image_read(frame_files[seq(1, length(frame_files), by = 2)])  # Every other frame
all_frames <- image_animate(all_frames, fps = FPS/2)

output_gif <- "output/drc_conflict_publication.gif"
image_write(all_frames, output_gif)
cat("GIF saved to:", output_gif, "\n")

# Cleanup
unlink(temp_dir, recursive = TRUE)

cat("\n=== COMPLETE ===\n")
cat("MP4:", output_mp4, "\n")
cat("GIF:", output_gif, "\n")
