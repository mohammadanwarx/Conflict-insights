# Script 04: Animated Sudan Conflict Map
# Purpose: Animate fatalities over time on Sudan map with event type legend
# Output: Animated GIF saved to output/

library(tidyverse)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(gganimate)

# Load processed Sudan data
data <- read_csv("data/processed/Sudan_fatalities_filtered.csv", show_col_types = FALSE)
cat("Loaded", nrow(data), "records\n")

# Get Sudan boundaries
sudan <- ne_countries(scale = "medium", country = "Sudan", returnclass = "sf")

# Prepare data for animation - aggregate by month and event type
anim_data <- data %>%
  mutate(
    year_month = floor_date(week, "month"),
    date_label = format(year_month, "%B %Y")
  ) %>%
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

# Define color palette for event types
event_colors <- c(
  "battles" = "#E41A1C",
  "violence against civilians" = "#377EB8",
  "explosions/remote violence" = "#FF7F00",
  "riots" = "#984EA3",
  "protests" = "#4DAF4A",
  "strategic developments" = "#A65628"
)

# Create animated map
p <- ggplot() +
  # Sudan boundary
  geom_sf(data = sudan, fill = "gray90", color = "black", linewidth = 0.5) +
  # Conflict points
  geom_point(
    data = anim_data,
    aes(x = longitude, y = latitude, 
        size = total_fatalities, 
        color = event_type),
    alpha = 0.7
  ) +
  # Color scale
  scale_color_manual(values = event_colors, name = "Event Type") +
  # Size scale
  scale_size_continuous(
    range = c(1, 10),
    name = "Fatalities",
    labels = scales::comma
  ) +
  # Labels
  labs(
    title = "Sudan Conflict Fatalities",
    subtitle = "Date: {closest_state}",
    caption = "Data Source: ACLED"
  ) +
  # Theme
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "darkred"),
    legend.position = "right",
    panel.grid = element_blank()
  ) +
  # Coordinate limits for Sudan

coord_sf(xlim = c(21, 39), ylim = c(8, 23)) +
  # Animation
  transition_states(
    date_label,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_fade()

# Render animation
cat("Rendering animation (this may take a few minutes)...\n")
anim <- animate(
  p,
  nframes = 200,
  fps = 5,
  width = 800,
  height = 800,
  renderer = gifski_renderer()
)

# Save animation
anim_save("output/sudan_conflict_animated.gif", animation = anim)
cat("Animation saved to: output/sudan_conflict_animated.gif\n")
