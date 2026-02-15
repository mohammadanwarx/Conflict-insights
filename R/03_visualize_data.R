# Script 03: Visualize Sudan ACLED Data
# Purpose: Create visualizations of Sudan conflict data
# Output: Plots saved to output/

library(tidyverse)
library(lubridate)

# Load processed Sudan data
data <- read_csv("data/processed/Sudan_fatalities_filtered.csv", show_col_types = FALSE)
cat("Loaded", nrow(data), "records\n")

# ===== 1. FATALITIES OVER TIME =====
plot_fatalities_over_time <- function(data) {
  p <- data %>%
    mutate(year_month = floor_date(week, "month")) %>%
    group_by(year_month) %>%
    summarise(total_fatalities = sum(fatalities, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = year_month, y = total_fatalities)) +
    geom_line(color = "darkred", linewidth = 0.8) +
    geom_smooth(method = "loess", color = "blue", alpha = 0.2, se = TRUE) +
    labs(
      title = "Sudan Conflict Fatalities Over Time",
      subtitle = "Monthly aggregated fatalities from ACLED data",
      x = "Date",
      y = "Total Fatalities"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggsave("output/sudan_fatalities_timeline.png", p, width = 12, height = 6, dpi = 300)
  cat("Saved: output/sudan_fatalities_timeline.png\n")
  return(p)
}

# ===== 2. EVENT TYPES DISTRIBUTION =====
plot_event_types <- function(data) {
  p <- data %>%
    group_by(event_type) %>%
    summarise(
      total_events = sum(events, na.rm = TRUE),
      total_fatalities = sum(fatalities, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = reorder(event_type, total_fatalities), y = total_fatalities)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = scales::comma(total_fatalities)), hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(
      title = "Fatalities by Event Type in Sudan",
      x = "Event Type",
      y = "Total Fatalities"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14)) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15)))
  
  ggsave("output/sudan_event_types.png", p, width = 10, height = 6, dpi = 300)
  cat("Saved: output/sudan_event_types.png\n")
  return(p)
}

# ===== 3. FATALITIES BY REGION (ADMIN1) =====
plot_by_region <- function(data, top_n = 15) {
  p <- data %>%
    group_by(admin1) %>%
    summarise(total_fatalities = sum(fatalities, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_fatalities)) %>%
    slice(1:top_n) %>%
    ggplot(aes(x = reorder(admin1, total_fatalities), y = total_fatalities)) +
    geom_col(fill = "coral") +
    geom_text(aes(label = scales::comma(total_fatalities)), hjust = -0.1, size = 3) +
    coord_flip() +
    labs(
      title = paste("Top", top_n, "Regions by Fatalities in Sudan"),
      x = "Region (Admin1)",
      y = "Total Fatalities"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14)) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15)))
  
  ggsave("output/sudan_regions_fatalities.png", p, width = 10, height = 8, dpi = 300)
  cat("Saved: output/sudan_regions_fatalities.png\n")
  return(p)
}

# ===== 4. YEARLY FATALITIES =====
plot_yearly_fatalities <- function(data) {
  p <- data %>%
    mutate(year = year(week)) %>%
    group_by(year) %>%
    summarise(total_fatalities = sum(fatalities, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = factor(year), y = total_fatalities)) +
    geom_col(fill = "darkgreen") +
    geom_text(aes(label = scales::comma(total_fatalities)), vjust = -0.3, size = 3) +
    labs(
      title = "Yearly Fatalities in Sudan",
      x = "Year",
      y = "Total Fatalities"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1)))
  
  ggsave("output/sudan_yearly_fatalities.png", p, width = 12, height = 6, dpi = 300)
  cat("Saved: output/sudan_yearly_fatalities.png\n")
  return(p)
}

# ===== RUN ALL VISUALIZATIONS =====
cat("\n=== Generating Visualizations ===\n")
plot_fatalities_over_time(data)
plot_event_types(data)
plot_by_region(data)
plot_yearly_fatalities(data)
cat("\nAll visualizations saved to output/ folder.\n")
