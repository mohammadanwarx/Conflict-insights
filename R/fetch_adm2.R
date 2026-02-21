# Script: Fetch Sudan ADM2 via GADM API
# Purpose: Download Sudan ADM2 boundaries and create a labeled map

#===================================
# Set Working Directory
#===================================
# Get script directory - works with Rscript and RStudio
args <- commandArgs(trailingOnly = FALSE)
script_path <- NULL
for (arg in args) {
  if (startsWith(arg, "--file=")) {
    script_path <- normalizePath(sub("--file=", "", arg))
    break
  }
}

if (!is.null(script_path)) {
  project_root <- dirname(dirname(script_path))
} else {
  # Try rstudioapi if available
  project_root <- tryCatch({
    if (require("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
      dirname(script_dir)
    } else {
      getwd()
    }
  }, error = function(e) {
    getwd()
  })
}

setwd(project_root)
cat("Working directory set to:", getwd(), "\n\n")

#===================================
# Install Required Packages
#===================================
cat("=== INSTALLING PACKAGES ===\n")

# Set up user library if needed
user_lib_path <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library", 
                            paste0(R.version$major, ".", R.version$minor))
if (!dir.exists(user_lib_path)) {
  dir.create(user_lib_path, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib_path, .libPaths()))

# Install packages if needed
required_packages <- c("geodata", "sf", "ggplot2", "dplyr", "terra")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Installing: %s...\n", pkg))
    install.packages(pkg, lib = user_lib_path, repos = "https://cloud.r-project.org", quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
  cat(sprintf("Loaded: %s\n", pkg))
}

#===================================
# Create Directories
#===================================
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

#===================================
# Download Sudan ADM2 from GADM
#===================================
cat("\n=== DOWNLOADING SUDAN ADM2 ===\n")

sudan_adm2 <- geodata::gadm(country = "SDN", level = 2, path = "data/raw")

# Convert to sf object
sudan_adm2_sf <- sf::st_as_sf(sudan_adm2)

cat("ADM2 downloaded successfully!\n")
cat("Number of ADM2 regions:", nrow(sudan_adm2_sf), "\n")
cat("Columns:", paste(names(sudan_adm2_sf), collapse = ", "), "\n")

#===================================
# Display ADM2 Names
#===================================
cat("\n=== ADM2 NAMES ===\n")
adm2_info <- sudan_adm2_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::select(NAME_1, NAME_2) %>%
  dplyr::arrange(NAME_1, NAME_2) %>%
  as.data.frame()

print(adm2_info)

#===================================
# Visualize Sudan ADM2 with Labels
#===================================
cat("\n=== VISUALIZING ADM2 ===\n")

sudan_adm2_map <- ggplot2::ggplot(sudan_adm2_sf) +
  ggplot2::geom_sf(ggplot2::aes(fill = NAME_1), color = "black", linewidth = 0.3, alpha = 0.6) +
  ggplot2::geom_sf_text(ggplot2::aes(label = NAME_2), size = 2, check_overlap = TRUE) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Sudan Administrative Divisions (ADM2)",
    subtitle = "Source: GADM Database",
    fill = "State (ADM1)",
    x = "Longitude",
    y = "Latitude"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
    axis.text = ggplot2::element_text(size = 8),
    legend.position = "right",
    legend.text = ggplot2::element_text(size = 7),
    legend.title = ggplot2::element_text(size = 9)
  )

# Display the plot
print(sudan_adm2_map)

# Save the map
ggplot2::ggsave("output/Sudan_ADM2_map.png", sudan_adm2_map, width = 14, height = 16, dpi = 300)
cat("Map saved to: output/Sudan_ADM2_map.png\n")

# Save ADM2 as GeoPackage
sf::st_write(sudan_adm2_sf, "data/raw/sudan_adm2.gpkg", delete_layer = TRUE, quiet = TRUE)
cat("ADM2 shapefile saved to: data/raw/sudan_adm2.gpkg\n")

cat("\n=== DONE ===\n")
