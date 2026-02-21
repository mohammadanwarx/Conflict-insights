# Base Map Utilities for DRC Conflict Animation
# Purpose: Create reusable base map with OSM-style tiles
# Usage: source("R/base_map_drc.R") before creating maps

#===================================
# Fix PROJ database conflict (PostgreSQL/PostGIS)
#===================================
Sys.unsetenv("PROJ_LIB")
Sys.unsetenv("PROJ_DATA")

#===================================
# Install/load required packages
#===================================
user_lib_path <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library", 
                            paste0(R.version$major, ".", R.version$minor))
if (!dir.exists(user_lib_path)) {
  dir.create(user_lib_path, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib_path, .libPaths()))

required_packages <- c("sf", "maptiles", "tidyterra", "terra", "ggplot2")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing:", pkg, "\n")
    install.packages(pkg, lib = user_lib_path, repos = "https://cloud.r-project.org", quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

library(sf)
library(maptiles)
library(tidyterra)
library(terra)
library(ggplot2)

#===================================
# Function to get DRC base map tiles
#===================================
get_drc_basemap <- function(
    boundaries,
    provider = "CartoDB.Positron",
    zoom = NULL,
    crop = TRUE,
    cachedir = "data/raw/tiles"
) {
  # Create cache directory if needed
  if (!dir.exists(cachedir)) {
    dir.create(cachedir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Get tiles for the extent of the boundaries
  tiles <- get_tiles(
    x = boundaries,
    provider = provider,
    zoom = zoom,
    crop = crop,
    cachedir = cachedir,
    verbose = TRUE
  )
  
  return(tiles)
}

#===================================
# Function to create base map layer
#===================================
create_drc_basemap <- function(
    drc_boundaries,
    provider = "CartoDB.Positron",
    zoom = 6,
    show_adm2 = TRUE,
    adm2_color = "gray40",
    adm2_linewidth = 0.15,
    adm2_fill = NA
) {
  # Get basemap tiles
  tiles <- get_drc_basemap(drc_boundaries, provider = provider, zoom = zoom)
  
  # Create base ggplot with tiles
  p <- ggplot() +
    geom_spatraster_rgb(data = tiles)
  
  # Add ADM2 boundaries if requested
  if (show_adm2) {
    p <- p + geom_sf(
      data = drc_boundaries,
      fill = adm2_fill,
      color = adm2_color,
      linewidth = adm2_linewidth
    )
  }
  
  # Apply minimal theme
  p <- p + theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
  
  return(p)
}

#===================================
# Pre-cache DRC tiles for animation
#===================================
cache_drc_tiles <- function(drc_boundaries, providers = c("CartoDB.Positron")) {
  cat("Pre-caching tiles for DRC...\n")
  
  for (prov in providers) {
    cat("  Provider:", prov, "\n")
    tryCatch({
      tiles <- get_drc_basemap(drc_boundaries, provider = prov, zoom = 6)
      cat("  -> Cached successfully\n")
    }, error = function(e) {
      cat("  -> Error:", e$message, "\n")
    })
  }
  
  cat("Caching complete.\n")
}

cat("Base map utilities loaded. Functions available:\n")
cat("  - get_drc_basemap(boundaries, provider, zoom)\n")
cat("  - create_drc_basemap(drc_boundaries, provider, zoom, show_adm2)\n")
cat("  - cache_drc_tiles(drc_boundaries, providers)\n")
