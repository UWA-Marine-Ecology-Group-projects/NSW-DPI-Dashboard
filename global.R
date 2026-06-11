library(shiny)
library(bslib)
library(leaflet)
# library(leafgl)
library(ggplot2)
library(dplyr)
library(bsicons)
library(sf)
library(shinycssloaders)
library(scales)

source("R/helpers.R")          # defines filter_by_park(), ensure_sf_ll(), base_map(), etc.
source("R/mod_park_summary.R") # defines mod_park_summary_ui/server (contains session$onFlushed INSIDE)
source("R/theme.R")

# Load data -----
load("app_data/nsw_bruv_data.Rdata") # TODO add RLS here too

# Calculate average latitude for main map ----
min_lat <- min(nsw_bruv_data$bruv_metadata$latitude_dd)
max_lat <- max(nsw_bruv_data$bruv_metadata$latitude_dd)

mean_lat <- (min_lat + max_lat)/2

# # Spatial files for maps ----
commonwealth.mp <- readRDS("app_data/spatial/commonwealth.mp.RDS") %>%
  st_as_sf() 

# state_mp <- readRDS("app_data/spatial/sa_state_mp.RDS")

# # Pallettes for maps ----
# state.pal <- colorFactor(c("#f18080", # Restricted Access Zone (RAZ)
#                            "#69a802", # Sanctuary Zone (SZ)
#                            "#799CD2", # Habitat Protection (HPZ)
#                            "#BED4EE" # General Managed Use Zone (GMUZ)
# ), state_mp$zone)
# 
# # unique(state.mp$zone_type)
# 
commonwealth.pal <- colorFactor(c("#f6c1d9", # Sanctuary
                                  "#7bbc63", # National Park
                                  "#fdb930", # Recreational Use
                                  "#fff7a3", # Habitat Protection
                                  '#b9e6fb', # Multiple Use
                                  '#ccc1d6'# Special Purpose
), commonwealth.mp$zone)


# These are used to build the metric UI plots -----
metric_defs <- c(
  species_richness      = "Species richness",
  total_abundance = "Total abundance",
  cti           = "Community temperature index",
  blt   = "Biomass of large teleosts"
)

metric_y_lab <- list(
  species_richness      = "Average No. species",
  total_abundance = "Average No. individuals",
  cti           = "Community temperature index (°C)",
  blt   = "Biomass (kg)"
)

# Deploy app ----
# renv::status()
# renv::snapshot(prompt = FALSE)
# 
# rsconnect::appDependencies()
