#################################################################
# 03 Check RLS / ATRC data for possible errors before publishing
#
# Finds records that may be mistakes, to check with the data custodian
# (RLS / ATRC / UTAS) BEFORE they are shown in the app.
#
# Run AFTER 01_format_rls_data.R (it uses the raw IMOS files and the tidy
# data from script 01). Nothing is changed or removed here, it only lists
# things to check.
#
# Outputs (outputs/rls_checks/):
#   - 00_summary.csv             one row per check with the number of records found
#   - 01_ ... .csv               one file per check with the records to look at
#   - rls_checks.xlsx            everything in one workbook (if writexl is installed)
#################################################################

# Load libraries ----
library(CheckEM)
library(tidyverse)
library(sf)

dir.create("outputs/rls_checks", recursive = TRUE, showWarnings = FALSE)

# Read raw IMOS files (same function as script 01) ----
read_imos_csv <- function(path) {
  header_row <- read_lines(path, n_max = 500) %>%
    str_detect("^FID,") %>%
    which()

  read_csv(path, skip = header_row - 1, guess_max = 100000, show_col_types = FALSE)
}

rls_surveys_raw <- read_imos_csv("data/rls/IMOS_-_National_Reef_Monitoring_Network_Sub-Facility_-_Survey_metadata.csv") %>%
  mutate(survey_id = as.character(survey_id), survey_date = as.Date(survey_date))

rls_fish_raw <- read_imos_csv("data/rls/IMOS_-_National_Reef_Monitoring_Network_Sub-Facility_-_Global_reef_fish_abundance_and_biomass.csv") %>%
  mutate(survey_id = as.character(survey_id), survey_date = as.Date(survey_date))

# Tidy data from script 01 ----
rls_surveys <- read_rds("data/tidy/rls_surveys.rds") # surveys used in the app
rls_blocks  <- read_rds("data/tidy/rls_blocks.rds")
rls_fish    <- read_rds("data/tidy/rls_fish.rds")

# Only check raw records for surveys that are used in the app
fish_raw_used <- rls_fish_raw %>%
  semi_join(rls_surveys, by = "survey_id")

# Survey info to add to every check so records are easy to find
survey_info <- rls_surveys %>%
  select(survey_id, program, bioregion, location, site_code, site_name, date, depth_m)

life_history <- CheckEM::australia_life_history %>%
  select(family, genus, species, australian_common_name, length_max_cm)

# Each check is saved in this list ----
checks <- list()

# 1. Surveys -----------------------------------------------------

## Method 1 surveys with no fish data at all ----
# Real zeros, or data not entered? (dropped from the app)
checks$surveys_without_fish <- rls_surveys_raw %>%
  filter(str_detect(methods, "\\b1\\b"), program %in% c("RLS", "ATRC")) %>%
  anti_join(rls_fish_raw, by = "survey_id") %>%
  select(survey_id, program, area, location, site_code, site_name, survey_date, depth, methods)

## Fish data for surveys that are not in the survey metadata ----
checks$fish_surveys_not_in_metadata <- rls_fish_raw %>%
  anti_join(rls_surveys_raw, by = "survey_id") %>%
  distinct(survey_id, program, area, location, site_code, site_name, survey_date)

## Surveys with only one block ----
# Was the second block not done, not entered, or empty?
checks$surveys_with_one_block <- rls_blocks %>%
  count(survey_id, name = "n_blocks") %>%
  filter(n_blocks == 1) %>%
  left_join(survey_info, by = "survey_id")

## Blocks numbered something other than 1 or 2 ----
checks$unexpected_block_numbers <- rls_blocks %>%
  filter(!block %in% c(1, 2)) %>%
  left_join(survey_info, by = "survey_id")

## Blocks with no fish recorded ----
# Very unusual on NSW reefs (only non-fish, e.g. cephalopods, were recorded)
checks$blocks_without_fish <- rls_blocks %>%
  anti_join(rls_fish, by = c("survey_id", "block")) %>%
  left_join(survey_info, by = "survey_id")

## Possible duplicate surveys ----
# Same site, date and depth entered under more than one survey_id
checks$possible_duplicate_surveys <- rls_surveys %>%
  group_by(site_code, date, depth_m) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(site_code, date, depth_m)

## Unusual depths ----
checks$unusual_depths <- rls_surveys %>%
  filter(is.na(depth_m) | depth_m <= 1 | depth_m > 25) %>%
  arrange(depth_m)

## Dates in the future or missing ----
checks$bad_dates <- rls_surveys %>%
  filter(is.na(date) | date > Sys.Date())

# 2. Sites -------------------------------------------------------

## Site codes with more than one site name or position ----
checks$sites_with_multiple_names_or_positions <- rls_surveys_raw %>%
  group_by(site_code) %>%
  summarise(
    n_names     = n_distinct(site_name),
    n_positions = n_distinct(latitude, longitude),
    site_names  = paste(unique(site_name), collapse = "; "),
    .groups = "drop"
  ) %>%
  filter(n_names > 1 | n_positions > 1)

## Sites near a Sanctuary Zone edge or outside the bioregions (made in script 01) ----
if (file.exists("outputs/rls_site_status_check.csv")) {
  checks$sites_status_near_boundary <- read_csv("outputs/rls_site_status_check.csv", show_col_types = FALSE)
}

## Sites in Commonwealth waters ----
# Seaward of NSW coastal (state) waters (3 nm), or inside a Commonwealth marine park.
# Coordinates are rounded to ~1 km, so look at dist_to_state_waters_m: small
# distances may just be rounding.
site_points <- rls_surveys %>%
  group_by(site_code, site_name, location, bioregion, status, latitude_dd, longitude_dd) %>%
  summarise(n_surveys = n(), first_date = min(date), last_date = max(date), .groups = "drop") %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = FALSE) %>%
  st_transform(3577) # Australian Albers (metres)

# State coastal waters (NSW plus the neighbouring states, for sites near the borders)
state_waters <- st_read("data/spatial/Coastal_Waters_areas_(AMB2020).shp", quiet = TRUE) %>%
  clean_names() %>%
  filter(str_detect(name, "New South Wales|Victoria|Queensland")) %>%
  st_transform(3577) %>%
  st_make_valid() %>%
  st_union()

commonwealth_parks <- readRDS("app_data/spatial/commonwealth.mp.RDS") %>%
  st_as_sf() %>%
  st_transform(3577) %>%
  st_make_valid() %>%
  select(cwth_marine_park_zone = ZoneName)

# Nearest point on the edge of state waters. The coast faces east, so a site
# east of that point is offshore (Commonwealth waters) and a site west of it
# is landward (estuary, river, bay or on land)
edge_x <- st_nearest_points(site_points, state_waters) %>%
  st_cast("POINT") %>%
  st_coordinates() %>%
  as_tibble() %>%
  slice(seq(2, n(), by = 2)) %>% # each line has 2 points: the site, then the nearest edge
  pull(X)

site_waters <- site_points %>%
  mutate(
    site_x = st_coordinates(geometry)[, "X"],
    edge_x = edge_x,
    in_state_waters = lengths(st_intersects(geometry, state_waters)) > 0,
    dist_to_state_waters_m = round(as.numeric(st_distance(geometry, state_waters))),
    water_type = case_when(
      in_state_waters ~ "State coastal waters",
      site_x > edge_x ~ "Offshore of state waters (Commonwealth)",
      TRUE ~ "Landward of state waters (estuary, river, bay or land)"
    ),
    # Site or location names that suggest a river or estuary
    estuary_name = str_detect(
      paste(site_name, location),
      regex("river|creek|lake|lagoon|estuar|inlet|entrance|harbour|bridge|wharf|jetty|marina|channel|basin", ignore_case = TRUE)
    )
  ) %>%
  st_join(commonwealth_parks) %>%
  distinct(site_code, .keep_all = TRUE) %>%
  st_drop_geometry() %>%
  select(-site_x, -edge_x)

checks$sites_in_commonwealth_waters <- site_waters %>%
  filter(water_type == "Offshore of state waters (Commonwealth)" | !is.na(cwth_marine_park_zone)) %>%
  arrange(bioregion, -dist_to_state_waters_m)

## Sites in rivers or estuaries ----
# Landward of state coastal waters, or the site name suggests a river/estuary.
# Large enclosed bays (e.g. Jervis Bay, Port Stephens, Sydney Harbour) are
# landward of the baseline too, so they will show up here - check which
# should be kept with the custodian.
checks$sites_in_rivers_or_estuaries <- site_waters %>%
  filter(water_type == "Landward of state waters (estuary, river, bay or land)" | estuary_name) %>%
  arrange(location, site_name)

# Number of sites and surveys for each location (quicker to read than the full list)
checks$sites_in_rivers_or_estuaries_by_location <- checks$sites_in_rivers_or_estuaries %>%
  group_by(location, water_type) %>%
  summarise(n_sites = n(), n_surveys = sum(n_surveys), .groups = "drop") %>%
  arrange(-n_surveys)

# 3. Species names -----------------------------------------------

## Names not in the CheckEM life history list ----
# Spelling mistakes, old names or morphospecies (e.g. "sp [Lord Howe]")
checks$names_not_in_life_history <- rls_fish %>%
  group_by(family, genus, species) %>%
  summarise(n_records = n(), total_fish = sum(total), n_surveys = n_distinct(survey_id), .groups = "drop") %>%
  anti_join(life_history, by = c("family", "genus", "species")) %>%
  filter(!species %in% "spp") %>%
  arrange(-n_records)

## Coarse IDs dropped from the app (not identified to family) ----
checks$records_not_to_family <- fish_raw_used %>%
  filter(method %in% 1, is.na(family) | is.na(species_name)) %>%
  count(class, species_name, reporting_name, name = "n_records") %>%
  arrange(-n_records)

## Non-fish records dropped from the app ----
checks$non_fish_records <- fish_raw_used %>%
  filter(!class %in% c("Actinopterygii", "Elasmobranchii", "Teleostei", "Actinopteri")) %>%
  count(phylum, class, family, species_name, name = "n_records") %>%
  arrange(-n_records)

## Species recorded on only one survey ----
# Rare species can be real, but are also the most likely to be misidentified
checks$species_seen_once <- rls_fish %>%
  filter(!species %in% "spp") %>%
  group_by(family, genus, species) %>%
  filter(n_distinct(survey_id) == 1) %>%
  ungroup() %>%
  left_join(survey_info, by = "survey_id") %>%
  select(family, genus, species, total, size_class, survey_id, site_name, date, program)

## Recorded name different from the name used in the app ----
# Shows where the synonym list or name cleaning changed a name
checks$names_changed <- rls_fish_raw %>%
  semi_join(rls_surveys, by = "survey_id") %>%
  filter(method %in% 1, !is.na(family)) %>%
  distinct(family_raw = family, species_name) %>%
  separate(species_name, into = c("genus", "species"), extra = "merge", fill = "right", remove = FALSE) %>%
  mutate(species = str_remove_all(species, "\\.")) %>%
  left_join(CheckEM::aus_synonyms, by = c("family_raw" = "family", "genus", "species")) %>%
  filter(!is.na(genus_correct)) %>%
  transmute(
    old_name = species_name,
    new_name = paste(genus_correct, species_correct),
    family_old = family_raw,
    family_new = family_correct
  )

# 4. Counts and sizes --------------------------------------------

## Very large counts in one row ----
# Schools can be real, but these drive total abundance - check they aren't typos
checks$very_large_counts <- rls_fish %>%
  filter(total >= 500) %>%
  left_join(survey_info, by = "survey_id") %>%
  select(scientific, total, size_class, survey_id, block, site_name, date, program) %>%
  arrange(-total)

## Sizes bigger than the maximum length for the species ----
checks$size_bigger_than_max_length <- rls_fish %>%
  left_join(life_history, by = c("family", "genus", "species")) %>%
  filter(!is.na(length_max_cm), size_class > length_max_cm) %>%
  left_join(survey_info, by = "survey_id") %>%
  select(scientific, size_class, length_max_cm, total, survey_id, block, site_name, date, program) %>%
  arrange(-(size_class / length_max_cm))

## Fish with a size class of 0 or missing ----
# These have no size, so they are left out of biomass and the size metrics
checks$missing_or_zero_size <- rls_fish %>%
  filter(is.na(size_class) | size_class == 0) %>%
  left_join(survey_info, by = "survey_id") %>%
  select(scientific, size_class, total, biomass, survey_id, block, site_name, date, program)

## Sized fish with no biomass ----
# Missing length-weight values - these fish add 0 to b20, b30 and BLT
checks$sized_fish_without_biomass <- rls_fish %>%
  filter(size_class > 0, is.na(biomass) | biomass == 0) %>%
  count(scientific, name = "n_records") %>%
  arrange(-n_records)

## Biomass per fish that doesn't match the rest of the species ----
# For one species and size class, the weight of one fish should always be the same.
# Rows more than 10% away from the usual value may have a wrong count or biomass.
checks$inconsistent_biomass <- rls_fish %>%
  filter(total > 0, biomass > 0) %>%
  mutate(grams_per_fish = biomass / total) %>%
  group_by(scientific, size_class) %>%
  mutate(usual_grams_per_fish = median(grams_per_fish)) %>%
  ungroup() %>%
  filter(abs(grams_per_fish / usual_grams_per_fish - 1) > 0.1) %>%
  left_join(survey_info, by = "survey_id") %>%
  select(scientific, size_class, total, biomass, grams_per_fish, usual_grams_per_fish,
         survey_id, block, site_name, date, program)

## Raw rows that are duplicated ----
# Same survey, block, species and size class entered more than once (summed in the app)
checks$duplicate_rows <- fish_raw_used %>%
  filter(method %in% 1) %>%
  count(survey_id, block, species_name, size_class, name = "n_rows") %>%
  filter(n_rows > 1) %>%
  left_join(survey_info, by = "survey_id")

## Transects with unusually high total abundance ----
# More than 3 x the 99th percentile for the bioregion
checks$transects_with_very_high_abundance <- rls_fish %>%
  group_by(survey_id) %>%
  summarise(total_fish = sum(total), .groups = "drop") %>%
  left_join(survey_info, by = "survey_id") %>%
  group_by(bioregion) %>%
  filter(total_fish > 3 * quantile(total_fish, 0.99)) %>%
  ungroup() %>%
  arrange(-total_fish)

# Summary and save ----
checks_summary <- tibble(
  check = names(checks),
  n_records = map_int(checks, nrow)
) %>%
  mutate(file = paste0(str_pad(row_number(), 2, pad = "0"), "_", check, ".csv"))

checks_summary

write_csv(checks_summary, "outputs/rls_checks/00_summary.csv")

walk2(checks, checks_summary$file, \(data, file) {
  write_csv(data, file.path("outputs/rls_checks", file))
})

# Everything in one workbook (sheet names max 31 characters)
if (requireNamespace("writexl", quietly = TRUE)) {
  sheets <- c(list(summary = checks_summary), checks)
  names(sheets) <- str_sub(names(sheets), 1, 31)
  writexl::write_xlsx(sheets, "outputs/rls_checks/rls_checks.xlsx")
}
