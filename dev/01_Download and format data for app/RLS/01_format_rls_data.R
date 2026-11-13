#################################################################
# 01 Format RLS / ATRC Method 1 (fish) data for the NSW dashboard
#
# Inputs (IMOS / AODN downloads, saved in data/rls/):
#   - IMOS_-_National_Reef_Monitoring_Network_Sub-Facility_-_Survey_metadata.csv
#   - IMOS_-_National_Reef_Monitoring_Network_Sub-Facility_-_Global_reef_fish_abundance_and_biomass.csv
#
# Decisions (checked with Brooke 13/11/2026):
#   - Method 1 fish only (this download has no M2 cryptic fish or inverts)
#   - Programs: RLS and ATRC (Parks Vic dropped)
#   - All months kept (no summer filter, unlike the BRUVs)
#   - Clipped to the NSW marine bioregions (data/spatial/Marine_Bioregions.shp)
#     like the BRUVs. A 2 km buffer is used because public RLS site
#     coordinates are rounded to 2 decimal places (~1 km)
#   - Status: "No-Take" = inside a NSW Sanctuary Zone, everything else "Fished"
#     (same spellings as the BRUV data). Sites near a zone boundary are written
#     to outputs/rls_site_status_check.csv to check; corrections go in
#     data/lookups/rls_site_status_corrections.csv (columns: site_code, status)
#   - Fish only: Actinopterygii + Elasmobranchii ("Teleostei" and
#     "Actinopteri" relabelled as Actinopterygii)
#   - Records not identified to at least family are dropped
#   - Blocks are kept separate here. Metrics are calculated per block and then
#     averaged to the transect (survey) in script 02, like DEW
#
# Outputs (data/tidy/):
#   - rls_surveys.rds  one row per transect (survey_id) with site, bioregion, status, date
#   - rls_blocks.rds   one row per surveyed block (includes blocks with no fish)
#   - rls_fish.rds     cleaned fish records (block x species x size class)
#################################################################

# Load libraries ----
library(CheckEM)
library(tidyverse)
library(sf)

dir.create("data/tidy", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# Read IMOS csv files ----
# The files start with ~70 lines of "#" notes. We can't use comment = "#"
# because some site names contain a "#" (e.g. "Potato Point Spud #1"), so we
# skip down to the header row instead.
read_imos_csv <- function(path) {
  header_row <- read_lines(path, n_max = 500) %>%
    str_detect("^FID,") %>%
    which()

  read_csv(path, skip = header_row - 1, guess_max = 100000, show_col_types = FALSE)
}

rls_surveys_raw <- read_imos_csv("data/rls/IMOS_-_National_Reef_Monitoring_Network_Sub-Facility_-_Survey_metadata.csv")
rls_fish_raw    <- read_imos_csv("data/rls/IMOS_-_National_Reef_Monitoring_Network_Sub-Facility_-_Global_reef_fish_abundance_and_biomass.csv")

# Survey list ----
rls_surveys <- rls_surveys_raw %>%
  filter(str_detect(methods, "\\b1\\b")) %>% # Method 1 surveys only ("1" but not "12" or "13")
  filter(program %in% c("RLS", "ATRC")) %>%
  mutate(
    survey_id   = as.character(survey_id),
    survey_date = as.Date(survey_date),
    year        = as.character(year(survey_date))
  ) %>%
  select(survey_id, program, area, location, site_code, site_name, survey_date, year, depth,
         latitude, longitude, survey_latitude, survey_longitude)

unique(rls_surveys$program)
range(rls_surveys$survey_date)

# Site list with coordinates ----
# Use the precise survey positions where a site has them, otherwise the rounded site position
rls_sites <- rls_surveys %>%
  group_by(site_code) %>%
  summarise(
    site_name    = first(site_name),
    area         = first(area),
    location     = first(location),
    latitude_dd  = if_else(all(is.na(survey_latitude)),  first(latitude),  mean(survey_latitude,  na.rm = TRUE)),
    longitude_dd = if_else(all(is.na(survey_longitude)), first(longitude), mean(survey_longitude, na.rm = TRUE)),
    precise_coordinates = any(!is.na(survey_latitude)),
    .groups = "drop"
  ) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = FALSE) %>%
  st_transform(3577) # Australian Albers (metres) for distances

# Bioregions (same shapefile as the BRUVs) ----
bioregions <- st_read("data/spatial/Marine_Bioregions.shp") %>%
  clean_names() %>%
  st_transform(3577) %>%
  st_make_valid() %>%
  select(bioregion)

# NSW Sanctuary Zones (No-Take) ----
sanctuary_zones <- readRDS("app_data/spatial/sa_state_mp.RDS") %>% # NSW state marine parks (CAPAD)
  st_as_sf() %>%
  filter(zone_type %in% "Sanctuary Zone") %>%
  st_transform(3577) %>%
  st_make_valid() %>%
  st_union()

# Add bioregion and status to each site ----
rls_sites_bioregion <- rls_sites %>%
  st_join(bioregions, join = st_is_within_distance, dist = 2000) %>% # 2 km buffer for rounded coordinates
  distinct(site_code, .keep_all = TRUE) %>% # if a site is within 2 km of two bioregions keep the first
  mutate(
    dist_to_sanctuary_m = round(as.numeric(st_distance(geometry, sanctuary_zones))),
    status = if_else(dist_to_sanctuary_m == 0, "No-Take", "Fished")
  ) %>%
  st_drop_geometry()

# Manual status corrections (optional) ----
corrections_path <- "data/lookups/rls_site_status_corrections.csv"

if (file.exists(corrections_path)) {
  status_corrections <- read_csv(corrections_path, show_col_types = FALSE) %>%
    select(site_code, status_corrected = status)

  rls_sites_bioregion <- rls_sites_bioregion %>%
    left_join(status_corrections, by = "site_code") %>%
    mutate(status = coalesce(status_corrected, status)) %>%
    select(-status_corrected)
}

# Write a csv of sites to check ----
# Sites outside every bioregion (dropped) and sites within 1 km of a Sanctuary Zone edge
rls_site_check <- rls_sites_bioregion %>%
  filter(is.na(bioregion) | dist_to_sanctuary_m < 1000) %>%
  arrange(bioregion, dist_to_sanctuary_m)

write_csv(rls_site_check, "outputs/rls_site_status_check.csv")

# Sites dropped because they are outside the bioregions
rls_sites_bioregion %>%
  filter(is.na(bioregion)) %>%
  count(area, location)

rls_sites_bioregion %>% count(bioregion, status)

# Final survey list ----
rls_surveys_final <- rls_surveys %>%
  inner_join(
    rls_sites_bioregion %>%
      filter(!is.na(bioregion)) %>%
      select(site_code, bioregion, status, latitude_dd, longitude_dd),
    by = "site_code"
  ) %>%
  rename(date = survey_date, depth_m = depth) %>%
  select(survey_id, program, location, site_code, site_name, bioregion, status,
         date, year, depth_m, latitude_dd, longitude_dd)

# Block list ----
# Made BEFORE the fish-only filter, so a block that only recorded e.g. an
# octopus is kept as a block with zero fish
rls_blocks <- rls_fish_raw %>%
  mutate(survey_id = as.character(survey_id)) %>%
  filter(method %in% 1) %>%
  semi_join(rls_surveys_final, by = "survey_id") %>%
  distinct(survey_id, block)

# Surveys in the survey list with no fish data at all (can't tell if they are zeros or missing, so dropped like DEW)
surveys_without_fish <- anti_join(rls_surveys_final, rls_blocks, by = "survey_id")
surveys_without_fish

rls_surveys_final <- rls_surveys_final %>%
  semi_join(rls_blocks, by = "survey_id")

# Number of blocks per survey (69 surveys only have one block)
rls_blocks %>%
  count(survey_id, name = "n_blocks") %>%
  count(n_blocks)

# Clean fish data ----
rls_fish <- rls_fish_raw %>%
  mutate(survey_id = as.character(survey_id)) %>%
  filter(method %in% 1) %>%
  semi_join(rls_surveys_final, by = "survey_id") %>%
  select(survey_id, block, class, family, species_name, reporting_name, size_class, total, biomass) %>%

  # Fish only
  mutate(class = if_else(class %in% c("Teleostei", "Actinopteri"), "Actinopterygii", class)) %>%
  filter(class %in% c("Actinopterygii", "Elasmobranchii")) %>%

  # Split names into genus and species (same steps as DEW)
  separate(species_name, into = c("genus", "species"), extra = "merge", fill = "right", remove = FALSE) %>%
  mutate(species = str_remove_all(species, "\\.")) %>% # "spp." to "spp"
  mutate(genus = if_else(genus == family, "Unknown", genus)) %>% # e.g. "Labridae spp."
  replace_na(list(family = "Unknown", genus = "Unknown", species = "spp")) %>%
  filter(!family %in% "Unknown") %>% # drops records not identified to family (e.g. "Actinopterygii spp.")

  # Update synonyms using CheckEM
  left_join(CheckEM::aus_synonyms, by = c("family", "genus", "species")) %>%
  mutate(
    genus   = if_else(!is.na(genus_correct), genus_correct, genus),
    species = if_else(!is.na(species_correct), species_correct, species),
    family  = if_else(!is.na(family_correct), family_correct, family)
  ) %>%
  select(-c(family_correct, genus_correct, species_correct)) %>%
  mutate(scientific = paste(family, genus, species)) %>%

  # Combine any duplicate rows (same block, species and size class)
  group_by(survey_id, block, class, family, genus, species, scientific, size_class) %>%
  summarise(
    total   = sum(total, na.rm = TRUE),
    biomass = sum(biomass, na.rm = TRUE), # grams, from RLS
    .groups = "drop"
  )

# Checks ----
unique(rls_fish$class)
length(unique(rls_fish$scientific))

# Names that changed or have no match in the CheckEM life history list
rls_fish %>%
  distinct(family, genus, species) %>%
  anti_join(CheckEM::australia_life_history, by = c("family", "genus", "species"))

# Every survey and block in the fish data is in the block list
nrow(anti_join(rls_fish, rls_blocks, by = c("survey_id", "block"))) == 0

# Number of transects and blocks
length(unique(rls_surveys_final$survey_id))
nrow(rls_blocks)

# Save tidy data ----
write_rds(rls_surveys_final, "data/tidy/rls_surveys.rds")
write_rds(rls_blocks, "data/tidy/rls_blocks.rds")
write_rds(rls_fish, "data/tidy/rls_fish.rds")
