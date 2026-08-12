# Load libraries
library(CheckEM)
library(dplyr)
library(stringr)
library(tidyr)
library(here)
library(sf)
library(vegan)

# TODO - should Pseudocaranx georgianus be spp in everything?

# Read in data ----
bruv_metadata <- readRDS("data/raw/bruv_metadata_nsw.rds") %>%
  dplyr::select(campaignid, sample, everything()) %>%
  dplyr::mutate(year = str_sub(date_time, 1, 4)) %>%
  dplyr::mutate(date = as.Date(date_time)) %>%
  dplyr::mutate(month = str_sub(date_time, 6, 7)) %>%
  dplyr::filter(!month %in% c("01", "02", "12")) %>% # Remove summer
  glimpse

unique(bruv_metadata$month) %>% sort()

campaign_lookup <- bruv_metadata %>%
  dplyr::select(campaignid, sample, date_time, date) %>%
  sf::st_drop_geometry() %>%
  dplyr::arrange(campaignid, date) %>%
  dplyr::group_by(campaignid) %>%
  dplyr::mutate(
    days_since_previous = as.numeric(date - dplyr::lag(date)),
    new_event = is.na(days_since_previous) | days_since_previous > 14,
    event_number = cumsum(new_event)
  ) %>%
  dplyr::group_by(campaignid, event_number) %>%
  dplyr::mutate(
    start_date = min(date),
    new_campaignid = paste0(campaignid, "_", format(start_date, "%Y%m%d"))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(campaignid, date, event_number, start_date, new_campaignid) %>%
  dplyr::mutate(start_month = str_sub(start_date, 1, 7)) %>%
  dplyr::mutate(year = str_sub(date, 1, 4)) %>%
  dplyr::distinct()

unique(bruv_metadata$campaignid)

write.csv(bruv_metadata %>% distinct(campaignid) %>% arrange(campaignid), "nsw_campaigns.csv")

unique(campaign_lookup$new_campaignid) # 88 unique

# THIS is too many plots for each campaign
bruv_metadata %>% 
  left_join(campaign_lookup) %>%
  distinct(bioregion, new_campaignid) %>%
  group_by(bioregion) %>%
  count()

bruv_metadata %>% 
  distinct(bioregion, year) %>%
  group_by(bioregion) %>%
  count()

# Create bioregion lookup ----
bioregions_shp <- sf::st_read("data/spatial/Marine_Bioregions.shp") %>% clean_names()

bioregions_shp <- st_transform(bioregions_shp, 4326)

bioregions <- bruv_metadata %>%
  select(sample_url, bioregion, status) %>%
  sf::st_drop_geometry()

# Create state marineparks ----
# TODO move this somewhere else

# state_shp <- sf::st_read("data/spatial/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Marine.shp") %>%
#   clean_names() %>%
#   dplyr::filter(state %in% "NSW") %>%
#   dplyr::select(name, type, iucn, state, comments) %>%
#   mutate(
#     zone_type = str_extract(
#       comments,
#       regex(
#         "Habitat Protection Zone|Special Purpose Zone|Sanctuary Zone|General Use Zone|Aquatic Reserve",
#         ignore_case = FALSE
#       )
#     )
#   )
# 
# state_mp <- saveRDS(state_shp, "app_data/spatial/sa_state_mp.rds")

# Get list of fish/shark/rays species ----
fishes <- CheckEM::australia_life_history %>%
  dplyr::filter(class %in% c("Actinopterygii", "Elasmobranchii")) %>%
  dplyr::select(family, genus, species, rls_thermal_niche, australian_common_name)

unique(fishes$class)

bruv_count <- readRDS("data/raw/bruv_count_nsw.rds") %>%
  dplyr::mutate(species = if_else(genus %in% "Pseudocaranx", "georgianus", species)) %>%
  semi_join(fishes) %>%
  semi_join(bruv_metadata)

bruv_length <- readRDS("data/raw/bruv_length_nsw.rds") %>%
  left_join(bioregions) %>%
  semi_join(fishes) %>%
  semi_join(bruv_metadata)

# Create lists of samples to join to complete data ----
count_samples <- bruv_metadata %>%
  filter(successful_count %in% TRUE)

length_samples <- bruv_metadata %>%
  filter(successful_length %in% TRUE)

# Calculate Metrics ----
## Total Abundance ----
# TODO Make sure that all species are fish!!
# TODO Check if this should include spps

total_abundance_samples <- bruv_count %>%
  dplyr::group_by(sample_url) %>%
  dplyr::summarise(value = sum(count)) %>%
  dplyr::full_join(count_samples) %>%
  replace_na(list(value = 0)) %>%
  dplyr::mutate(metric = "total_abundance") %>%
  dplyr::select(campaignid, sample, sample_url, metric, value) %>%
  glimpse

## Species richness ----
# TODO Make sure that all species are fish!!

species_richness_samples <- bruv_count %>%
  distinct(sample_url, family, genus, species) %>%
  dplyr::group_by(sample_url) %>%
  dplyr::summarise(value = n()) %>%
  dplyr::full_join(count_samples) %>%
  replace_na(list(value = 0)) %>%
  dplyr::mutate(metric = "species_richness") %>%
  dplyr::select(campaignid, sample, sample_url, metric, value) %>%
  glimpse()

## CTI ----
cti_samples <- bruv_count %>%
  left_join(bruv_metadata) %>%
  CheckEM::create_cti() %>%
  dplyr::full_join(count_samples) %>%
  dplyr::filter(!is.na(cti)) %>%
  dplyr::mutate(metric = "cti") %>%
  dplyr::rename(value = cti) %>%
  dplyr::select(campaignid, sample, sample_url, metric, value) %>%
  glimpse()

names(cti_samples)

samples_missing_cti <- anti_join(bruv_metadata, cti_samples) %>%
  dplyr::select(sample_url) %>%
  left_join(total_abundance_samples) %>%
  dplyr::filter(value > 0) %>%
  left_join(bruv_count)

distinct_species <- samples_missing_cti %>% distinct(family, genus, species)

# TODO make sure there are no non-fish in the cti first!
# TODO check where the empty samples are
# TODO 23 species that don't have cti values- should we try and get these?
# TODO # 13 samples missing fish - ask Tim what to do for samples that did not observe any fish? Todd and I spoke about it and I think that I remove them.

## BLT ----
large_bodied_carnivores <- CheckEM::australia_life_history %>%
  dplyr::filter(fb_trophic_level > 2.8) %>%
  dplyr::filter(length_max_cm > 40) %>%
  dplyr::filter(class %in% "Actinopterygii") %>%
  dplyr::filter(!order %in% c("Anguilliformes", "Ophidiiformes", "Notacanthiformes","Syngnathiformes", 
                              "Synbranchiformes", "Stomiiformes", "Siluriformes", "Saccopharyngiformes", "Osmeriformes", 
                              "Osteoglossiformes", "Lophiiformes", "Lampriformes", "Beloniformes")) %>%
  # left_join(maturity_mean) %>%
  dplyr::mutate(fb_length_at_maturity_mm = fb_length_at_maturity_cm * 10) %>%
  dplyr::mutate(l50 = fb_length_at_maturity_mm) %>%
  dplyr::filter(!is.na(l50)) %>%
  dplyr::select(family, genus, species, l50, fb_a, fb_b, fb_a_ll, fb_b_ll) %>% #, wa_l50
  glimpse()

species_count <- bruv_count %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(total_count = sum(count))

nsw_lbc <- semi_join(large_bodied_carnivores, bruv_count) %>%
  left_join(species_count)
# 69 species in NSW data that have trophic level over 2.8 and max length above 400 mm
# only 20 of them have a size of maturity

genus_length <- CheckEM::australia_life_history %>%
  dplyr::group_by(family, genus) %>%
  summarise(fb_genus_a = mean(fb_a, na.rm = T),
            fb_genus_b = mean(fb_b, na.rm = T),
            fb_genus_a_ll = mean(fb_a_ll, na.rm = T),
            fb_genus_b_ll = mean(fb_b_ll, na.rm = T)) %>%
  glimpse()

length_mass_lbc <- bruv_length %>%
  left_join(large_bodied_carnivores) %>%
  dplyr::left_join(genus_length) %>%
  dplyr::mutate(fb_a = if_else(is.na(fb_a), fb_genus_a, fb_a),
                fb_b = if_else(is.na(fb_b), fb_genus_b, fb_b),
                fb_a_ll = if_else(is.na(fb_a_ll), fb_genus_a_ll, fb_a_ll),
                fb_b_ll = if_else(is.na(fb_b_ll), fb_genus_b_ll, fb_b_ll)) %>%
  dplyr::filter(!is.na(l50),
                !is.na(length_mm)) %>%
  dplyr::mutate(fb_b_ll = if_else(is.na(fb_b_ll), 1, fb_b_ll)) %>%
  dplyr::mutate(fb_a_ll = if_else(is.na(fb_a_ll), 0, fb_a_ll)) %>%
  dplyr::mutate(adjlength = (((length_mm/10) * fb_b_ll) + fb_a_ll)) %>% 
  dplyr::mutate(mass_g = (adjlength ^ fb_b) * fb_a * count) %>%
  glimpse()

blt_samples <- length_mass_lbc %>%
  dplyr::filter(length_mm > l50) %>% # FOR BLT > Length of maturity
  dplyr::group_by(sample_url) %>%
  dplyr::summarise(biomass_kg = sum(mass_g, na.rm = T)/1000) %>%
  dplyr::right_join(length_samples) %>%
  dplyr::mutate(biomass_kg = if_else(is.na(biomass_kg), 0, biomass_kg)) %>%
  dplyr::mutate(metric = "blt") %>%
  dplyr::rename(value = biomass_kg) %>%
  dplyr::select(campaignid, sample, sample_url, metric, value) 

alt_samples <- length_mass_lbc %>%
  dplyr::filter(length_mm > l50) %>% # FOR BLT > Length of maturity
  dplyr::group_by(sample_url) %>%
  dplyr::summarise(number = sum(count)) %>%
  dplyr::right_join(length_samples) %>%
  dplyr::mutate(number = if_else(is.na(number), 0, number)) %>%
  dplyr::mutate(metric = "alt") %>%
  dplyr::rename(value = number) %>%
  dplyr::select(campaignid, sample, sample_url, metric, value) 

lh <- CheckEM::australia_life_history

length_for_size_biomass <- bruv_length %>%
  left_join(CheckEM::australia_life_history) %>%
  dplyr::left_join(genus_length) %>%
  dplyr::mutate(fb_a = if_else(is.na(fb_a), fb_genus_a, fb_a),
                fb_b = if_else(is.na(fb_b), fb_genus_b, fb_b),
                fb_a_ll = if_else(is.na(fb_a_ll), fb_genus_a_ll, fb_a_ll),
                fb_b_ll = if_else(is.na(fb_b_ll), fb_genus_b_ll, fb_b_ll)) %>%
  dplyr::filter(!is.na(length_mm)) %>%
  dplyr::mutate(fb_b_ll = if_else(is.na(fb_b_ll), 1, fb_b_ll)) %>%
  dplyr::mutate(fb_a_ll = if_else(is.na(fb_a_ll), 0, fb_a_ll)) %>%
  dplyr::select(sample_url, family, genus, species, length_mm, count, fb_a, fb_b, fb_a_ll, fb_b_ll) %>%
  dplyr::mutate(adjlength_cm = (((length_mm/10) * fb_b_ll) + fb_a_ll)) %>% 
  dplyr::mutate(mass_g = (adjlength_cm ^ fb_b) * fb_a * count) %>%
  glimpse()

b20_samples <- length_for_size_biomass %>%
  dplyr::filter(length_mm > 200) %>% # FOR BLT > Length of maturity
  dplyr::group_by(sample_url) %>%
  dplyr::summarise(biomass_kg = sum(mass_g, na.rm = T)/1000) %>%
  dplyr::right_join(length_samples) %>%
  dplyr::mutate(biomass_kg = if_else(is.na(biomass_kg), 0, biomass_kg)) %>%
  dplyr::mutate(metric = "b20") %>%
  dplyr::rename(value = biomass_kg) %>%
  dplyr::select(campaignid, sample, sample_url, metric, value) 

a20_samples <- length_for_size_biomass %>%
  dplyr::filter(length_mm > 200) %>% # FOR BLT > Length of maturity
  dplyr::group_by(sample_url) %>%
  dplyr::summarise(number = sum(count)) %>%
  dplyr::right_join(length_samples) %>%
  dplyr::mutate(number = if_else(is.na(number), 0, number)) %>%
  dplyr::mutate(metric = "a20") %>%
  dplyr::rename(value = number) %>%
  dplyr::select(campaignid, sample, sample_url, metric, value) 

b30_samples <- length_for_size_biomass %>%
  dplyr::filter(length_mm > 300) %>% # FOR BLT > Length of maturity
  dplyr::group_by(sample_url) %>%
  dplyr::summarise(biomass_kg = sum(mass_g, na.rm = T)/1000) %>%
  dplyr::right_join(length_samples) %>%
  dplyr::mutate(biomass_kg = if_else(is.na(biomass_kg), 0, biomass_kg)) %>%
  dplyr::mutate(metric = "b30") %>%
  dplyr::rename(value = biomass_kg) %>%
  dplyr::select(campaignid, sample, sample_url, metric, value) 

a30_samples <- length_for_size_biomass %>%
  dplyr::filter(length_mm > 300) %>% # FOR BLT > Length of maturity
  dplyr::group_by(sample_url) %>%
  dplyr::summarise(number = sum(count)) %>%
  dplyr::right_join(length_samples) %>%
  dplyr::mutate(number = if_else(is.na(number), 0, number)) %>%
  dplyr::mutate(metric = "a30") %>%
  dplyr::rename(value = number) %>%
  dplyr::select(campaignid, sample, sample_url, metric, value) 

# Combine all metrics ----
metrics <- bind_rows(total_abundance_samples, 
                     species_richness_samples, 
                     cti_samples,
                     blt_samples,
                     alt_samples,
                     b20_samples,
                     a20_samples,
                     b30_samples,
                     a30_samples) %>%
  left_join(bioregions) %>%
  left_join(count_samples %>% 
              select(sample_url, sample, date_time, date, status)) %>%
  left_join(campaign_lookup)

## Indicator Species and most abundant species -----
# TODO start with top 50 most abundant

number_of_species <- bruv_count %>%
  distinct(family, genus, species)

# 325 species

nrow(count_samples) * nrow(number_of_species) # should have 1,042,600 rows

complete_bruv_count <- bruv_count %>%
  full_join(count_samples) %>%
  complete(sample_url, nesting(family, genus, species)) %>%
  dplyr::filter(!is.na(family)) %>%
  replace_na(list(count = 0)) %>%
  dplyr::select(sample_url, family, genus, species, count) %>%
  left_join(bioregions) 

top_50_most_abundant_species_overall <- complete_bruv_count %>%
  dplyr::group_by(family, genus, species) %>% 
  dplyr::summarise(average_abundance = mean(count),
                   se = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count)))) %>%
  ungroup() %>%
  arrange(-average_abundance) %>%
  slice_head(n = 50) %>%
  dplyr::mutate(group = "overall") %>%
  dplyr::mutate(by_status = FALSE) %>%
  glimpse

top_50_most_abundant_species_bioregion <- complete_bruv_count %>%
  dplyr::group_by(bioregion, family, genus, species) %>% 
  dplyr::summarise(
    average_abundance = mean(count),
    se = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count)))) %>%
  dplyr::group_by(bioregion) %>%
  dplyr::arrange(dplyr::desc(average_abundance), .by_group = TRUE) %>%
  dplyr::slice_head(n = 50) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(group = "bioregion") %>%
  dplyr::mutate(by_status = FALSE) %>%
  glimpse

# Make a list of unique species to join once calculated by status
top_50_species_overall <- top_50_most_abundant_species_overall %>%
  distinct(family, genus, species)

top_50_most_abundant_species_overall_status <- complete_bruv_count %>%
  dplyr::group_by(status, family, genus, species) %>% 
  dplyr::summarise(average_abundance = mean(count),
                   se = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count)))) %>%
  ungroup() %>%
  semi_join(top_50_species_overall) %>%
  dplyr::mutate(group = "overall") %>%
  dplyr::mutate(by_status = TRUE) %>%
  glimpse

top_50_species_bioregions <- top_50_most_abundant_species_bioregion %>%
  distinct(bioregion, family, genus, species)

top_50_most_abundant_species_bioregion_status <- complete_bruv_count %>%
  dplyr::group_by(bioregion, status, family, genus, species) %>% 
  dplyr::summarise(
    average_abundance = mean(count),
    .groups = "drop",
    se = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count)))) %>%
  ungroup() %>%
  semi_join(top_50_species_bioregions) %>%
  dplyr::mutate(group = "bioregion") %>%
  dplyr::mutate(by_status = TRUE) %>%
  glimpse

common_names <- CheckEM::australia_life_history %>%
  select(family, genus, species, australian_common_name)

top_50_most_abundant_species_bioregion_status_year <- complete_bruv_count %>%
  left_join(count_samples) %>%
  dplyr::group_by(bioregion, status, year, family, genus, species) %>% 
  dplyr::summarise(
    average_abundance = mean(count),
    .groups = "drop",
    se = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count)))) %>%
  ungroup() %>%
  semi_join(top_50_species_bioregions) %>%
  dplyr::mutate(group = "bioregion") %>%
  dplyr::mutate(by_status = TRUE) %>%
  glimpse %>%
  left_join(common_names) %>%
  mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")"))  %>%
  tidyr::extract(
    display_name,
    into   = c("sci", "common"),
    regex  = "^(.*?)\\s*\\((.*?)\\)$",
    remove = FALSE
  ) |>
  dplyr::mutate(
    label = paste0("*", sci, "*<br>(", common, ")")
  ) %>%
  glimpse

names(top_50_most_abundant_species_bioregion_status_year)


# TODO add top_50_most_abundant_species_park
# TODO by year??

# Combine top species data frames ----


top_species <- bind_rows(top_50_most_abundant_species_overall,
                         top_50_most_abundant_species_bioregion,
                         top_50_most_abundant_species_overall_status,
                         top_50_most_abundant_species_bioregion_status,
) %>%
  left_join(common_names) %>%
  mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")"))  %>%
  tidyr::extract(
    display_name,
    into   = c("sci", "common"),
    regex  = "^(.*?)\\s*\\((.*?)\\)$",
    remove = FALSE
  ) |>
  dplyr::mutate(
    label = paste0("*", sci, "*<br>(", common, ")")
  ) %>%
  glimpse

# Quick stats for overview page ----
overview_stats <- tibble::tibble(
  num_bruvs = nrow(count_samples),
  num_fish = sum(bruv_count$count, na.rm = TRUE),
  num_lengths = sum(bruv_length$count, na.rm = TRUE),
  biggest_fish = max(bruv_length$length_mm, na.rm = TRUE),
  min_year = min(count_samples$year, na.rm = TRUE),
  max_year = max(count_samples$year, na.rm = TRUE),
  min_depth = min(count_samples$depth_m, na.rm = TRUE),
  max_depth = max(count_samples$depth_m, na.rm = TRUE)
)

bioregion_metadata_stats <- count_samples %>%
  group_by(bioregion) %>%
  summarise(
    num_bruvs = n(),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    min_depth = min(depth_m, na.rm = TRUE),
    max_depth = max(depth_m, na.rm = TRUE)
  )

bioregion_count_stats <- complete_bruv_count %>%
  group_by(bioregion) %>%
  summarise(
    num_fish = sum(count, na.rm = TRUE)
  )

bioregion_length_stats <- bruv_length %>%
  group_by(bioregion) %>%
  summarise(
    num_lengths = sum(count, na.rm = TRUE),
    biggest_fish = max(length_mm, na.rm = TRUE)
  )

bioregion_stats <- list(
  bioregion_metadata_stats,
  bioregion_count_stats,
  bioregion_length_stats) %>%
  purrr::reduce(left_join, by = "bioregion")

# Dataframes to use in app
bruv_metadata
overview_stats
bioregion_stats
top_species
metrics



# Diagnostic plots ----
# CTI -----
sti <- CheckEM::australia_life_history %>%
  clean_names() %>%
  dplyr::select(family, genus, species, rls_thermal_niche) %>%
  mutate(scientific = paste(family, genus, species, sep = " ")) %>%
  dplyr::distinct() %>%
  glimpse()

# Thermal Index stacked plot
cti_top_10 <- complete_bruv_count %>%
  left_join(count_samples) %>%
  mutate(scientific = paste(family, genus, species, sep = " ")) %>%
  mutate(label = paste(genus, species, sep = " ")) %>%
  group_by(bioregion, year, scientific, label) %>%
  summarise(
    n    = sum(!is.na(count)),
    maxn = mean(count, na.rm = TRUE),
    se   = stats::sd(count, na.rm = TRUE) / sqrt(n),#sd(count, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop") %>%
  left_join(sti) %>%
  filter(!is.na(rls_thermal_niche)) %>%
  group_by(bioregion, year) %>%
  slice_max(order_by = maxn, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  # dplyr::filter(bioregion %in% "Tweed-Moreton") %>%
  left_join(common_names) %>%
  mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")"))  %>%
  tidyr::extract(
    display_name,
    into   = c("sci", "common"),
    regex  = "^(.*?)\\s*\\((.*?)\\)$",
    remove = FALSE
  ) |>
  dplyr::mutate(
    label = paste0("*", sci, "*<br>(", common, ")"),
    niche_lab = scales::number(rls_thermal_niche, accuracy = 0.01)
  ) %>%
  glimpse

top_50_abundance <- complete_bruv_count %>%
  semi_join(top_50_species_bioregions) %>%
  left_join(bruv_metadata) %>%
  left_join(common_names) %>%
  mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")"))

# Species accumulation curves ----

# Metadata for successful BRUV deployments
# st_drop_geometry() keeps this much lighter for the SAC calculation
sac_sample_info <- count_samples %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    sample_url,
    sample,
    campaignid,
    bioregion,
    status,
    year
  ) %>%
  dplyr::filter(
    !is.na(bioregion),
    !is.na(status),
    !is.na(year)
  ) %>%
  dplyr::distinct()


# Summarise fish observations to one row per deployment/species
sac_species_counts <- bruv_count %>%
  dplyr::semi_join(
    sac_sample_info,
    by = "sample_url"
  ) %>%
  dplyr::filter(
    !is.na(genus),
    !is.na(species)
  ) %>%
  dplyr::mutate(
    scientific_name = paste(family, genus, species)
  ) %>%
  dplyr::group_by(
    sample_url,
    scientific_name
  ) %>%
  dplyr::summarise(
    count = sum(count, na.rm = TRUE),
    .groups = "drop"
  )


# Convert to deployment x species matrix
sac_species_wide <- sac_species_counts %>%
  tidyr::pivot_wider(
    names_from = scientific_name,
    values_from = count,
    values_fill = 0
  )


# Keep a record of which columns are species columns
species_cols <- setdiff(
  names(sac_species_wide),
  "sample_url"
)


# Join back to ALL successful deployments.
#
# This is important because a successful BRUV deployment with zero fish
# should still contribute one deployment to the accumulation curve.
sac_wide <- sac_sample_info %>%
  dplyr::left_join(
    sac_species_wide,
    by = "sample_url"
  ) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(species_cols),
      ~ tidyr::replace_na(.x, 0)
    )
  ) %>%
  dplyr::arrange(
    bioregion,
    year,
    status,
    sample_url
  )


# Function to calculate a sample-based species accumulation curve
make_species_accumulation <- function(df, species_cols) {
  
  species_mat <- df %>%
    dplyr::select(
      dplyr::all_of(species_cols)
    ) %>%
    as.data.frame()
  
  # No curve can be calculated if there are no deployments/species
  if (nrow(species_mat) == 0 || ncol(species_mat) == 0) {
    return(
      tibble::tibble(
        deployments = numeric(),
        richness = numeric(),
        sd = numeric()
      )
    )
  }
  
  # Convert abundance to presence / absence
  species_pa <- vegan::decostand(
    species_mat,
    method = "pa"
  )
  
  # Random-order sample accumulation
  sac <- vegan::specaccum(
    species_pa,
    method = "random",
    permutations = 999
  )
  
  tibble::tibble(
    deployments = sac$sites,
    richness = sac$richness,
    sd = sac$sd
  )
}


# Set seed so curves are reproducible whenever the data file is rebuilt
set.seed(123)


# Calculate one SAC for every:
# bioregion x year x protection status
species_accumulation <- sac_wide %>%
  dplyr::group_by(
    bioregion,
    year,
    status
  ) %>%
  dplyr::group_modify(
    ~ make_species_accumulation(
      df = .x,
      species_cols = species_cols
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    year = as.character(year),
    lower = pmax(
      richness - tidyr::replace_na(sd, 0),
      0
    ),
    upper = richness + tidyr::replace_na(sd, 0)
  ) %>%
  glimpse()

maturity_mean <- maturity %>%
  dplyr::group_by(family, genus, species, sex) %>%
  dplyr::slice(which.min(l50_mm)) %>%
  ungroup() %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(wa_l50 = mean(l50_mm)) %>%
  ungroup() %>%
  glimpse()




# Combined data
nsw_bruv_data <- structure(
  list(
    # Dataframes
    bruv_metadata = bruv_metadata,
    overview_stats = overview_stats,
    bioregion_stats = bioregion_stats,
    top_species = top_species,
    top_50_most_abundant_species_bioregion_status_year = top_50_most_abundant_species_bioregion_status_year,
    metrics = metrics,
    
    # Diagnostic data
    species_accumulation = species_accumulation,
    cti_top_10 = cti_top_10, 
    
    # TODO add shapefiles here
    bioregions_shp = bioregions_shp,
    
    top_50_abundance = top_50_abundance
    
  ), class = "data")

save(nsw_bruv_data, file = here::here("app_data/nsw_bruv_data.Rdata"))

