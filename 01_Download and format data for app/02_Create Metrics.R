# Load libraries
library(CheckEM)
library(dplyr)
library(stringr)
library(tidyr)
library(here)
library(sf)

# TODO - should Pseudocaranx georgianus be spp in everything?

# Read in data ----
bruv_metadata <- readRDS("data/raw/bruv_metadata_nsw.rds") %>%
  dplyr::select(campaignid, sample, everything()) %>%
  dplyr::mutate(year = str_sub(date_time, 1, 4))

# Create bioregion lookup ----
bioregions_shp <- sf::st_read("data/spatial/Marine_Bioregions.shp") %>% clean_names()

bioregions_shp <- st_transform(bioregions_shp, 4326)

bioregions <- bruv_metadata %>%
  select(sample_url, bioregion) %>%
  sf::st_drop_geometry()

bruv_count <- readRDS("data/raw/bruv_count_nsw.rds")

bruv_length <- readRDS("data/raw/bruv_length_nsw.rds") %>%
  left_join(bioregions)

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
# TODO add

# Combine all metrics ----
metrics <- bind_rows(total_abundance_samples, 
                     species_richness_samples, 
                     cti_samples) %>%
  left_join(bioregions)


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
  dplyr::summarise(average_abundance = mean(count), .groups = "drop") %>%
  ungroup() %>%
  arrange(-average_abundance) %>%
  slice_head(n = 50) %>%
  dplyr::mutate(group = "overall") %>%
  glimpse

top_50_most_abundant_species_bioregion <- complete_bruv_count %>%
  dplyr::group_by(bioregion, family, genus, species) %>% 
  dplyr::summarise(
    average_abundance = mean(count, na.rm = TRUE),
    .groups = "drop") %>%
  dplyr::group_by(bioregion) %>%
  dplyr::arrange(dplyr::desc(average_abundance), .by_group = TRUE) %>%
  dplyr::slice_head(n = 50) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(group = "bioregion") %>%
  glimpse

test <- top_50_most_abundant_species_bioregion %>%
  group_by(bioregion) %>%
  count()

# TODO add top_50_most_abundant_species_park
# TODO by year??

# Combine top species data frames ----
common_names <- CheckEM::australia_life_history %>%
  select(family, genus, species, australian_common_name)

top_species <- bind_rows(top_50_most_abundant_species_overall,
                         top_50_most_abundant_species_bioregion) %>%
  left_join(common_names) %>%
  mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
  glimpse()

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

# Combined data
nsw_bruv_data <- structure(
  list(
    # Dataframes
    bruv_metadata = bruv_metadata,
    overview_stats = overview_stats,
    bioregion_stats = bioregion_stats,
    top_species = top_species,
    metrics = metrics,
    
    # TODO add shapefiles here
    bioregions_shp = bioregions_shp
    
  ), class = "data")

save(nsw_bruv_data, file = here::here("app_data/nsw_bruv_data.Rdata"))

