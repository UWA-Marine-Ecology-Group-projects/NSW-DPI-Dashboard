# Load libraries
library(CheckEM)
library(dplyr)
library(stringr)
library(tidyr)

# Read in data ----
bruv_metadata <- readRDS("data/raw/bruv_metadata_nsw.rds") %>%
  dplyr::select(campaignid, sample, everything())

bruv_count <- readRDS("data/raw/bruv_count_nsw.rds")

bruv_length <- readRDS("data/raw/bruv_length_nsw.rds")

# Create lists of samples to join to complete data ----
count_samples <- bruv_metadata %>%
  filter(successful_count %in% TRUE)

length_samples <- bruv_metadata %>%
  filter(successful_length %in% TRUE)

# Create bioregion lookup ----
bioregions <- bruv_metadata %>%
  select(sample_url, bioregion) %>%
  sf::st_drop_geometry()

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


## Indicator Species -----
# TODO start with top 50 most abundant

# Combine all metrics ----
metrics <- bind_rows(total_abundance_samples, species_richness_samples, cti_samples) %>%
  left_join(bioregions)
