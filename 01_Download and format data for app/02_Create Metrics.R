# Load libraries
library(CheckEM)
library(dplyr)
library(stringr)
library(tidyr)

# Read in data ----
bruv_metadata <- readRDS("data/raw/bruv_metadata_nsw.rds")
bruv_count <- readRDS("data/raw/bruv_count_nsw.rds")
bruv_length <- readRDS("data/raw/bruv_length_nsw.rds")

# Create lists of samples to join to complete data ----
count_samples <- bruv_metadata %>%
  filter(successful_count %in% TRUE)

length_samples <- bruv_metadata %>%
  filter(successful_length %in% TRUE)

# Calculate Metrics ----
# Total Abundance
# Species Richness
# CTI
# BLT 
# Indicator species 

## Total Abundance ----
# TODO Make sure that all species are fish!!
# TODO Check if this should include spps

total_abundance_samples <- bruv_count %>%
  dplyr::group_by(sample_url) %>%
  dplyr::summarise(value = sum(count)) %>%
  dplyr::full_join(count_samples) %>%
  replace_na(list(value = 0)) %>%
  dplyr::mutate(metric = "total_abundance") %>%
  dplyr::select(sample_url, metric, value)

## Species richness ----
# TODO Make sure that all species are fish!!

species_richness_samples <- bruv_count %>%
  distinct(sample_url, family, genus, species) %>%
  dplyr::group_by(sample_url) %>%
  dplyr::summarise(value = n()) %>%
  dplyr::full_join(count_samples) %>%
  replace_na(list(value = 0)) %>%
  dplyr::mutate(metric = "species_richness") %>%
  dplyr::select(sample_url, metric)

# CTI ----



# BLT ----



# Indicator Species -----
# TODO start with top 50 most abundant

# All metrics ----
metrics <- bind_rows(total_abundance_samples, species_richness_samples)
