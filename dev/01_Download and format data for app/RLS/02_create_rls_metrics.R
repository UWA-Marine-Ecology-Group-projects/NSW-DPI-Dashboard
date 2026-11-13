#################################################################
# 02 Create RLS / ATRC (Dive) metrics for the NSW dashboard
#
# Uses the tidy data made in 01_format_rls_data.R and creates the SAME
# metrics and dataframes as the BRUV script
# (dev/01_Download and format data for app/02_Create Metrics.R), so the
# app can swap between them with the BRUVS / Dive switch.
#
# The saved list (nsw_rls_data) uses the same element and column names as
# nsw_bruv_data. For Dive data:
#   - sample_url = survey_id (one RLS transect)
#   - campaignid = program (RLS or ATRC)
#   - sample     = site name
#   - "bruv_metadata" holds the transect metadata (name kept so the app
#     code works for both methods)
#
# Decisions (checked with Brooke 13/11/2026):
#   - Each metric is calculated per block, then the blocks are averaged to
#     give one value per transect (like DEW). For abundance and biomass this
#     is the transect total divided by the number of blocks
#   - Biomass uses the RLS biomass column (grams)
#   - Size thresholds use size_class > 20 cm and > 30 cm (same as BRUV > 200 mm)
#   - ALT/BLT: same large-bodied carnivore list as BRUVs, fish with
#     size_class (in mm) > length at maturity
#   - Species richness drops a genus "spp" record in a block when a named
#     species of the same genus is also in that block (DEW rule, now also
#     used for BRUVs)
#   - CTI uses CheckEM::create_cti() for each block, then blocks averaged
#################################################################

# Load libraries ----
library(CheckEM)
library(tidyverse)
library(sf)
library(vegan)
library(here)

# Read in data ----
rls_surveys <- read_rds("data/tidy/rls_surveys.rds")
rls_blocks  <- read_rds("data/tidy/rls_blocks.rds")
rls_fish    <- read_rds("data/tidy/rls_fish.rds")

common_names <- CheckEM::australia_life_history %>%
  select(family, genus, species, australian_common_name)

# Metadata in the same format as the BRUV metadata ----
rls_metadata <- rls_surveys %>%
  mutate(
    sample_url = survey_id,
    campaignid = program,
    sample     = site_name
  ) %>%
  glimpse()

# Number of blocks in each transect (most have 2, some have 1)
n_blocks <- rls_blocks %>%
  count(survey_id, name = "n_blocks")

# Abundance of each species in each block (summed over size classes)
block_species <- rls_fish %>%
  group_by(survey_id, block, family, genus, species, scientific) %>%
  summarise(count = sum(total), .groups = "drop")

# Functions to average blocks ----
# Abundance and biomass: the mean of the blocks is the transect total divided
# by the number of blocks. Transects with nothing recorded get a zero.
average_blocks <- function(data, metric_name) {
  data %>%
    group_by(survey_id) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
    right_join(n_blocks, by = "survey_id") %>%
    mutate(
      value  = replace_na(total, 0) / n_blocks,
      metric = metric_name
    ) %>%
    select(survey_id, metric, value)
}

# Metrics that are calculated per block (e.g. species richness): take the
# mean of the block values. Blocks with no fish get a zero.
mean_of_blocks <- function(block_values, metric_name) {
  block_values %>%
    right_join(rls_blocks, by = c("survey_id", "block")) %>%
    mutate(value = replace_na(value, 0)) %>%
    group_by(survey_id) %>%
    summarise(value = mean(value), .groups = "drop") %>%
    mutate(metric = metric_name)
}

# Calculate Metrics ----
## Total abundance ----
total_abundance_samples <- rls_fish %>%
  transmute(survey_id, value = total) %>%
  average_blocks("total_abundance")

## Species richness ----
species_richness_blocks <- block_species %>%
  filter(count > 0) %>%
  group_by(survey_id, block, family, genus) %>%
  filter(!(species == "spp" & any(species != "spp"))) %>% # drop genus spp if a named species of that genus is in the block
  ungroup() %>%
  count(survey_id, block, name = "value")

species_richness_samples <- mean_of_blocks(species_richness_blocks, "species_richness")

## CTI ----
# create_cti() is given one block at a time (campaignid = transect, sample = block)
cti_blocks <- block_species %>%
  mutate(
    campaignid = survey_id,
    sample     = as.character(block),
    sample_url = paste(survey_id, block, sep = "_")
  ) %>%
  CheckEM::create_cti()

names(cti_blocks) # should include campaignid, sample and cti

# Blocks without any species that have a thermal niche have no CTI (not zero), so they are left out
cti_samples <- cti_blocks %>%
  filter(!is.na(cti)) %>%
  group_by(survey_id = campaignid) %>%
  summarise(value = mean(cti), .groups = "drop") %>%
  mutate(metric = "cti")

# Transects with fish but no CTI (species without a thermal niche value)
anti_join(total_abundance_samples, cti_samples, by = "survey_id") %>%
  filter(value > 0)

## Large bodied carnivores (ALT and BLT) ----
# Same species list as the BRUV script
large_bodied_carnivores <- CheckEM::australia_life_history %>%
  filter(fb_trophic_level > 2.8) %>%
  filter(length_max_cm > 40) %>%
  filter(class %in% "Actinopterygii") %>%
  filter(!order %in% c("Anguilliformes", "Ophidiiformes", "Notacanthiformes", "Syngnathiformes",
                       "Synbranchiformes", "Stomiiformes", "Siluriformes", "Saccopharyngiformes", "Osmeriformes",
                       "Osteoglossiformes", "Lophiiformes", "Lampriformes", "Beloniformes")) %>%
  mutate(l50 = fb_length_at_maturity_cm * 10) %>% # mm
  filter(!is.na(l50)) %>%
  select(family, genus, species, l50)

lbc_fish <- rls_fish %>%
  inner_join(large_bodied_carnivores, by = c("family", "genus", "species")) %>%
  filter(size_class * 10 > l50) # size class is in cm, l50 in mm

# Large bodied carnivores in the RLS data
lbc_fish %>% distinct(family, genus, species)

blt_samples <- lbc_fish %>%
  transmute(survey_id, value = biomass / 1000) %>% # kg
  average_blocks("blt")

alt_samples <- lbc_fish %>%
  transmute(survey_id, value = total) %>%
  average_blocks("alt")

## Abundance and biomass bigger than 20 and 30 cm ----
fish_bigger_20 <- rls_fish %>% filter(size_class > 20)
fish_bigger_30 <- rls_fish %>% filter(size_class > 30)

b20_samples <- fish_bigger_20 %>% transmute(survey_id, value = biomass / 1000) %>% average_blocks("b20")
a20_samples <- fish_bigger_20 %>% transmute(survey_id, value = total) %>% average_blocks("a20")
b30_samples <- fish_bigger_30 %>% transmute(survey_id, value = biomass / 1000) %>% average_blocks("b30")
a30_samples <- fish_bigger_30 %>% transmute(survey_id, value = total) %>% average_blocks("a30")

# Combine all metrics ----
metrics <- bind_rows(
  total_abundance_samples,
  species_richness_samples,
  cti_samples,
  blt_samples,
  alt_samples,
  b20_samples,
  a20_samples,
  b30_samples,
  a30_samples
) %>%
  left_join(
    rls_metadata %>% select(survey_id, campaignid, sample, sample_url, bioregion, status, date, year),
    by = "survey_id"
  ) %>%
  select(campaignid, sample, sample_url, metric, value, bioregion, status, date, year)

# Every metric has one value per transect (CTI can have fewer)
metrics %>% count(metric)

metrics_mean_se <- metrics %>%
  group_by(metric, bioregion, year, status) %>%
  summarise(
    n    = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    se   = if_else(n > 1, sd(value, na.rm = TRUE) / sqrt(n), 0),
    .groups = "drop"
  )

write_csv(metrics_mean_se, "outputs/rls_metrics_mean_se.csv")

# Top 10 species diagnostics for a20, b20, a30, b30, alt and blt ----
# Average per transect for each species (zeros where it wasn't seen), then the
# top 10 species in each bioregion and year, then the mean and SE by status
top_10_by_status <- function(fish_subset, metric_name, use_biomass = FALSE) {

  species_values <- fish_subset %>%
    mutate(value = if (use_biomass) biomass / 1000 else total) %>%
    group_by(survey_id, family, genus, species, scientific) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    complete(survey_id = rls_surveys$survey_id, nesting(family, genus, species, scientific), fill = list(value = 0)) %>%
    left_join(n_blocks, by = "survey_id") %>%
    mutate(value = value / n_blocks) %>%
    left_join(rls_metadata %>% select(survey_id, bioregion, year, status), by = "survey_id")

  top_10 <- species_values %>%
    group_by(bioregion, year, scientific) %>%
    summarise(average = mean(value), .groups = "drop") %>%
    group_by(bioregion, year) %>%
    slice_max(average, n = 10, with_ties = FALSE) %>%
    ungroup() %>%
    distinct(bioregion, year, scientific)

  species_values %>%
    semi_join(top_10, by = c("bioregion", "year", "scientific")) %>%
    group_by(bioregion, year, status, scientific, family, genus, species) %>%
    summarise(
      se    = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
      value = mean(value),
      .groups = "drop"
    ) %>%
    mutate(metric = metric_name)
}

top_10_diagnostic_a_and_b <- bind_rows(
  top_10_by_status(fish_bigger_20, "a20"),
  top_10_by_status(fish_bigger_20, "b20", use_biomass = TRUE),
  top_10_by_status(fish_bigger_30, "a30"),
  top_10_by_status(fish_bigger_30, "b30", use_biomass = TRUE),
  top_10_by_status(lbc_fish, "alt"),
  top_10_by_status(lbc_fish, "blt", use_biomass = TRUE)
) %>%
  select(bioregion, year, status, scientific, family, genus, species, metric, value, se) %>%
  left_join(common_names, by = c("family", "genus", "species")) %>%
  glimpse()

## Indicator Species and most abundant species -----
# Average abundance of every species in every transect (mean of blocks, zeros included)
complete_rls_count <- block_species %>%
  group_by(survey_id, family, genus, species) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  complete(survey_id = rls_surveys$survey_id, nesting(family, genus, species), fill = list(count = 0)) %>%
  left_join(n_blocks, by = "survey_id") %>%
  mutate(count = count / n_blocks) %>%
  left_join(rls_metadata %>% select(survey_id, sample_url, bioregion, status, year), by = "survey_id") %>%
  select(sample_url, family, genus, species, count, bioregion, status, year)

nrow(complete_rls_count) == nrow(rls_surveys) * nrow(distinct(block_species, family, genus, species))

top_50_most_abundant_species_overall <- complete_rls_count %>%
  group_by(family, genus, species) %>%
  summarise(
    average_abundance = mean(count),
    se = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count))),
    .groups = "drop"
  ) %>%
  arrange(-average_abundance) %>%
  slice_head(n = 50) %>%
  mutate(group = "overall", by_status = FALSE)

top_50_most_abundant_species_bioregion <- complete_rls_count %>%
  group_by(bioregion, family, genus, species) %>%
  summarise(
    average_abundance = mean(count),
    se = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count))),
    .groups = "drop"
  ) %>%
  group_by(bioregion) %>%
  arrange(desc(average_abundance), .by_group = TRUE) %>%
  slice_head(n = 50) %>%
  ungroup() %>%
  mutate(group = "bioregion", by_status = FALSE)

top_50_species_overall <- top_50_most_abundant_species_overall %>%
  distinct(family, genus, species)

top_50_most_abundant_species_overall_status <- complete_rls_count %>%
  group_by(status, family, genus, species) %>%
  summarise(
    average_abundance = mean(count),
    se = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count))),
    .groups = "drop"
  ) %>%
  semi_join(top_50_species_overall, by = c("family", "genus", "species")) %>%
  mutate(group = "overall", by_status = TRUE)

top_50_species_bioregions <- top_50_most_abundant_species_bioregion %>%
  distinct(bioregion, family, genus, species)

top_50_most_abundant_species_bioregion_status <- complete_rls_count %>%
  group_by(bioregion, status, family, genus, species) %>%
  summarise(
    average_abundance = mean(count),
    se = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count))),
    .groups = "drop"
  ) %>%
  semi_join(top_50_species_bioregions, by = c("bioregion", "family", "genus", "species")) %>%
  mutate(group = "bioregion", by_status = TRUE)

# Same labels as the BRUV script
add_species_labels <- function(data) {
  data %>%
    left_join(common_names, by = c("family", "genus", "species")) %>%
    mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")")) %>%
    tidyr::extract(display_name, into = c("sci", "common"), regex = "^(.*?)\\s*\\((.*?)\\)$", remove = FALSE) %>%
    mutate(label = paste0("*", sci, "*<br>(", common, ")"))
}

top_50_most_abundant_species_bioregion_status_year <- complete_rls_count %>%
  group_by(bioregion, status, year, family, genus, species) %>%
  summarise(
    average_abundance = mean(count),
    se = sd(count, na.rm = TRUE) / sqrt(sum(!is.na(count))),
    .groups = "drop"
  ) %>%
  semi_join(top_50_species_bioregions, by = c("bioregion", "family", "genus", "species")) %>%
  mutate(group = "bioregion", by_status = TRUE) %>%
  add_species_labels() %>%
  glimpse()

top_species <- bind_rows(
  top_50_most_abundant_species_overall,
  top_50_most_abundant_species_bioregion,
  top_50_most_abundant_species_overall_status,
  top_50_most_abundant_species_bioregion_status
) %>%
  add_species_labels() %>%
  glimpse()

# Quick stats for overview page ----
# num_bruvs = number of transects (name kept so the app works for both methods)
# num_lengths = number of fish given a size class
sized_fish <- rls_fish %>% filter(size_class > 0)

overview_stats <- tibble(
  num_bruvs    = nrow(rls_metadata),
  num_fish     = sum(rls_fish$total, na.rm = TRUE),
  num_lengths  = sum(sized_fish$total, na.rm = TRUE),
  biggest_fish = max(rls_fish$size_class, na.rm = TRUE) * 10, # mm
  min_year     = min(rls_metadata$year, na.rm = TRUE),
  max_year     = max(rls_metadata$year, na.rm = TRUE),
  min_depth    = min(rls_metadata$depth_m, na.rm = TRUE),
  max_depth    = max(rls_metadata$depth_m, na.rm = TRUE)
)

bioregion_metadata_stats <- rls_metadata %>%
  group_by(bioregion) %>%
  summarise(
    num_bruvs = n(),
    min_year  = min(year, na.rm = TRUE),
    max_year  = max(year, na.rm = TRUE),
    min_depth = min(depth_m, na.rm = TRUE),
    max_depth = max(depth_m, na.rm = TRUE)
  )

bioregion_count_stats <- rls_fish %>%
  left_join(rls_metadata %>% select(survey_id, bioregion), by = "survey_id") %>%
  group_by(bioregion) %>%
  summarise(num_fish = sum(total, na.rm = TRUE))

bioregion_length_stats <- sized_fish %>%
  left_join(rls_metadata %>% select(survey_id, bioregion), by = "survey_id") %>%
  group_by(bioregion) %>%
  summarise(
    num_lengths  = sum(total, na.rm = TRUE),
    biggest_fish = max(size_class, na.rm = TRUE) * 10
  )

bioregion_stats <- bioregion_metadata_stats %>%
  left_join(bioregion_count_stats, by = "bioregion") %>%
  left_join(bioregion_length_stats, by = "bioregion")

bioregion_stats

# CTI top 10 (diagnostic plot) ----
sti <- CheckEM::australia_life_history %>%
  clean_names() %>%
  select(family, genus, species, rls_thermal_niche) %>%
  mutate(scientific = paste(family, genus, species, sep = " ")) %>%
  distinct()

cti_top_10 <- complete_rls_count %>%
  mutate(scientific = paste(family, genus, species, sep = " ")) %>%
  group_by(bioregion, year, scientific) %>%
  summarise(
    n    = sum(!is.na(count)),
    maxn = mean(count, na.rm = TRUE), # average abundance per transect (name kept for the app)
    se   = sd(count, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  left_join(sti, by = "scientific") %>%
  filter(!is.na(rls_thermal_niche)) %>%
  group_by(bioregion, year) %>%
  slice_max(order_by = maxn, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  add_species_labels() %>%
  mutate(niche_lab = scales::number(rls_thermal_niche, accuracy = 0.01)) %>%
  glimpse()

# Species abundance for the "Choose a species" tab ----
top_50_abundance <- complete_rls_count %>%
  semi_join(top_50_species_bioregions, by = c("bioregion", "family", "genus", "species")) %>%
  left_join(rls_metadata %>% select(sample_url, campaignid, sample, date), by = "sample_url") %>%
  left_join(common_names, by = c("family", "genus", "species")) %>%
  mutate(display_name = paste0(genus, " ", species, " (", australian_common_name, ")"))

# Species accumulation curves ----
# Same as the BRUV script, using transects instead of BRUV deployments
sac_sample_info <- rls_metadata %>%
  select(sample_url, sample, campaignid, bioregion, status, year) %>%
  filter(!is.na(bioregion), !is.na(status), !is.na(year)) %>%
  distinct()

sac_species_wide <- block_species %>%
  mutate(scientific_name = paste(family, genus, species)) %>%
  group_by(sample_url = survey_id, scientific_name) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  pivot_wider(names_from = scientific_name, values_from = count, values_fill = 0)

species_cols <- setdiff(names(sac_species_wide), "sample_url")

# Join back to ALL transects so transects without fish still count
sac_wide <- sac_sample_info %>%
  left_join(sac_species_wide, by = "sample_url") %>%
  mutate(across(all_of(species_cols), ~ replace_na(.x, 0))) %>%
  arrange(bioregion, year, status, sample_url)

make_species_accumulation <- function(df, species_cols) {

  species_mat <- df %>%
    select(all_of(species_cols)) %>%
    as.data.frame()

  if (nrow(species_mat) == 0 || ncol(species_mat) == 0) {
    return(tibble(deployments = numeric(), richness = numeric(), sd = numeric()))
  }

  # Only one transect in this group: specaccum() can't make a curve, so
  # return a single point (the number of species on that transect)
  if (nrow(species_mat) == 1) {
    return(tibble(deployments = 1, richness = sum(species_mat > 0), sd = 0))
  }

  species_pa <- vegan::decostand(species_mat, method = "pa")

  sac <- vegan::specaccum(species_pa, method = "random", permutations = 999)

  tibble(
    deployments = sac$sites, # number of transects (name kept for the app)
    richness = sac$richness,
    sd = sac$sd
  )
}

set.seed(123)

species_accumulation <- sac_wide %>%
  group_by(bioregion, year, status) %>%
  group_modify(~ make_species_accumulation(df = .x, species_cols = species_cols)) %>%
  ungroup() %>%
  mutate(
    year  = as.character(year),
    lower = pmax(richness - replace_na(sd, 0), 0),
    upper = richness + replace_na(sd, 0)
  ) %>%
  glimpse()

# Species length data for dashboard ----
# RLS sizes are size classes (cm), converted to mm so the app can use length_mm
species_length_data <- sized_fish %>%
  left_join(rls_metadata %>% select(survey_id, sample_url, bioregion, year, status), by = "survey_id") %>%
  mutate(length_mm = size_class * 10) %>%
  select(sample_url, bioregion, year, status, family, genus, species, length_mm, count = total) %>%
  left_join(common_names, by = c("family", "genus", "species")) %>%
  mutate(
    display_name = if_else(
      is.na(australian_common_name),
      paste(genus, species),
      paste0(genus, " ", species, " (", australian_common_name, ")")
    )
  ) %>%
  glimpse()

# Bioregion shapefile ----
bioregions_shp <- st_read("data/spatial/Marine_Bioregions.shp") %>%
  clean_names() %>%
  st_transform(4326)

# Combined data (same names as nsw_bruv_data) ----
nsw_rls_data <- structure(
  list(
    # Dataframes
    bruv_metadata = rls_metadata, # transect metadata (name kept for the app)
    overview_stats = overview_stats,
    bioregion_stats = bioregion_stats,
    top_species = top_species,
    top_50_most_abundant_species_bioregion_status_year = top_50_most_abundant_species_bioregion_status_year,
    metrics = metrics,

    # Diagnostic data
    species_accumulation = species_accumulation,
    cti_top_10 = cti_top_10,
    top_10_diagnostic_a_and_b = top_10_diagnostic_a_and_b,

    bioregions_shp = bioregions_shp,

    top_50_abundance = top_50_abundance,

    species_length_data = species_length_data
  ), class = "data")

save(nsw_rls_data, file = here::here("app_data/nsw_rls_data.Rdata"))
