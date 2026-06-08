# install.packages('remotes')
# library('remotes')
# options(timeout=9999999)
# 
# remotes::install_github("GlobalArchiveManual/CheckEM")

library(CheckEM)
library(dplyr)
library(httr)
library(stringr)

# Set your API token to access GlobalArchive data shared with you ----
CheckEM::ga_api_set_token()

# Load the saved token
token <- readRDS("secrets/api_token.RDS")

# Load the metadata, count and length ----
CheckEM::ga_api_all_data(synthesis_id = "19",
                         token = token,
                         dir = "data/raw/",
                         include_zeros = FALSE)

# TODO - I think a spatial join would be best to do this so that I make sure I get all campaigns

bioregions <- sf::st_read("data/spatial/Marine_Bioregions.shp") %>% clean_names()
plot(bioregions)

bioregions <- st_transform(bioregions, 4326)

# ## Load in data again to save time ----
bruv_metadata <- readRDS("data/raw/metadata.RDS") 

bruv_metadata_sf <- bruv_metadata %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326)

bruv_metadata_nsw <- st_join(bruv_metadata_sf, bioregions) %>%
  dplyr::filter(!is.na(bioregion)) %>%
  glimpse()

unique(bruv_metadata_nsw$bioregion)

# TODO check over this list with Matt and Nath
unique(bruv_metadata_nsw$campaignid) %>% sort()

plot(bruv_metadata$longitude_dd, bruv_metadata$latitude_dd)
plot(bruv_metadata_nsw$longitude_dd, bruv_metadata_nsw$latitude_dd)

bruv_count_nsw <- readRDS("data/raw/count.RDS") %>% 
  semi_join(., bruv_metadata_nsw)

bruv_length_nsw <- readRDS("data/raw/length.RDS") %>% 
  semi_join(., bruv_metadata_nsw)

# TODO potentially add these in later

# benthos <- readRDS("data/raw/benthos_summarised.RDS") %>%
#   filter(campaignid %in% c(campaign_list))
# 
# relief <- readRDS("data/raw/relief_summarised.RDS") %>%
#   filter(campaignid %in% c(campaign_list))

# Save data for analysis ----
saveRDS(bruv_metadata_nsw, "data/raw/bruv_metadata_nsw.rds")
saveRDS(bruv_count_nsw, "data/raw/bruv_count_nsw.rds")
saveRDS(bruv_length_nsw, "data/raw/bruv_length_nsw.rds")
