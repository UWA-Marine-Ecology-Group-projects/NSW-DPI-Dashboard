library(tidytext)

sti <- CheckEM::australia_life_history %>%
  clean_names() %>%
  dplyr::select(family, genus, species, rls_thermal_niche) %>%
  mutate(scientific = paste(family, genus, species, sep = " ")) %>%
  dplyr::distinct() %>%
  glimpse()


# Thermal Index stacked plot
cti.10 <- complete_bruv_count %>%
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
  dplyr::filter(bioregion %in% "Tweed-Moreton") %>%
  glimpse()

# num_years <- cti.10 %>%
#   distinct(bioregion,  year) %>%
#   group_by(bioregion) %>%
#   count()
# 
# test <- complete_bruv_count %>%
#   left_join(count_samples) %>%
#   dplyr::filter(bioregion %in% "Batemans Shelf") %>%
#   filter(year %in% "2019")
# 
# unique(test$sample_url)

# Will need to do this on server to find unique species

# sp.cti.y1 <- cti.10 %>% filter(year == years[1]) %>% pull(scientific)
# sp.cti.y2 <- cti.10 %>% filter(year == years[2]) %>% pull(scientific)
# 
# unique_species_cti <- union(
#   setdiff(sp.cti.y1, sp.cti.y2),
#   setdiff(sp.cti.y2, sp.cti.y1))

# Have put in the helpers.R
log1p10_trans <- trans_new(
  name = "log10p1",
  transform = function(x) log10(x + 1),
  inverse   = function(x) 10^x - 1
)

# choose the centering statistic
mid_niche <- median(cti.10$rls_thermal_niche, na.rm = TRUE)

# global limits across both facets/years
niche_limits <- range(cti.10$rls_thermal_niche, na.rm = TRUE)

bar_cti <- ggplot(
  cti.10,
  aes(
    x = reorder_within(scientific_label, rls_thermal_niche, year),
    y = maxn,
    fill = rls_thermal_niche
  )
) +
  geom_col(colour = "black", linewidth = 0.25) +
  geom_errorbar(
    aes(
      ymin = pmax(maxn - se, 0),
      ymax = maxn + se
    ),
    width = 0.2
  ) +
  geom_text(aes(y = 23, label = niche_lab), hjust = 0, size = 3) +
  coord_flip(clip = "off") +
  facet_wrap(~year, scales = "free_y") +
  scale_x_reordered() +
  scale_y_continuous(
    trans = log1p10_trans,
    expand = expansion(mult = c(0, 0.15)),
    breaks = c(0, 5, 10, 20, 30),
    labels = scales::label_number()
  ) +
  # centre GREY at the mean thermal niche
  scale_fill_gradientn(
    colours = c(
      "#2166ac",
      "#67a9cf",
      "#d1e5f0",
      "#fddbc7",
      "#ef8a62",
      "#b2182b"
      
    ),
    values  = scales::rescale(c(niche_limits[1],
                                mid_niche,
                                niche_limits[2])),
    limits = niche_limits,
    na.value = "grey80"
  ) +
  guides(fill = "none") +
  labs(
    x = "Species",
    y = expression(Log[10]~(Average~abundance~+~1))
  ) +
  theme_bw()

bar_cti

