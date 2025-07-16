library(raster)
library(terra)
library(tidyterra)
library(ggplot2)
library(osmextract)
library(ggspatial)
library(sf)

setwd("~/OneDrive - University of Dundee/KnowledgeExchange/ESGI-187-Bristol/vodafone/data")


f2024pred_noloss <- "Tony_Vernon_2024 LTE MIMO Terminal [No Loss] (DL) 800MHz/"
f2024pred_loss <- "Tony_Vernon_2024 LTE MIMO Terminal [Loss] (DL) 800MHz/"

r <- rast(paste0(f2024pred_noloss, "LTE  MIMO Terminal [No Loss] (DL) 800MHz.asc"))
r[r == -1] <- NA

ggplot() + 
  geom_spatraster(data = r) +
  scale_fill_viridis_c(na.value = 'white') +
  labs(title = "Vodafone Predicted 4G Coverage",
       subtitle = "LTE MIMO Terminal [No Loss] (DL) 800MHz",
       caption = "Data: 2024",
       fill = "RSRP (dBm)") +
  theme_void() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  coord_equal()

r <- rast(paste0(f2024pred_loss, "LTE  MIMO Terminal [Loss] (DL) 800MHz.asc"))
r[r == -1] <- NA

ggplot() +
  geom_spatraster(data = r) +
  scale_fill_viridis_c(na.value = 'white') +
  labs(title = "Vodafone Predicted 4G Coverage",
       subtitle = "LTE MIMO Terminal [Loss] (DL) 800MHz",
       caption = "Data: 2024",
       fill = "RSRP (dBm)") +
  theme_void() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  coord_equal()


# distribution of raster values
rr <- as_tibble(values(r))
rr_sum <- summary(rr)

rr |> na.omit() |>
  ggplot(aes(value)) +
  geom_density(stat="density", adjust = 3, color = "black", linewidth = 1, linetype = 1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(rr$value, na.rm = TRUE),
                            sd = sd(rr$value, na.rm = TRUE)),
                color = "red",
                linetype = 2,
                linewidth = 1) +
  labs(title = "Smoothing estimate of Vodafone Predicted 4G RSRP",
       subtitle = "LTE MIMO Terminal [Loss] (DL) 800MHz",
       caption = "Data: 2024",
       x = "RSRP (dBm)",
       y = "Density") +
  theme_classic() 

rr |> na.omit() |>
  dplyr::sample_frac(0.05) |>
  ggplot(aes(sample = value)) +
  geom_qq() +
  stat_qq_line() +
  labs(title = "Vodafone Predicted 4G RSRP",
       subtitle = "LTE MIMO Terminal [Loss] (DL) 800MHz",
       caption = "Data: 2024",
       x = "Observed quantiles",
       y = "Theoretical quantiles") +
  theme_classic()

# 
# gb_pbf <- oe_download("https://download.geofabrik.de/europe/great-britain-latest.osm.pbf")
# eur_pbf <- oe_download("https://download.geofabrik.de/europe-latest.osm.pbf")
eur_pbf <- "/Users/ehall001/Library/CloudStorage/OneDrive-UniversityofDundee/KnowledgeExchange/ESGI-187-Bristol/vodafone/data/osm_pbf/europe-latest.osm.pbf"
uk_bound <- oe_read(
  file  = eur_pbf,
  layer = "multipolygons",
  query = "
    SELECT *
      FROM multipolygons
     WHERE boundary    = 'administrative'
       AND admin_level = '2'
       AND name        = 'Great Britain'
  "
)
# 
# crs(r) <- "EPSG:27700"
# uk_bound <- st_transform(uk_bound, crs(r)) # Project the UK boundary to the raster CRS
# 
# ggplot() +
#   geom_spatraster(data = x) +
#   geom_sf(data = uk_gb,
#           fill         = NA,
#           color        = "black",
#           size         = 0.6,
#           inherit.aes  = FALSE) +
#   scale_fill_viridis_c(na.value = 'white') +
#   labs(title = "Vodafone Predicted 4G Coverage",
#        subtitle = "LTE MIMO Terminal [Loss] (DL) 800MHz",
#        caption = "Data: 2024",
#        fill = "RSRP (dBm)") +
#   theme_void() +
#   theme(legend.position = "bottom", 
#         plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5)) + 
#   coord_sf()

library(rnaturalearth)
uk_ne <- ne_countries(country = "United Kingdom", returnclass = "sf")
crs(r) <- "EPSG:27700"
uk_ne <- st_transform(uk_ne, crs(r))

ggplot() +
  geom_spatraster(data = r) +
  geom_sf(data       = uk_ne,
          fill       = NA,
          color      = "black",
          size       = 0.6,
          inherit.aes = FALSE) +
  scale_fill_viridis_c(na.value = 'white') +
  labs(title    = "Vodafone Predicted 4G Coverage (UK)",
       subtitle = "LTE MIMO Terminal [Loss] (DL) 800 MHz",
       caption  = "Data: 2024",
       fill     = "RSRP (dBm)") +
  coord_sf(datum = NA) +
  theme_void() +
  theme(legend.position   = "bottom",
        plot.title        = element_text(hjust = 0.5),
        plot.subtitle     = element_text(hjust = 0.5))

## Observations

library(readr)
obs <- read_csv("lte_2023_tb.csv")

obs_long <- obs |> 
  # head(n=10) |>
  select(latitude, longitude, terrain_bin, contains('rsrp'), contains('pci'), contains('earfcn')) |>
  pivot_longer(
    # pivot all the *_top[1-4]_vf columns
    cols = matches("^(rsrp|pci|earfcn)_(top[1-4])_vf$"),
    # .value will become the metric names (rsrp, pci, earfcn)
    # category will be top1, top2, top3 or top4
    names_to = c(".value", "category"),
    names_pattern = "(.*)_(top[1-4])_vf"
  )
rm(obs)

obs_long$category <- factor(obs_long$category)
obs_long$pci <- factor(obs_long$pci)
obs_long$earfcn <- factor(obs_long$earfcn)

obs_long <- obs_long |> 
  filter(earfcn == "6300")

obs_long2 <- obs_long |> 
  # head(n=100) |>
  group_by(terrain_bin) |>
  slice_max(order_by = rsrp, n = 1, with_ties = FALSE) |>
  ungroup()

obs_long2 |> 
  na.omit() |> 
  summarise(mean_rsrp = mean(rsrp, na.rm = TRUE), n = dplyr::n())

obs_long2 |>
  ggplot(aes(rsrp)) +
  geom_density(stat="density", adjust = 3, color = "black", linewidth = 1, linetype = 1) + 
  stat_function(fun = dnorm,
                args = list(mean = mean(obs_long2$rsrp, na.rm = TRUE),
                            sd = sd(obs_long2$rsrp, na.rm = TRUE)),
                color = "red",
                linetype = 2,
                linewidth = 1) +
  labs(title = "Smoothing estimate of Ofcom 4G Observations",
       subtitle = "RSRP for EARFCN = 6300 (strongest PCI only), n = 582585",
       caption = "Data: 2023",
       x = "RSRP (dBm)",
       y = "Density") +
  theme_classic() 

# 
library(arrow)
library(feather)
pred2024 <- read_feather("~/Library/CloudStorage/OneDrive-UniversityofDundee/KnowledgeExchange/ESGI-187-Bristol/vodafone/data/coordinates_Predicted_BestPCIs.feather")
obs2024 <- read_feather("~/Library/CloudStorage/OneDrive-UniversityofDundee/KnowledgeExchange/ESGI-187-Bristol/vodafone/data/4g-lte-2024-mobile-signal-measurement-data.arrow")

