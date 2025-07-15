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

r = raster(paste0(f2024pred_noloss, "LTE  MIMO Terminal [No Loss] (DL) 800MHz.asc"))
# values(r)[values(r) == -1] = NA
x <- rast(r)


ggplot() + 
  geom_spatraster(data = x) +
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

r = raster(paste0(f2024pred_loss, "LTE  MIMO Terminal [Loss] (DL) 800MHz.asc"))
# values(r)[values(r) == -1] = NA
x <- rast(r)

ggplot() +
  geom_spatraster(data = x) +
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
rr <- as.data.frame(values(r))
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
gb_pbf <- oe_download("https://download.geofabrik.de/europe/great-britain-latest.osm.pbf")
gb_bounds <- oe_read(
  file  = gb_pbf,
  layer = "other_relations",
  query = "
    SELECT *
      FROM other_relations
     WHERE boundary    = 'administrative'
       AND admin_level = '2'
       AND name        = 'United Kingdom'
  "
)

crs(x) <- "EPSG:27700"
uk_gb <- st_transform(gb_bounds, crs = crs(x))

ggplot() +
  geom_spatraster(data = x) +
  geom_sf(data = uk_gb,
          fill         = NA,
          color        = "black",
          size         = 0.6,
          inherit.aes  = FALSE) +
  scale_fill_viridis_c(na.value = 'white') +
  labs(title = "Vodafone Predicted 4G Coverage",
       subtitle = "LTE MIMO Terminal [Loss] (DL) 800MHz",
       caption = "Data: 2024",
       fill = "RSRP (dBm)") +
  theme_void() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  coord_sf()
