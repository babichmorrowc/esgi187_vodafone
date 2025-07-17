library(readr)
library(tidyverse)
library(data.table)
library(sf)

# OSGB36 is the right mapping
# epsg27700

# combined_summary <- read_csv("data/4g-lte-2024-mobile-signal-measurement-data-combined-summary.csv")
combined_summary_45_110 <- read_csv("data/4g-lte-2024-mobile-signal-measurement-data-combined-summary-45-110.csv")

# map coverage just to check
plot(combined_summary_45_110$x, combined_summary_45_110$y)

# How many terrain bins are missing predicted Vodafone PCI?
sum(combined_summary_45_110$voda_pci == -1) / nrow(combined_summary_45_110) # 1.7%

# Add on land use data
# england_wales_landuse <- read_csv("data/LandUsageTB(EnglandWales).csv") %>% 
#   filter(nchar(terrain_bin) == 12) %>% 
#   mutate(easting = substr(terrain_bin, 1, 6),
#          northing = substr(terrain_bin, 7, 12)) %>% 
#   mutate(easting = as.numeric(easting),
#          northing = as.numeric(northing)) %>% 
#   #convert
#   mutate(x = easting + 100,
#          y = northing - 100)

# # Let's map the coverage of the land use dataset
# ggplot(england_wales_landuse, aes(x = easting, y = northing, color = Land_Usage)) +
#   geom_point() +
#   coord_fixed() +
#   labs(title = "Land Use Data Coverage in England and Wales",
#        x = "Easting",
#        y = "Northing") +
#   theme_minimal()
# 
# # Map the coverage of the combined summary dataset
# ggplot(england_wales_landuse, aes(x = easting, y = northing, color = Land_Usage)) +
#   geom_point() +
#   geom_point(data = combined_summary_45_110, aes(x = x, y = y), color = "black") +
#   theme_minimal()
# 
# combined_summary_45_110_landuse <- combined_summary_45_110 %>% 
#   # left_join(england_wales_landuse, by = c("x", "y"))
#   left_join(england_wales_landuse, by = c("x" = "easting",
#                                           "y" = "northing"))
# 
# # Try a join using +/- 100
# # Convert to data.tables
# dt_combined <- as.data.table(combined_summary_45_110)
# dt_landuse <- as.data.table(england_wales_landuse)
# joined <- dt_combined[
#   dt_landuse,
#   on = .(x >= easting - 100, x <= easting + 100,
#          y >= northing - 100, y <= northing + 100),
#   allow.cartesian = TRUE
# ]
# 
# filtered_england_wales <- combined_summary_45_110_landuse %>% 
#   filter(x >= 170000 & x <= 655300,
#          y >= 100000 & y <= 656000)
# 
# # how many cells now have land use?
# sum(!is.na(combined_summary_45_110_landuse$Land_Usage)) / nrow(combined_summary_45_110_landuse) # 5.36%
# 
# # Plot which cells had a successful land use match
# ggplot(combined_summary_45_110_landuse, aes(x = x, y = y)) +
#   geom_point(aes(color = !is.na(Land_Usage))) +
#   coord_fixed() +
#   labs(title = "Land Use Data Matched with Combined Summary",
#        x = "Easting",
#        y = "Northing") +
#   theme_minimal()
# 
# 
# combined_summary_45_110_landuse %>% 
#   group_by(Land_Usage) %>% 
#   summarise(n = n())


# Read in shape file
uk_glx_geodata_townsuburb_combined <- read_sf("data/10k Towns/uk_glx_geodata_townsuburb_combined.shp")
# plot(st_geometry(uk_glx_geodata_townsuburb_combined))

# use the same crs
crs <- st_crs(uk_glx_geodata_townsuburb_combined)
points_sf <- st_as_sf(combined_summary_45_110, coords = c("x", "y"), crs = crs)
# plot(points_sf)

classified_points <- st_join(points_sf, uk_glx_geodata_townsuburb_combined["geog"])

combined_summary_45_110_landuse <- combined_summary_45_110 %>% 
  left_join(select(classified_points, tb, geog), by = "tb") %>% 
  mutate(urban_rural = ifelse(!is.na(geog), "Urban", "Rural"))


ggplot(combined_summary_45_110_landuse, aes(x = x, y = y)) +
  geom_point(aes(color = urban_rural), size = 0.5) +
  coord_fixed() +
  labs(title = "Urban and Rural Classification\nof Ofcom path",
       x = "",
       y = "",
       color = "Land use") +
  theme_minimal()


# Save out urban & rural classification joined to data
dave_4g_lte_2024_mobile_signal_measurement_data_combined_summary_45_110_ext <- read_csv("data/4g-lte-2024-mobile-signal-measurement-data-combined-summary-45-110-ext.csv")
# join on urban rural
dave_4g_lte_2024_mobile_signal_measurement_data_combined_summary_45_110_ext <- dave_4g_lte_2024_mobile_signal_measurement_data_combined_summary_45_110_ext %>% 
  left_join(combined_summary_45_110_landuse %>% select(tb, urban_rural), by = "tb")
write_csv(dave_4g_lte_2024_mobile_signal_measurement_data_combined_summary_45_110_ext,
          "data/4g-lte-2024-mobile-signal-measurement-data-combined-summary-45-110-ext-landuse.csv")
