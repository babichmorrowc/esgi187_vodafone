library(readr)
library(readxl)
library(arrow)
library(dbplyr)
library(duckdb)
library(sgo)
library(tidytable)

latlon2tb <- function(lat, lon, bin_length = 25) {
  pts <- sgo_points(list(longitude=lon, latitude=lat), epsg=4326)
  bng.pts <- sgo_lonlat_bng(pts)
  tb.x <- bng.pts$x - bng.pts$x %% bin_length
  tb.y <- bng.pts$y - bng.pts$y %% bin_length
  terrain_bin <- paste(tb.x, tb.y, sep = "")
  return(terrain_bin)
}

lte_2023 <- read_csv(
  "data/Tony_Vernon_2023_Data_Model_and_Input_Files/lte_2023.csv",
  col_select = c(
    "latitude",
    "longitude",
    'pci_top1_vf',
    'power_top1_vf',
    'sinr_top1_vf',
    'rsrp_top1_vf',
    'rsrq_top1_vf',
    'earfcn_top1_vf',
    'add_plmn_top1_vf',
    'mcc_top1_vf',
    'mnc_top1_vf',
    'nr_top1_vf',
    'pci_top2_vf',
    'power_top2_vf',
    'sinr_top2_vf',
    'rsrp_top2_vf',
    'rsrq_top2_vf',
    'earfcn_top2_vf',
    'add_plmn_top2_vf',
    'mcc_top2_vf',
    'mnc_top2_vf',
    'nr_top2_vf',
    'pci_top3_vf',
    'power_top3_vf',
    'sinr_top3_vf',
    'rsrp_top3_vf',
    'rsrq_top3_vf',
    'earfcn_top3_vf',
    'add_plmn_top3_vf',
    'mcc_top3_vf',
    'mnc_top3_vf',
    'nr_top3_vf',
    'pci_top4_vf',
    'power_top4_vf',
    'sinr_top4_vf',
    'rsrp_top4_vf',
    'rsrq_top4_vf',
    'earfcn_top4_vf',
    'add_plmn_top4_vf',
    'mcc_top4_vf',
    'mnc_top4_vf',
    'nr_top4_vf'
  )
)

# Add terrain bin
lte_2023$terrain_bin <- latlon2tb(lte_2023$latitude, lte_2023$longitude)

# Write file out
lte_2023_tb <- write_csv(lte_2023, "data/output/lte_2023_tb.csv")

