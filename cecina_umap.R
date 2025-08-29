# Run UMAP on the data
library(readr)
library(tidyverse)
library(uwot)

lte_2024 <- read_csv("data/4g-lte-2024-mobile-signal-measurement-data-combined-summary-45-110-ext-landuse-sites.csv")

lte_2024 <- lte_2024 |> 
  mutate(
    err_match1_mean = voda_rsrp - ofcom_rsrp_match_1_mean,
    err_match_mean =  voda_rsrp - ofcom_rsrp_match_mean,
    err_all_mean   =  voda_rsrp - ofcom_rsrp_all_mean,
    range_all      = ofcom_rsrp_all_max - ofcom_rsrp_all_min, 
    pci_match      = ifelse(voda_pci == ofcom_pci_mode, 1, -1),
    ofcom_pci_zero = ifelse(ofcom_pci_mode == 0, 1, -1),
    urban_match    = ifelse(urban_rural == "Urban", 1, -1)
  ) |>
  rename(
    site_dist = `lte_comb$site_dist`
  )

pal <- c("Mismatch" = "#FC4E07", "Match" = "#00AFBB")
groups <- factor(
  lte_2024$pci_match,
  levels = c(-1, 1),
  labels = c("Mismatch","Match")
)

# select vars for UMAP
umap_df <- lte_2024 |>
  select(
    # err_match1_mean,           # response variable
    # err_all_mean,
    ofcom_pci_zero,
    ofcom_pci_mode_count,   
    ofcom_match_1_count,
    ofcom_all_count,
    range_all,
    pci_match,
    urban_match,
    site_dist,
    x,
    y
  ) |>
  # remove NAs
  filter(!is.na(site_dist)) |>
  scale()

res.umap <- uwot::umap(umap_df, n_neighbors = 15, n_components = 2, verbose = TRUE)
# default confguration:
# n_neighbors = 15, n_components = 2

# See UMAP loadings


# Visualize UMAP
umap_df <- as.data.frame(res.umap)
colnames(umap_df) <- c("UMAP1", "UMAP2")
umap_df$error <- lte_2024$err_all_mean[!is.na(lte_2024$site_dist)]
umap_df$pci_match <- lte_2024$pci_match[!is.na(lte_2024$site_dist)]
ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = pci_match)) +
  geom_point(alpha = 0.7, size = 1.5) +
  theme_minimal() +
  labs(title = "UMAP Projection", color = "Mean error") +
  theme(legend.position = "right")


