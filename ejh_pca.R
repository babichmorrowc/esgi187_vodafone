library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(GGally)
library(corrplot)

# lte_2024 <- read_csv("~/OneDrive - University of Dundee/KnowledgeExchange/ESGI-187-Bristol/vodafone/data/4g-lte-2024-mobile-signal-measurement-data-combined-summary.csv")
lte_2024 <- read_csv("~/OneDrive - University of Dundee/KnowledgeExchange/ESGI-187-Bristol/vodafone/data/4g-lte-2024-mobile-signal-measurement-data-combined-summary-45-110-ext-landuse-sites.csv")

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

lte_2024 |> 
  select(voda_rsrp, ofcom_rsrp_match_mean, ofcom_rsrp_all_mean, ofcom_rsrp_match_1_mean) |>
  pivot_longer(
    everything(),
    names_to  = "Source",
    values_to = "RSRP"
  ) |>
  mutate(
    Source = recode(
      Source,
      voda_rsrp               = "Vodafone",
      ofcom_rsrp_match_mean   = "Ofcom (matched)",
      ofcom_rsrp_match_1_mean = "Ofcom (top 1)",
      ofcom_rsrp_all_mean     = "Ofcom (all)",
    )
  ) |>
  ggplot(aes(x = RSRP, colour = Source, linetype = Source)) +
  geom_density() +
  labs(title = "UE compatible 4G RSRP",
       x = "RSRP (dBm)",
       y = "Density",
       caption = "Data: 2024 (UE-compatible 4G)") +
  theme_classic() +
  theme(legend.position = "bottom")

lte_2024 |> group_by(pci_match) |>
  summarise(count = n())

lte_2024 |> 
  select(contains("err")) |>
  pivot_longer(
    everything(),
    names_to  = "Type",
    values_to = "Error"
  ) |>
  mutate(
    Type = recode(
      Type,
      err_match1_mean   = "Top 1",
      err_match_mean = "Matched",
      err_all_mean    = "All",
    )
  ) |>
  ggplot(aes(x = Error, colour = Type, linetype = Type)) +
  geom_density() +
  labs(title = "Mean error for different TB averaging",
       x = "Error (dBm)",
       y = "Density") +
  theme_classic() 

lte_2024 |> 
  summarise(mean = mean(err_match1_mean, na.rm = TRUE))

# select vars for PCA
pca_df <- lte_2024 |>
  select(
    err_match1_mean,           # response variable
    err_all_mean,
    # ofcom_pci_zero,
    ofcom_pci_mode_count,   
    ofcom_match_1_count,
    ofcom_all_count,
    range_all,
    pci_match,
    urban_match,
    # site_dist,
    # x,
    # y,
  )

res.pca <- PCA(
  X      = pca_df,
  scale.unit = TRUE,   # centre and scale each variable
  ncp    = 5,          # number of PCs to keep
  graph  = FALSE       # don’t draw default graphs
)

# Scree plot of eigenvalues: how much variance each PC explains
fviz_eig(
  res.pca,
  addlabels = TRUE,    # show % values on bars
  ylim      = c(0, 100)
)

# Variable map: see how each variable loads on PC1 vs PC2
fviz_pca_var(
  res.pca,
  col.var    = "contrib", # colour by contribution to PCs
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel      = TRUE       # avoid label overlap
)

# Individual map: plot observations in PC space, coloured by pci_match (converted to factor here for discrete groups)
fviz_pca_ind(
  res.pca,
  geom.ind     = "point",
  habillage    = factor(lte_2024$pci_match, labels = c("Mismatch","Match")),
  addEllipses  = TRUE,    # confidence ellipses around groups
  palette      = pal
)

# Contribution of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 6, ylim = c(0, 70)) +
  ggtitle("Top Contributors to PC1")

# Contribution of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 6, ylim = c(0, 70)) +
  ggtitle("Top Contributors to PC2")

# Contribution of variables to PC3
fviz_contrib(res.pca, choice = "var", axes = 3, top = 6, ylim = c(0, 70)) +
  ggtitle("Top Contributors to PC3")

# Cos2 plot: how well each variable is represented on the PCs (higher cos² is better)
fviz_cos2(res.pca, choice = "var", axes = 1:2) +
  ggtitle("Cos² of Variables on PC1 & PC2")

# Biplot 
set.seed(11)  # for reproducibility
all_ids   <- rownames(res.pca$ind$coord)
sample_ids <- sample(all_ids, 1e2)

fviz_pca_biplot(
  res.pca,
  geom.ind    = "point",
  habillage   = groups,
  addEllipses = TRUE,
  palette     = pal,
  col.var     = "black",
  arrow.size  = 0.5,
  select.ind  = list(name = sample_ids),  # <— only these individuals
  repel       = TRUE
) +
  theme_classic()

# Pairwise PC Scatterplots
pc_scores <- as.data.frame(res.pca$ind$coord) # Extract scores from FactoMineR output
pc_scores$pci_match <- factor(
  lte_2024$pci_match,
  levels = c(-1, 1),
  labels = c("Mismatch", "Match")
)
pc_scores |> 
  slice_sample(n = 5000) |>
  ggpairs(
    columns = 1:4,
    mapping = aes(colour = pci_match, fill   = pci_match),
    title   = "Pairwise PCs 1–4 by PCI Match",
    palette = pal
  ) + theme_classic()


# Experimental 3D plot 
library(plotly)
library(reticulate)

# Extract the first three PC scores and your grouping factor
pca3 <- as.data.frame(res.pca$ind$coord[, 1:3])
colnames(pca3) <- c("PC1", "PC2", "PC3")
pca3$pci_match <- factor(
  lte_2024$pci_match,
  levels = c(-1, 1),
  labels = c("Mismatch","Match")
)

# Define palette
pal <- c("Mismatch" = "#FC4E07", "Match" = "#00AFBB")

# Plot
plot_ly(
  data = pca3,
  x = ~PC1, y = ~PC2, z = ~PC3,
  color = ~pci_match,
  colors = pal,
  type = "scatter3d",
  mode = "markers"
) %>%
  layout(
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    ),
    legend = list(title = list(text = "PCI Match"))
  )

## Create a GIF?
# install.packages(c("rgl","magick"))
library(scatterplot3d)
library(magick)

# 1) Prepare your PCA scores & colours
pca3 <- as.data.frame(res.pca$ind$coord[,1:3])
colnames(pca3) <- c("PC1","PC2","PC3")
pca3$pci_match <- factor(
  lte_2024$pci_match,
  levels = c(-1,1),
  labels = c("Mismatch","Match")
)
pal   <- c("Mismatch" = "#FC4E07", "Match" = "#00AFBB")
cols  <- pal[as.character(pca3$pci_match)]

# 2) Loop over a sequence of azimuth angles, save each frame
n_frames <- 120                         
# generate n_frames+1 angles from 0 to 360, then drop the last (360° == 0°)
angles <- seq(0, 360, length.out = n_frames + 1)
angles <- angles[-(n_frames + 1)]

png_files <- vapply(seq_along(angles), function(i) {
  fn <- tempfile(fileext = ".png")
  png(fn, width = 800, height = 800)
  
  scatterplot3d(
    x      = pca3$PC1,
    y      = pca3$PC2,
    z      = pca3$PC3,
    color  = cols,
    pch    = 19,
    xlab   = "PC1", ylab = "PC2", zlab = "PC3",
    angle  = angles[i],
    scale.y = 1,
    box    = FALSE
  )
  
  title(main = sprintf("3D PCA — azimuth %d°", round(angles[i])))
  dev.off()
  fn
}, character(1))

# 3) Read & stitch into a perfectly looping GIF
frames <- image_read(png_files)
gif    <- image_animate(image_join(frames), fps = 10, loop = 0)

# 4) Save
image_write(gif, "pca3d_spin.gif")