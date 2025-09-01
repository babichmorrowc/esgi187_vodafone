library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(GGally)
library(corrplot)
library(forcats)

lte_2024 <- read_csv("4g-lte-2024-mobile-signal-measurement-data-combined-summary-45-110-ext-landuse-sites.csv")

lte_2024 <- lte_2024 |> 
  mutate(
    err_match1_mean = voda_rsrp - ofcom_rsrp_match_1_mean,
    err_match_mean  = voda_rsrp - ofcom_rsrp_match_mean,
    err_all_mean    = voda_rsrp - ofcom_rsrp_all_mean,
    range_ofcom_rsrp= ofcom_rsrp_all_max - ofcom_rsrp_all_min, 
    pci_match       = ifelse(voda_pci == ofcom_pci_mode, 1, -1),
    ofcom_pci_zero  = ifelse(ofcom_pci_mode == 0, 1, -1),
    urban           = ifelse(urban_rural == "Urban", 1, -1)
    ) |>
  rename(
    site_dist = `lte_comb$site_dist`
   )



pal <- c("Mismatch" = "#FC4E07", "Match" = "#00AFBB")
pal2 <- c("Urban" = "#FC4E07", "Rural" = "#00AFBB")
groups <- factor(
  lte_2024$pci_match,
  levels = c(-1, 1),
  labels = c("Mismatch","Match")
)
groups2 <- factor(
  lte_2024$urban,
  levels = c(-1, 1),
  labels = c("Urban","Rural")
)

lte_2024 |> 
  summarise(mean = mean(err_match1_mean, na.rm = TRUE))

# select vars for PCA
pca_df <- lte_2024 |>
  select(
    voda_rsrp,
    # err_match1_mean,           
    err_all_mean,
    # ofcom_pci_zero,
    ofcom_pci_mode_count,   
    ofcom_match_1_count,
    # voda_pci,
    ofcom_all_count,
    range_ofcom_rsrp,
    pci_match,
    urban,
    site_dist
  )

res.pca <- PCA(
  X      = pca_df,
  scale.unit = TRUE,   # centre and scale each variable
  ncp    = 5,          
  graph  = FALSE       
)

# Scree plot of eigenvalues: how much variance each PC explains
p1 <- fviz_eig(
  res.pca,
  addlabels = TRUE,    # show % values on bars
  ylim      = c(0, 40)
) + theme_classic(base_size = 16)
ggsave("pca_scree.png", p1, width = 6, height = 4, dpi = 300)


# Variable map: see how each variable loads on PC1 vs PC2
p2 <- fviz_pca_var(
  res.pca,
  col.var    = "contrib", # colour by contribution to PCs
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel      = TRUE       # avoid label overlap
) + theme_classic(base_size = 16)
p2
ggsave("pca_variables.png", p2, width = 6, height = 4, dpi = 300)

# Individual map: plot observations in PC space, coloured by pci_match (converted to factor here for discrete groups)
p3 <- fviz_pca_ind(
  res.pca,
  geom.ind     = "point",
  habillage    = factor(lte_2024$urban, labels = c("Urban","Rural")),
  addEllipses  = TRUE,    # confidence ellipses around groups
  palette      = pal2
) + theme_classic(base_size = 16) 
p3
ggsave("pca_pc_space.png", p3, width = 6, height = 4, dpi = 300)


# Contribution of variables to PC1
p4 <- fviz_contrib(res.pca, choice = "var", axes = 1, top = 4, ylim = c(0, 50), base_size = 16) +
  ggtitle("Top Contributors to PC1") + theme_classic(base_size = 16) +
  theme(axis.title.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(angle = 45))
ggsave("pca_cont_pc1.png", p4, width = 6, height = 4, dpi = 300)


# Contribution of variables to PC2
p5 <- fviz_contrib(res.pca, choice = "var", axes = 2, top = 4, ylim = c(0, 50), base_size = 16) +
  ggtitle("Top Contributors to PC2") + theme_classic(base_size = 16) +
  theme(axis.title.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(angle = 45))
ggsave("pca_cont_pc2.png", p5, width = 6, height = 4, dpi = 300)


# Contribution of variables to PC3
p6 <- fviz_contrib(res.pca, choice = "var", axes = 3, top = 4, ylim = c(0, 50), base_size = 16) +
  ggtitle("Top Contributors to PC3") + theme_classic(base_size = 16) +
  theme(axis.title.x = element_blank()) +
  scale_x_discrete(guide = guide_axis(angle = 45))
ggsave("pca_cont_pc3.png", p6, width = 6, height = 4, dpi = 300)


# Contribution of variables to PC4
fviz_contrib(res.pca, choice = "var", axes = 4, top = 4, ylim = c(0, 50)) +
  ggtitle("Top Contributors to PC4")



# Cos2 plot: how well each variable is represented on the PCs (higher cos² is better)
fviz_cos2(res.pca, choice = "var", axes = 1:2) +
  ggtitle("Cos² of Variables on PC1 & PC2")

# Biplot 
set.seed(11)  # for reproducibility
all_ids   <- rownames(res.pca$ind$coord)
sample_ids <- sample(all_ids, 5e2)

p7 <- fviz_pca_biplot(
  res.pca,
  geom.ind    = "point",
  habillage   = groups2,
  addEllipses = TRUE,
  palette     = pal2,
  col.var     = "black",
  arrow.size  = 0.5,
  select.ind  = list(name = sample_ids),  # <— only these individuals
  repel       = TRUE
) +
  theme_classic(base_size = 16)
ggsave("pca_biplot.png", p7, width = 6, height = 4, dpi = 300)

# Pairwise PC Scatterplots
pc_scores <- as.data.frame(res.pca$ind$coord) # Extract scores from FactoMineR output
pc_scores$pci_match <- factor(
  lte_2024$urban,
  levels = c(-1, 1),
  labels = c("Urban", "Rural")
)
p8 <- pc_scores |> 
  slice_sample(n = 5000) |>
  ggpairs(
    columns = 1:3,
    mapping = aes(colour = pci_match, fill   = pci_match, alpha = 0.2),
    title   = "Pairwise PCs by Land Use",
    palette = pal
  ) + theme_classic(base_size = 16)
ggsave("pca_pairwise.png", p8, width = 6, height = 4, dpi = 300)
 