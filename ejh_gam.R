library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(mgcv)
library(ggplot2)
library(patchwork)


### DATA
lte_2024 <- read_csv("~/OneDrive - University of Dundee/KnowledgeExchange/ESGI-187-Bristol/vodafone/data/4g-lte-2024-mobile-signal-measurement-data-combined-summary-45-110-ext-landuse-sites.csv") |> 
  mutate(
    err_match1_mean = voda_rsrp - ofcom_rsrp_match_1_mean,
    err_match_mean  = voda_rsrp - ofcom_rsrp_match_mean,
    err_all_mean    = voda_rsrp - ofcom_rsrp_all_mean,
    range_ofcom_rsrp      = ofcom_rsrp_all_max - ofcom_rsrp_all_min, 
    pci_match       = ifelse(voda_pci == ofcom_pci_mode, 1, -1),
    ofcom_pci_zero  = ifelse(ofcom_pci_mode == 0, 1, -1),
    urban           = ifelse(urban_rural == "Urban", 1, -1)
  ) |>
  rename(
    site_dist = `lte_comb$site_dist`
  )
# Prepare data
# select your predictors and response:
dt <- as.data.table(lte_2024)
rm(lte_2024)
# Subset & factor your columns
dt <- dt[, .(
  err_all_mean,
  voda_pci    = factor(voda_pci),
  voda_rsrp,
  pci_match   = factor(pci_match),
  urban       = factor(urban),
  site_dist,
  dist_log   = log1p(site_dist),
  dist_20log = 20 * log10(site_dist + 1),
  dist_sqrt  = sqrt(site_dist),
  dist_inv   = 1/(site_dist + 1),
  x, 
  y, 
  x2 = x^2,
  y2 = y^2,
  xy = x * y
)][complete.cases(dt)]
# scale coordinates
dt[, `:=`(x_s = as.numeric(scale(x)), y_s = as.numeric(scale(y)))]

# Train/test split indices
set.seed(123)
n         <- nrow(dt)
train_idx <- sample(n, size = floor(0.8 * n))
test_idx  <- setdiff(seq_len(n), train_idx)

# Compute smoothing constants
k            <- 20
global_mean  <- dt[train_idx, mean(err_all_mean)]
# Compute per‐PCI smoothing (shrinkage) on TRAIN only
pci_stats <- dt[train_idx,
                .( 
                  count = .N,
                  meanY = mean(err_all_mean)
                ),
                by = voda_pci
]
pci_stats[, te := (meanY * count + global_mean * k) / (count + k)] # Smoothing (shrinkage)
# Map the encoding back onto full data
#    join dt ← pci_stats on voda_pci
dt <- pci_stats[dt, on = "voda_pci"]
#    rename the joined column
setnames(dt, "te", "voda_pci_te")
#    for any PCI in TEST not seen in TRAIN, fill with global_mean
dt[is.na(voda_pci_te), voda_pci_te := global_mean]

###

### GAM model (baseline linear effects) 
# using bam instead of gam 
gam_mod <- bam(
  err_all_mean ~
    s(x_s, k=200, bs="tp") + s(y_s, k=200, bs="tp") + 
    ti(x_s, y_s, bs=c("tp","tp"), k=c(30,30)) + 
    # s(site_dist, k=10) +
    s(dist_20log, by = urban, k = 10, bs = "tp") +  # two curves: urban/rural
    voda_pci_te  + 
    pci_match + urban,
  data = dt[train_idx],
  method="fREML", 
  discrete=TRUE
)
# Out-of-sample check you already computed:
pred_gam <- predict(gam_mod, newdata = dt[test_idx])
r2_gam <- cor(dt$err_all_mean[test_idx], pred_gam)^2
rmse_gam <- sqrt(mean((dt$err_all_mean[test_idx] - pred_gam)^2))
c(R2_test = r2_gam, RMSE_test = rmse_gam)

summary(gam_mod)
gam.check(gam_mod)

# # concurvity(gam_mod, full = TRUE)
# plot(gam_mod, select=4, shade=TRUE, residuals=TRUE) #(rural)
# plot(gam_mod, select=5, shade=TRUE, residuals=TRUE) #(urban)
# vis.gam(gam_mod, view=c("x_s","y_s"), plot.type="contour")
# 
# # Observed vs Predicted with calibration line
# p <- predict(gam_mod, dt[test_idx])
# y <- dt$err_all_mean[test_idx]
# df <- data.frame(pred = p, obs = y)
# 
# ggplot(df, aes(pred, obs)) +
#   geom_hex(bins = 40) +
#   geom_smooth(method = "lm", se = FALSE) +        # fitted calibration line
#   geom_abline(slope = 1, intercept = 0, linetype = 2) +  # perfect calibration
#   labs(x = "Predicted error (dB)", y = "Observed error (dB)",
#        title = "GAM Observed vs Predicted (test set)") +
#   theme_classic()
# summary(lm(obs ~ pred, data = df))  # report slope/intercept & R²
# 
# res <- y - p
# ggplot(data.frame(pred = p, res = res), aes(pred, res)) +
#   geom_hex(bins = 40) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   geom_smooth(se = FALSE) +
#   labs(x = "Predicted error (dB)", y = "Residual (dB)",
#        title = "GAM Residuals vs Fitted (test set)") +
#   theme_classic()

# Ensure we have convenient labels for 'urban' (levels are "-1", "1")
urban_labs <- c(`-1` = "Rural", `1` = "Urban")

# Helper: grab term names as mgcv prints them (used to robustly pick columns)
.tmp_terms <- predict(gam_mod, newdata = as.data.frame(dt[1]), type = "terms", se.fit = TRUE)
term_names <- colnames(.tmp_terms$fit)

# Identify columns for the pieces we want
spat_cols  <- grep("^s\\(x_s\\)|^s\\(y_s\\)|^ti\\(x_s, *y_s\\)", term_names, value = TRUE)

# Distance smooths by Urban/Rural (with CI) ------------------
# Build a grid for distance and both urban levels; hold other vars at constants
dist_seq <- seq(
  quantile(dt$dist_20log, 0.01, na.rm = TRUE),
  quantile(dt$dist_20log, 0.99, na.rm = TRUE),
  length.out = 250
)

# Choose constants for other predictors (values don't affect *this* smooth)
x0  <- mean(dt$x_s, na.rm = TRUE)
y0  <- mean(dt$y_s, na.rm = TRUE)

# use baseline levels for factors not involved in the smooth
pci_base   <- levels(dt$pci_match)[1]   # likely "-1"
urban_lvls <- levels(dt$urban)          # "-1","1"

z <- qnorm(0.975)

new_dist <- expand.grid(
  dist_20log = dist_seq,
  urban      = urban_lvls,
  KEEP.OUT.ATTRS = FALSE
) |>
  mutate(
    urban      = factor(urban, levels = urban_lvls),
    pci_match  = factor(pci_base, levels = levels(dt$pci_match)),
    voda_pci_te = 0,                     # centered for clarity
    x_s = x0, y_s = y0
  )

# Predict term-wise, then pick the appropriate 'by' column per row
pr_dist <- predict(gam_mod, newdata = new_dist, type = "terms", se.fit = TRUE)

# Column to use per row, e.g. "s(dist_20log):urban-1" or "s(dist_20log):urban1"
col_for_row <- paste0("s(dist_20log):urban", as.character(new_dist$urban))
col_idx <- match(col_for_row, colnames(pr_dist$fit))

# Effect and SE for the relevant smooth (other 'by' column contributes 0)
new_dist$fit <- pr_dist$fit[cbind(seq_len(nrow(new_dist)), col_idx)]
new_dist$se  <- pr_dist$se.fit[cbind(seq_len(nrow(new_dist)), col_idx)]
new_dist <- new_dist |>
  mutate(
    lo = fit - z * se,
    hi = fit + z * se,
    urban_label = factor(recode(as.character(urban), !!!urban_labs), levels = c("Rural","Urban"))
  )

p_dist <- ggplot(new_dist, aes(x = dist_20log, y = fit, color = urban_label, fill = urban_label)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2, color = NA) +
  geom_line(size = 0.9) +
  labs(
    x = "20·log10(distance + 1)",
    y = "Partial effect on error (dB)",
    color = "Setting", fill = "Setting",
    title = "Distance error smooths by setting"
  ) +
  theme_classic(base_size = 16)
p_dist

# Spatial field heatmap ---------------------------
# Grid over scaled coordinates; sum s(x_s) + s(y_s) + ti(x_s, y_s)
xs_seq <- seq(
  quantile(dt$x_s, 0.02, na.rm = TRUE),
  quantile(dt$x_s, 0.98, na.rm = TRUE),
  length.out = 180
)
ys_seq <- seq(
  quantile(dt$y_s, 0.02, na.rm = TRUE),
  quantile(dt$y_s, 0.98, na.rm = TRUE),
  length.out = 180
)

sp_grid <- expand.grid(
  x_s = xs_seq,
  y_s = ys_seq,
  KEEP.OUT.ATTRS = FALSE
) |>
  mutate(
    # Hold non-spatial covariates at constants so we isolate spatial component
    dist_20log  = median(dt$dist_20log, na.rm = TRUE),
    voda_pci_te = 0,
    pci_match   = factor(levels(dt$pci_match)[1], levels = levels(dt$pci_match)),
    urban       = factor(levels(dt$urban)[1], levels = levels(dt$urban))
  )

pr_sp <- predict(gam_mod, newdata = sp_grid, type = "terms", se.fit = FALSE)

# Sum the spatial terms only
sp_grid$spatial_effect <- rowSums(pr_sp[, spat_cols, drop = FALSE])

L <- max(abs(sp_grid$spatial_effect), na.rm = TRUE)

p_spatial <- ggplot(sp_grid, aes(x = x_s, y = y_s, fill = spatial_effect)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_distiller(
    type = "div", palette = "RdBu", direction = -1,  # blue for negatives, red for positives
    limits = c(-L, L), oob = scales::squish
  ) +
  labs(
    x = "Scaled easting (x_s)", y = "Scaled northing (y_s)",
    fill = "Effect",
    title = "Spatial field of systematic error"
  ) +
  coord_fixed() +
  theme_classic(base_size = 16)
p_spatial 

# SAVE
ggsave("gam_smooth_spatial.png", p_spatial, width = 6, height = 4, dpi = 300)
ggsave("gam_smooth_distance.png", p_dist, width = 6, height = 4, dpi = 300)
###

