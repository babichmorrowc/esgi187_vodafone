library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(xgboost)
library(ggplot2)
library(patchwork)


### DATA
lte_2024 <- read_csv("4g-lte-2024-mobile-signal-measurement-data-combined-summary-45-110-ext-landuse-sites.csv") |> 
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

### XGBOOST model 
# Build feature‐matrix, dropping the original factor
#    Encode the remaining factors with model.matrix:
X_full <- model.matrix(
  ~ voda_pci_te
  + site_dist
  + x + y + xy + x2 + y2
    + pci_match
    + urban,
  data = dt
)
y_full <- dt$err_all_mean
rm(dt, pci_stats)

# Split into train/test DMatrix
dtrain <- xgb.DMatrix(
  data  = X_full[train_idx, ],
  label = y_full[train_idx]
)
dtest <- xgb.DMatrix(
  data  = X_full[test_idx, ],
  label = y_full[test_idx]
)

val_idx <- sample(train_idx, floor(0.2*length(train_idx)))
tr_idx  <- setdiff(train_idx, val_idx)
dtr  <- xgb.DMatrix(X_full[tr_idx, ],  label = y_full[tr_idx])
dval <- xgb.DMatrix(X_full[val_idx, ], label = y_full[val_idx])

watchlist <- list(train = dtr, val = dval)

# Train XGBoost (vanilla RMSE)
params <- list(
  booster        = "gbtree",
  objective      = "reg:squarederror",
  eval_metric    = "rmse",
  tree_method    = "hist",
  eta            = 0.05,
  max_depth      = 6,
  subsample      = 0.8,
  colsample_bytree=0.8
)

set.seed(2025)
xgb_mod <- xgb.train(
  params  = params,
  data    = dtr,
  nrounds = 20000,
  early_stopping_rounds = 100,
  watchlist = watchlist,
  verbose = 1
)

# xgb.save(xgb_mod, 'xgb_vanilla.model')

# Plot
eval_df <- as.data.frame(xgb_mod$evaluation_log) |>
  pivot_longer(
    cols      = c(train_rmse, val_rmse),
    names_to  = "set",
    values_to = "rmse"
  )
ggplot(eval_df, aes(x = iter, y = rmse, color = set)) +
  geom_line() +
  labs(
    title = "XGBoost (vanilla) Convergence",
    x     = "Boosting Round",
    y     = "RMSE",
    color = NULL
  ) +
  theme_classic(base_size = 16)

# Evaluate
preds <- predict(xgb_mod, dtest)
rmse <- sqrt(mean((y_full[test_idx] - preds)^2))
r2   <- cor(y_full[test_idx], preds)^2
cat(sprintf("Test RMSE = %.3f,  R² = %.3f\n", rmse, r2))

# Feature importance
imp <- xgb.importance(feature_names = colnames(X_full), model = xgb_mod)
print(imp)

imp |>
  mutate(Feature = factor(Feature, levels = rev(Feature))) |>
  filter(Gain >= 0.005) |>
  ggplot(aes(x = Gain, y = Feature)) +
  geom_col(fill = "#00AFBB") +
  labs(
    title    = "XGBoost Variable Importance (by Gain)",
    x        = "Relative Contribution to Model",
    y        = NULL
  ) +
  theme_classic(base_size = 16)


# DART parameters
params_dart <- list(
  booster        = "dart",
  objective      = "reg:squarederror",
  eval_metric    = "rmse",
  tree_method    = "hist",
  sample_type    = "uniform",   # or "weighted"
  normalize_type = "tree",      # how dropouts are normalized
  rate_drop      = 0.1,         # fraction of trees to drop each iteration
  skip_drop      = 0.5          # prob. of skipping dropout for a tree
)

xgb_mod_dart <- xgb.train(
  params   = params_dart,
  data     = dtr,
  nrounds  = 20000,
  watchlist= watchlist,
  early_stopping_rounds = 20
)

xgb.save(xgb_mod_dart, 'xgb_dart.model')

# # Plot
eval_df <- as.data.frame(xgb_mod_dart$evaluation_log) |>
  pivot_longer(
    cols      = c(train_rmse, val_rmse),
    names_to  = "set",
    values_to = "rmse"
  )
ggplot(eval_df, aes(x = iter, y = rmse, color = set)) +
  geom_line() +
  labs(
    title = "XGBoost (DART) Convergence",
    x     = "Boosting Round",
    y     = "RMSE",
    color = NULL
  ) +
  theme_minimal(base_size = 16)

# Evaluate
preds_dart <- predict(xgb_mod_dart, dtest)
rmse_dart <- sqrt(mean((y_full[test_idx] - preds_dart)^2))
r2_dart   <- cor(y_full[test_idx], preds_dart)^2
cat(sprintf("Test RMSE = %.3f,  R² = %.3f\n", rmse_dart, r2_dart))

# Feature importance
imp_dart <- xgb.importance(feature_names = colnames(X_full), model = xgb_mod_dart)
print(imp_dart)

imp_dart |>
  mutate(Feature = factor(Feature, levels = rev(Feature))) |>
  filter(Gain >= 0.005) |>
  ggplot(aes(x = Gain, y = Feature)) +
  geom_col(fill = "#00AFBB") +
  labs(
    title    = "XGBoost DART Variable Importance (by Gain)",
    x        = "Relative Contribution to Model",
    y        = NULL
  ) +
  theme_classic(base_size = 16)




# --- SHAP on the UNTOUCHED TEST SET (robust order) ----------------------------

# Get the model's internal feature order and align X_test to it
feat <- xgb_mod$feature_names

X_test <- X_full[test_idx, feat, drop = FALSE]   # reorder to model order
dtest_ordered <- xgb.DMatrix(X_test)

# SHAP contributions (+BIAS) -- use approxcontrib=TRUE for speed on big sets
shap_test <- predict(xgb_mod, dtest_ordered,
                     predcontrib = TRUE, approxcontrib = TRUE)

# Name columns exactly: features in 'feat' order, plus 'BIAS'
colnames(shap_test) <- c(feat, "BIAS")

# Global SHAP importance (mean |SHAP|)
shap_imp <- data.frame(
  Feature     = feat,
  MeanAbsSHAP = colMeans(abs(shap_test[, feat, drop = FALSE]))
)
shap_imp <- shap_imp[order(-shap_imp$MeanAbsSHAP), ]
# Drop intercept if present (shouldn't be if you used 'feat' from the model)
shap_imp <- subset(shap_imp, Feature != "(Intercept)")

print(head(shap_imp, 10))

p <- ggplot(shap_imp, aes(x = reorder(Feature, MeanAbsSHAP), y = MeanAbsSHAP)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Mean Abs SHAP (test set)",
       title = "Global importance by SHAP (XGBoost)") +
  theme_classic(base_size = 16)
p
ggsave("xgboost_shap_rank.png", p, width = 6, height = 4, dpi = 300)


 # Helper function to create SHAP dependence plots -----------------------------------------------------
shap_dependence_plot <- function(feature, color_by = NULL,
                                 sample_n = 50000, smoother = c("loess","none")) {
  smoother <- match.arg(smoother)
  idx <- seq_len(nrow(X_test))
  if (!is.null(sample_n) && length(idx) > sample_n) {
    set.seed(1); idx <- sample(idx, sample_n)
  }
  df <- data.frame(
    x = X_test[idx, feature],
    shap = shap_test[idx, feature]
  )
  if (!is.null(color_by) && color_by %in% colnames(X_test)) {
    df[[color_by]] <- X_test[idx, color_by]
    p <- ggplot(df, aes(x = x, y = shap, color = .data[[color_by]])) +
      geom_point(alpha = 0.5, size = 2)
  } else {
    p <- ggplot(df, aes(x = x, y = shap)) + geom_point(alpha = 0.4, size = 2)
  }
  if (smoother == "loess") {
    p <- p + geom_smooth(se = FALSE, color = "red", linewidth=2, method = "loess", formula = y ~ x, span = 0.8)
  }
  p + labs(x = feature, y = paste0("SHAP(", feature, ")"),
           title = paste("SHAP dependence for", feature)) +
    theme_classic(base_size = 16)
}

# Examples
p1 <- shap_dependence_plot("voda_pci_te", color_by = "urban1", smoother = "loess")
p2 <- shap_dependence_plot("site_dist", color_by = "urban1", smoother = "loess")
p3 <- shap_dependence_plot("x", color_by = "y", smoother = "loess")
p4 <- shap_dependence_plot("y", color_by = "x", smoother = "loess")

ggsave("xgboost_shap_dep_voda_pci_te.png", p1, width = 6, height = 6, dpi = 300)
ggsave("xgboost_shap_dep_site_dist.png", p2, width = 6, height = 6, dpi = 300)
ggsave("xgboost_shap_dep_x.png", p3, width = 6, height = 6, dpi = 300)
ggsave("xgboost_shap_dep_y.png", p4, width = 6, height = 6, dpi = 300)


# Signed average SHAP (directionality)
shap_signed_mean <- sort(colMeans(shap_test[, feat, drop = FALSE]), decreasing = TRUE)
print(shap_signed_mean)

# Binary factors: effect by level (echo GAM signs) --------------------------
if ("pci_match1" %in% feat) {
  df_pm <- data.frame(
    level = factor(X_test[, "pci_match1"], levels = c(0, 1),
                   labels = c("mismatch(-1)", "match(1)")),
    shap  = shap_test[, "pci_match1"]
  )
  print(aggregate(shap ~ level, df_pm, mean))
  q1 <- ggplot(df_pm, aes(level, shap)) +
    geom_boxplot(outlier.alpha = 0.2) +
    labs(x = "pci_match level", y = "SHAP(pci_match1)",
         title = "SHAP by pci_match level (test set)") +
    theme_classic(base_size = 16)
}

if ("urban1" %in% feat) {
  df_ur <- data.frame(
    level = factor(X_test[, "urban1"], levels = c(0, 1),
                   labels = c("Rural(-1)", "Urban(1)")),
    shap  = shap_test[, "urban1"]
  )
  print(aggregate(shap ~ level, df_ur, mean))
  q2 <- ggplot(df_ur, aes(level, shap)) +
    geom_boxplot(outlier.alpha = 0.2) +
    labs(x = "urban level", y = "SHAP(urban1)",
         title = "SHAP by urban level (test set)") +
    theme_classic(base_size = 16)
}

ggsave("xgboost_shap_urban.png", q2, width = 6, height = 6, dpi = 300)
ggsave("xgboost_shap_pci_match.png", q1, width = 6, height = 6, dpi = 300)


# Sanity check: SHAP reconstruction equals predictions
pred_test <- predict(xgb_mod, dtest_ordered)  # uses best iteration by default
recon <- rowSums(shap_test[, feat, drop = FALSE]) + shap_test[, "BIAS"]
cat("Max|reconstructed - predicted| =",
    max(abs(recon - pred_test)), "\n")
