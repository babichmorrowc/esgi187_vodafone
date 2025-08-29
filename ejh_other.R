# XGBoost with Quantile regression
#Quantile objective function; median (alpha = 0.5)
quantile_obj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  alpha  <- 0.5      # for median; change for other quantiles
  residuals <- labels - preds
  # gradient:
  grad <- ifelse(residuals > 0, -alpha, 1 - alpha)
  # hessian: constant (we use 1 to keep things simple)
  hess <- rep(1, length(preds))
  return(list(grad = grad, hess = hess))
}

params_quantile <- list(
  booster        = "gbtree",
  eval_metric    = "rmse",
  tree_method    = "hist",
  eta            = 0.1,
  max_depth      = 6,
  subsample      = 0.8,
  colsample_bytree=0.8
)
set.seed(2025)
xgb_quantile <- xgb.train(
  params               = params_quantile
  data                 = dtrain,
  nrounds              = 5000,
  obj                  = quantile_obj,
  watchlist            = list(train=dtrain, eval=dtest),
  early_stopping_rounds= 20,
  verbose              = 1
)

# Evaluate
preds_quantile <- predict(xgb_quantile, dtest)
rmse_quantile <- sqrt(mean((y_full[test_idx] - preds_quantile)^2))
r2_quantile   <- cor(y_full[test_idx], preds_quantile)^2
cat(sprintf("Test RMSE = %.3f,  R² = %.3f\n", rmse_quantile, r2_quantile))


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
  data     = dtrain,
  nrounds  = 1000,
  watchlist= list(val=dtest, train=dtrain),
  early_stopping_rounds = 20
)

# Evaluate
preds_dart <- predict(xgb_mod_dart, dtest)
rmse_dart <- sqrt(mean((y_full[test_idx] - preds_dart)^2))
r2_dart   <- cor(y_full[test_idx], preds_dart)^2
cat(sprintf("Test RMSE = %.3f,  R² = %.3f\n", rmse_dart, r2_dart))


## DART plus quantile objective 
params_dart_quantile <- list(
  booster        = "dart",
  eval_metric    = "rmse",
  tree_method    = "hist",
  sample_type    = "uniform",   # or "weighted"
  normalize_type = "tree",      # how dropouts are normalized
  rate_drop      = 0.1,         # fraction of trees to drop each iteration
  skip_drop      = 0.5,          # prob. of skipping dropout for a tree
  eta            = 0.1,
  max_depth      = 6,
  subsample      = 0.8,
  colsample_bytree=0.8
)

xgb_mod_dart_quantile <- xgb.train(
  params   = params_dart_quantile,
  data     = dtrain,
  nrounds  = 1000,
  watchlist= list(val=dtest, train=dtrain),
  obj      = quantile_obj,
  early_stopping_rounds = 20
)

# Evaluate
preds_dart_quantile <- predict(xgb_mod_dart_quantile, dtest)
rmse_dart_quantile <- sqrt(mean((y_full[test_idx] - preds_dart_quantile)^2))
r2_dart_quantile   <- cor(y_full[test_idx], preds_dart_quantile)^2
cat(sprintf("Test RMSE = %.3f,  R² = %.3f\n", rmse_dart_quantile, r2_dart_quantile))







# Set parameters and train
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  enable_categorical = TRUE,
  tree_method = "hist"
)

set.seed(2025)
xgb_mod <- xgb.train(
  params   = params,
  data     = dtrain,
  nrounds  = 200,
  verbose  = 0
)

# Extract and plot importance
imp <- xgb.importance(feature_names = colnames(X), model = xgb_mod)

# Print the raw table
print(imp)

# Gain: how much each feature improved the model's accuracy when it was used in splits (the most common "importance" metric).
# Cover: the relative number of observations impacted by those splits.
# Frequency: how often the feature was used.

# Or with ggplot2 for custom styling:
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
  theme_minimal()

preds <- predict(xgb_mod, dtest)
# Compute R² and RMSE
rmse <- sqrt(mean((y_test - preds)^2))
r2   <- cor(y_test, preds)^2

# Predict SHAP contributions on your test set
#    (Assuming you’ve already created dtest and y_test)
shap_contrib <- predict(
  xgb_mod,
  dtest,
  predcontrib = TRUE        # this returns a matrix: [n_test, n_features+1]
)

# Convert to a data.frame and drop the “BIAS” column
shap_df <- as.data.frame(shap_contrib)[, colnames(shap_contrib) != "BIAS"]

# Compute mean absolute SHAP for each feature
shap_imp <- shap_df |>
  summarise(across(everything(), ~ mean(abs(.x), na.rm=TRUE))) %>%
  pivot_longer(everything(), names_to="Feature", values_to="MeanAbsSHAP") %>%
  arrange(desc(MeanAbsSHAP))

print(shap_imp)

ggplot(shap_imp, aes(x = MeanAbsSHAP, y = fct_reorder(Feature, MeanAbsSHAP))) +
  geom_col(fill = "#00AFBB") +
  labs(
    title = "Feature importance by mean(|SHAP value|)",
    x     = "Average absolute SHAP",
    y     = NULL
  ) +
  theme_minimal()



###
gam_mod <- gam(
  err_all_mean ~
    s(x, y, k = 100) + s(range_all) + s(site_dist) +
    voda_rsrp + pci_match + urban_match,
  data = dt[train_idx]
)
pred_gam <- predict(gam_mod, newdata = dt[test_idx])
r2_gam <- cor(dt$err_all_mean[test_idx], pred_gam)^2

summary(gam_mod)








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


###
library(mgcv)
gam_mod <- gam(
  err_all_mean ~
    s(x, y, k = 100) + s(range_all) + s(site_dist) +
    voda_rsrp + pci_match + urban_match,
  data = dt[train_idx]
)
pred_gam <- predict(gam_mod, newdata = dt[test_idx])
r2_gam <- cor(dt$err_all_mean[test_idx], pred_gam)^2

summary(gam_mod)