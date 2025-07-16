library(arrow)
library(tidyverse)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# random vector of length 1000
# x <- sample(1:10, 1000, replace = TRUE)
# x <- as.factor(x)
# Mode(x)

#### Ofcom measured RSRP data ####
# Read in 2024 Offcom data
lte_2024 <- read_feather("data/4g-lte-2024-mobile-signal-measurement-data.arrow")

# Pivot the entire data longer
lte_2024_long <- lte_2024 |>
  # add row number
  mutate(obs_id = row_number()) |>
  select(obs_id,
         tb,
         starts_with(c("pci_", "earfcn_", "rsrp_"))) |> 
  # row per 1,2,3,4
  pivot_longer(
    cols = starts_with(c("pci_", "earfcn_", "rsrp_")),
    names_to = c(".value", "top"),
    names_pattern = "(.*)_(top[1-4]_vf)"
  ) |> 
  # Remove wrong EARFCN
  filter(earfcn == 6300) |> 
  # Remove NA RSRP
  filter(!is.na(rsrp))

rm(lte_2024)

# # Visualize the distribution of RSRP values
# ggplot(lte_2024_long, aes(x = rsrp)) +
#   geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
#   labs(title = "Distribution of RSRP Values in 2024 LTE Data",
#        x = "RSRP (dBm)",
#        y = "Frequency") +
#   theme_minimal()
# mean(lte_2024_long$rsrp, na.rm = TRUE)
# sd(lte_2024_long$rsrp, na.rm = TRUE)
# # Do a KS test on the RSRP values
# ks.test(lte_2024_long$rsrp, "pnorm", mean = mean(lte_2024_long$rsrp, na.rm = TRUE), 
#         sd = sd(lte_2024_long$rsrp, na.rm = TRUE))
# # data:  lte_2024_long$rsrp
# # D = 0.01024, p-value < 2.2e-16
# # alternative hypothesis: two-sided
# 
# # Compare to a normal distribution
# lte_2024_long |> 
#   ggplot(aes(rsrp)) +
#   geom_density(stat="density", adjust = 3, color = "black", linewidth = 1, linetype = 1) + 
#   stat_function(fun = dnorm,
#                 args = list(mean = mean(lte_2024_long$rsrp, na.rm = TRUE),
#                             sd = sd(lte_2024_long$rsrp, na.rm = TRUE)),
#                 color = "red",
#                 linetype = 2,
#                 linewidth = 1) +
#   labs(title = "Smoothing estimate of Ofcom Measured 4G RSRP",
#        subtitle = "RSRP for measurements with EARFCN = 6300",
#        caption = "Data: 2024",
#        x = "RSRP (dBm)",
#        y = "Density") +
#   theme_classic() 
# # QQ plot
# lte_2024_long |> 
#   filter(!is.na(rsrp)) |> 
#   # sample 10%
#   sample_frac(0.1) |> 
#   ggplot(aes(sample = rsrp)) +
#   geom_qq() +
#   stat_qq_line() +
#   labs(title = "Ofcom Measured 4G RSRP",
#        subtitle = "RSRP for measurements with EARFCN = 6300",
#        caption = "Data: 2024",
#        x = "Observed quantiles",
#        y = "Theoretical quantiles") +
#   theme_classic()

# summary by TB
# Over all 4 observations
lte_2024_wide_tb_all <- lte_2024_long |> 
  mutate(pci = as.factor(pci)) |> 
  group_by(tb) |> 
  summarise(
    mean_rsrp_all = mean(rsrp),
    sd_rsrp_all = sd(rsrp),
    median_rsrp_all = median(rsrp),
    max_rsrp_all = max(rsrp),
    # mode_pci_all = Mode(pci), # custom mode function
    best_pci_all = pci[which.max(max_rsrp_all)],
    # number of PCIs
    n_pcis_all = length(unique(pci)),
    n_obs_all = n()
  )

# For only the best RSRP of the 4 observations
best_obs <- lte_2024_long |>
  mutate(pci = as.factor(pci)) |> 
  group_by(obs_id, tb) |> 
  order_by(rsrp, .desc = TRUE) |>
  slice(1)

lte_2024_wide_tb_best <- lte_2024_long |>
  mutate(pci = as.factor(pci)) |> 
  group_by(obs_id, tb) |> 
  order_by(rsrp, .desc = TRUE) |>
  slice(1) |>
  summarise(
    mean_rsrp_best = mean(rsrp),
    sd_rsrp_best = sd(rsrp),
    median_rsrp_best = median(rsrp),
    max_rsrp_best = max(rsrp),
    mode_pci_best = Mode(pci), # custom mode function
    best_pci_best = pci[which.max(max_rsrp_best)],
    # number of PCIs
    n_pcis_best = length(unique(pci)),
    n_obs_best = n()
  )


#### Join Vodafone predicted data #####
best_pcis <- read_feather("data/coordinates_Predicted_BestPCIs.feather") |> 
  mutate(tb = paste0(x,y))
