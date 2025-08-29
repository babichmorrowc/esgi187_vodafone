library(data.table)
library(RColorBrewer)
library(patchwork)
library(terra)
library(arrow)
library(sgo)

# rm(list=ls())
# gc()
setwd('D:/Vodafone')
source('Functions_for_vis.R')

# col_names <- names(fread(
#   "OneDrive_1_2025-7-14/Tony_Vernon_2024 Ofcom 4G Measurement Data/4g-lte-2024-mobile-signal-measurement-data.csv",
#   nrows = 0
# ))
# file_path <- "OneDrive_1_2025-7-14/Tony_Vernon_2024 Ofcom 4G Measurement Data/4g-lte-2024-mobile-signal-measurement-data.csv"
# col_names <- names(fread(file_path, nrows = 1))
# data_2024_ofcom_first_100000<-fread(
#   'OneDrive_1_2025-7-14/Tony_Vernon_2024 Ofcom 4G Measurement Data/4g-lte-2024-mobile-signal-measurement-data.csv',
#   sep = ",",nrows=500000)
# 
# vf_6300_first_100000<-subset(data_2024_ofcom_first_100000,earfcn_top1_vf == '6300' |earfcn_top2_vf == '6300' |earfcn_top3_vf == '6300' |earfcn_top4_vf =='6300' )
# vf_6300_first_100000_vf<- vf_6300_first_100000[,c(1:5,126:165)]
# vf_6300_first_100000_vf_clean_dataset<-vf_6300_first_100000_vf[,c(1:5,6,9,11,16,19,21,26,29,31,36,39,41)]
# 
# head(vf_6300_first_100000_vf_clean_dataset)
# dt <- vf_6300_first_100000_vf_clean_dataset
# 
# basic_cols <- c("latitude", "longitude", "month_year", "hour_ref", "rnum")
# rsrp_cols <- c("rsrp_top1_vf", "rsrp_top2_vf", "rsrp_top3_vf", "rsrp_top4_vf")
# pci_cols  <- c("pci_top1_vf",  "pci_top2_vf",  "pci_top3_vf",  "pci_top4_vf")
# 
# vf_6300_first_100000_vf_clean_dataset_strongest_pci <- dt[, {
#   r1 <- as.numeric(get("rsrp_top1_vf"))
#   r2 <- as.numeric(get("rsrp_top2_vf"))
#   r3 <- as.numeric(get("rsrp_top3_vf"))
#   r4 <- as.numeric(get("rsrp_top4_vf"))
#   
#   p1 <- as.integer(get("pci_top1_vf"))
#   p2 <- as.integer(get("pci_top2_vf"))
#   p3 <- as.integer(get("pci_top3_vf"))
#   p4 <- as.integer(get("pci_top4_vf"))
#   
#   rsrps <- c(r1, r2, r3, r4)
#   pcis  <- c(p1, p2, p3, p4)
#   
#   if (all(is.na(rsrps))) {
#     list(rsrp_max = NA_real_, pci_rsrp_max = NA_integer_)
#   } else {
#     idx <- which.max(rsrps)
#     list(rsrp_max = rsrps[idx], pci_rsrp_max = pcis[idx])
#   }
# }, by = .(latitude, longitude, month_year, hour_ref, rnum)]
# 
# vf_6300_first_100000_vf_clean_dataset_strongest_pci
# 
# table(vf_6300_first_100000_vf_clean_dataset_strongest_pci$pci_rsrp_max)



data_cell_PCI <- read.csv("OneDrive_1_2025-7-14/L8_Sept24 Sites TXs and Cells/L8_Sept24 Cells.csv", header = TRUE)  
data_index <- read.csv("OneDrive_1_2025-7-14/Tony_Vernon_2024 LTE MIMO Terminal [No Loss] (DL) 800MHz/LTE  MIMO Terminal [No Loss] (DL) 800MHz.svr.csv",header=FALSE)  

asc_signal_rast <- rast("OneDrive_1_2025-7-14/Tony_Vernon_2024 LTE MIMO Terminal [No Loss] (DL) 800MHz/LTE  MIMO Terminal [No Loss] (DL) 800MHz.asc")
asc_index_rast <- rast("OneDrive_1_2025-7-14/Tony_Vernon_2024 LTE MIMO Terminal [No Loss] (DL) 800MHz/LTE  MIMO Terminal [No Loss] (DL) 800MHz.svr.asc")

# print(asc_index_rast)
# plot(asc_index_rast) 
# 
# print(asc_signal_rast)
# plot(asc_signal_rast)  

# check unmatched values
false_values <- data_index$V2[!data_index$V2 %in% data_cell_PCI$Transmitter]
unique(false_values)

unmatched_rows <- data_index[!data_index$V2 %in% data_cell_PCI$Transmitter, ]
head(unmatched_rows) 
dim(unmatched_rows)

# map cell_transmitter_index to PCIs(0 to 503)
map_table <- merge(data_index, data_cell_PCI, by.x = "V2", by.y = "Name", all.x = TRUE)
lookup_vector <- setNames(map_table$Physical.Cell.ID, map_table$V1)
asc_index_data <- as.matrix(asc_index_rast)
pcis_matrix <- matrix(
  lookup_vector[as.character(asc_index_data)],
  nrow = nrow(asc_index_data),
  ncol = ncol(asc_index_data),
  byrow = FALSE
)

table(is.na(pcis_matrix))

pcis_raster <- asc_index_rast
values(pcis_raster) <- as.vector(pcis_matrix)

dev.off()
minmax(pcis_raster)
full_plot<-plot(pcis_raster, main = 'Spatial graph of PCIs')

# #### subregion to take a look at (not really useful ###)
# extent_region <- ext(215000, 235000, 384000, 404000)
# cropped_raster <- crop(pcis_raster, extent_region)
# cropped_raster_signal <- crop(asc_signal_rast, extent_region)
# plot(cropped_raster, main = "Zoomed-in PCI region")
# levels(cropped_raster) <- data.frame(value = unique(values(cropped_raster)))
# cropped_raster_factor <- as.factor(cropped_raster)
# par(mfrow = c(1, 2))
# cropped_plot<-plot(cropped_raster_factor, main = "PCI",
#                    plg = list(
#                    size = c(0.3, 0.3),
#                    cex = 0.7))
# cropped_plot_signal<-plot(cropped_raster_signal, col=cols,main = "Rsrp")
# 
# par(mfrow = c(1, 2))
# plot(pcis_raster, main = "Full PCI Raster",)
# plot(cropped_raster_factor, main = "Zoomed-in PCI Region (Factor)")
# plot(asc_signal_rast,col=cols,main='Spatial graph predicted signal')
# plot(cropped_raster_signal,col=cols, main = "Zoomed-in PCI Region (Factor)")


### Map PCIs to the geographical matrix #
df <- as.data.frame(pcis_raster, xy = TRUE, cells = FALSE)
minmax(pcis_raster)

dt <- as.data.table(df)
setnames(dt, c("x", "y", "LTE  MIMO Terminal [No Loss] (DL) 800MHz.svr"), c("x", "y", "PCI"))

head(dt)
dim(dt)
table(dt$PCI)
coordinates_Predicted_BestPCIs<-dt
head(coordinates_Predicted_BestPCIs)
summary(coordinates_Predicted_BestPCIs)

sort(unique(coordinates_Predicted_BestPCIs$x), partial = 5)[1:5]

# save as .feather file
# write_feather(coordinates_Predicted_BestPCIs, "coordinates_Predicted_BestPCIs.feather")

df_signal <- as.data.frame(asc_signal_rast, xy = TRUE, cells = FALSE)
minmax(asc_signal_rast)

dt_signal <- as.data.table(df_signal)
setnames(dt_signal, c("x", "y", "LTE  MIMO Terminal [No Loss] (DL) 800MHz"), c("x", "y", "Rsrp"))

head(dt_signal)
dim(dt_signal)
coordinates_Predicted_Rsrp<-dt_signal
head(coordinates_Predicted_Rsrp)
summary(coordinates_Predicted_Rsrp)

# save as .feather file
# write_feather(coordinates_Predicted_Rsrp, "coordinates_Predicted_Rsrp.feather")

# bin_2024_first_100000n<-latlon2tb(vf_6300_first_100000_vf_clean_dataset_strongest_pci$latitude,vf_6300_first_100000_vf_clean_dataset_strongest_pci$longitude)
# head(bin_2024_first_100000n)
# coordinates_2024_car <- data.table(
#   bin = bin_2024_first_100000n,
#   x = substr(bin_2024_first_100000n, 1, 6),
#   y = substr(bin_2024_first_100000n, 7, 12)
# )
# head(coordinates_2024_car)
# head(coordinates_Predicted_BestPCIs)
# vf_6300_first_100000_vf_clean_dataset_strongest_pci$binx<-coordinates_2024_car$x
# vf_6300_first_100000_vf_clean_dataset_strongest_pci$biny<-coordinates_2024_car$y
# dt1 <- copy(vf_6300_first_100000_vf_clean_dataset_strongest_pci)
# dt2 <- copy(coordinates_Predicted_BestPCIs)
# dt1[, x_match := as.numeric(binx) + 50]
# dt1[, y_match := as.numeric(biny) + 50]
# setkey(dt2, x, y)
# dt1 <- dt2[dt1, on = .(x = x_match, y = y_match)]
# head(dt1)
# dim(dt1)
# unmatched_pcis<-subset(dt1,PCI !=pci_rsrp_max)
# head(unmatched_pcis)
# dim(unmatched_pcis)
# min(unmatched_pcis$x)
# max(unmatched_pcis$x)
# min(unmatched_pcis$y)
# max(unmatched_pcis$y)
# matched_pcis<-subset(dt1,PCI ==pci_rsrp_max)
# head(matched_pcis)
# dim(matched_pcis)
# min(matched_pcis$x)
# max(matched_pcis$x)
# min(matched_pcis$y)
# max(matched_pcis$y)
# 
# plot_pci_unmatched_region(350000, 355000, 468000, 478000,
#                           pcis_raster,        # PCI raster layer
#                           unmatched_pcis,     # Data frame with x, y, PCI, pci_rsrp_max
#                           title = "Unmatched_PCIs_2024",
#                           legend_title = "Voda-Of",
#                           legend_ncol = 3,
#                           color_range = c("#FFCCCC", "#FF6666", "#990000"))
# 
# 
# matched_summary_xy<-summarize_data_unmatched_pci_rsrp_by_bin(350000, 355000, 468000, 478000,
#                                          matched_pcis,
#                                          coordinates_Predicted_Rsrp)
# 
# plot_pci_unmatched_region_howmuch(
#   350000, 354000, 472000, 476000,
#   pci_raster = pcis_raster,
#   point_data = matched_summary_xy,
#   title = "Matched PCIs: RSRP Observed - Predicted",
#   legend_title = "Δ RSRP (dB)"
# )
# 
# plot_pci_unmatched_region(340000, 360000, 660000, 700000,
#                           pcis_raster,        # PCI raster layer
#                           unmatched_pcis,     # Data frame with x, y, PCI, pci_rsrp_max
#                           title = "Unmatched_PCIs_2024",
#                           legend_title = "Voda-Of",
#                           legend_ncol = 3,
#                           color_range = c("#FFCCCC", "#FF6666", "#990000"))
# 
# matched_summary_xy<-summarize_data_unmatched_pci_rsrp_by_bin(115000, 450000, 134000, 904000,
#                                                              matched_pcis,
#                                                              coordinates_Predicted_Rsrp)
# plot_pci_unmatched_region_howmuch(
#   340000, 360000, 660000, 700000,
#   pci_raster = pcis_raster,
#   point_data = matched_summary_xy,
#   title = "Matched PCIs: RSRP Observed - Predicted",
#   legend_title = "Δ RSRP (dB)"
# )
# plot_pci_unmatched_region(100000, 510000, 130000, 970000,
#                           pcis_raster,        # PCI raster layer
#                           unmatched_pcis,     # Data frame with x, y, PCI, pci_rsrp_max
#                           title = "Unmatched_PCIs_2024",
#                           legend_title = "Voda-Of",
#                           legend_ncol = 3,
#                           color_range = c("#FFCCCC", "#FF6666", "#990000"))
# 
# cor(summary_by_xy$n_obs,summary_by_xy$rsrp_diff)
# plot(summary_by_xy$n_obs,summary_by_xy$rsrp_diff)


###############################################################################################
### Thank to David we have the processed terribin_summary.csv file which is then used directly
### File name:
### 4g-lte-2024-mobile-signal-measurement-data-combined-summary.csv ###########################
###############################################################################################
cols <- colorRampPalette(c("yellow", "orange","red" ))(100)
data_summary_david<-read.csv('OneDrive_1_2025-7-17/4g-lte-2024-mobile-signal-measurement-data-combined-summary.csv')
head(data_summary_david)

setDT(data_summary_david)
setDT(coordinates_Predicted_BestPCIs)

min(coordinates_Predicted_BestPCIs$y)
min(coordinates_Predicted_BestPCIs$x)

coordinates_Predicted_BestPCIs[, `:=` (
  x_shift = as.character(x - 50),
  y_shift = as.character(y - 50)
)]

coordinates_Predicted_BestPCIs[, tb := paste0(x_shift, y_shift)]

data_summary_david[, tb := as.character(tb)]

data_summary_david_x_y <- merge(
  data_summary_david,
  coordinates_Predicted_BestPCIs[, .(tb, x_pred = x_shift, y_pred = y_shift)],
  by = "tb",
  all.x = TRUE
)
head(data_summary_david_x_y)
dim(data_summary_david_x_y)

subregion_data_summary_david_x_y<-data_summary_david_x_y[
  data_summary_david_x_y$x_pred > 0 & data_summary_david_x_y$x_pred < 900000 &
    data_summary_david_x_y$y_pred > 0 & data_summary_david_x_y$y_pred < 900000, ]
dim(subregion_data_summary_david_x_y)
head(subregion_data_summary_david_x_y)
colnames(subregion_data_summary_david_x_y)[c(2,4,16,17)]<-c('PCI','pci_rsrp_max','x','y')
subregion_data_summary_david_x_y$rsrp_mean_diff_match<-subregion_data_summary_david_x_y$ofcom_rsrp_match_mean-
                                                  subregion_data_summary_david_x_y$voda_rsrp
subregion_data_summary_david_x_y$rsrp_median_diff_match<-subregion_data_summary_david_x_y$ofcom_rsrp_match_median-
                                                  subregion_data_summary_david_x_y$voda_rsrp
subregion_data_summary_david_x_y$rsrp_amean_diff_match<-subregion_data_summary_david_x_y$ofcom_rsrp_all_mean-
                                                  subregion_data_summary_david_x_y$voda_rsrp
subregion_data_summary_david_x_y$rsrp_amedian_diff_match<-subregion_data_summary_david_x_y$ofcom_rsrp_all_median-
                                                  subregion_data_summary_david_x_y$voda_rsrp


# par(mfrow=c(1,2))
plot_pci_region_match_unmatch(350000, 353000, 470000, 473500,
                              pcis_raster,        # PCI raster layer
                              subregion_data_summary_david_x_y,     # Data frame with x, y, PCI, pci_rsrp_max
                              title = "PCIs_whether_match_2024",
                              legend_title = "Voda-Of",
                              legend_ncol = 3,
                              color_range = c("#FFCCCC", "#FF6666", "#990000"),
                              match_color = "white",  # 新增参数：匹配时的颜色
                              match_label = "Match") 

plot_pci_match_unmatch_region_howmuch(
  350000, 353000, 470000, 473500,
  pci_raster = pcis_raster,
  diff_field='rsrp_median_diff_match',
  point_data = subregion_data_summary_david_x_y,
  title = "PCIs: RSRP Observed - Predicted",
  legend_title = "Δ RSRP (dB)"
)

dev.off()

plot_pci_region_match_unmatch(345000, 360000, 465000, 485000,
                              pcis_raster,        # PCI raster layer
                              subregion_data_summary_david_x_y,     # Data frame with x, y, PCI, pci_rsrp_max
                              title = "PCIs_whether_match_2024",
                              legend_title = "Voda-Of",
                              legend_ncol = 3,
                              color_range = c("#FFCCCC", "#FF6666", "#990000"),
                              match_color = "white",  # 新增参数：匹配时的颜色
                              match_label = "Match",
                              left_drift=0.05,
                              legend_ncol_left=5 ) 

par(mfrow=c(2,2))
plot_pci_match_unmatch_region_howmuch(
  345000, 360000, 465000, 485000,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_mean_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(mean_diff_match)'),
  legend_title = "Δ RSRP (dB)"
)
plot_pci_match_unmatch_region_howmuch(
  345000, 360000, 465000, 485000,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_median_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(median_diff_match)'),
  legend_title = "Δ RSRP (dB)"
)
plot_pci_match_unmatch_region_howmuch(
  345000, 360000, 465000, 485000,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_amean_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(mean_diff_all)'),
  legend_title = "Δ RSRP (dB)"
)
plot_pci_match_unmatch_region_howmuch(
  345000, 360000, 465000, 485000,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_amedian_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(median_diff_all)'),
  legend_title = "Δ RSRP (dB)"
)


par(mfrow=c(2,2))
plot_pci_match_unmatch_region_howmuch(
  350000, 353000, 470000, 473500,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_mean_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(mean_diff_match)'),
  legend_title = "Δ RSRP (dB)"
)
plot_pci_match_unmatch_region_howmuch(
  350000, 353000, 470000, 473500,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_median_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(median_diff_match)'),
  legend_title = "Δ RSRP (dB)"
)
plot_pci_match_unmatch_region_howmuch(
  350000, 353000, 470000, 473500,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_amean_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(mean_diff_all)'),
  legend_title = "Δ RSRP (dB)"
)
plot_pci_match_unmatch_region_howmuch(
  350000, 353000, 470000, 473500,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_amedian_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(median_diff_all)'),
  legend_title = "Δ RSRP (dB)"
)

dev.off()



dev.off()

plot_pci_region_match_unmatch(333000,383000,147000,197000,
                                pcis_raster,        # PCI raster layer
                                subregion_data_summary_david_x_y,     # Data frame with x, y, PCI, pci_rsrp_max
                                title = "PCIs_whether_match_2024",
                                legend_title = "Voda-Of",
                                legend_ncol = 4,
                                color_range = c("#FFCCCC", "#FF6666", "#990000"),
                                match_color = "white",  # 新增参数：匹配时的颜色
                                match_label = "Match",
                                left_drift=0.05,
                                legend_ncol_left=7 ) 


par(mfrow=c(2,2))
plot_pci_match_unmatch_region_howmuch(
  333000,383000,147000,197000,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_mean_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(mean_diff_match)'),
  legend_title = "Δ RSRP (dB)",
  show_raster_legend = FALSE
)
plot_pci_match_unmatch_region_howmuch(
  333000,383000,147000,197000,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_median_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(median_diff_match)'),
  legend_title = "Δ RSRP (dB)",
  show_raster_legend = FALSE
)
plot_pci_match_unmatch_region_howmuch(
  333000,383000,147000,197000,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_amean_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(mean_diff_all)'),
  legend_title = "Δ RSRP (dB)",
  show_raster_legend = FALSE
)
plot_pci_match_unmatch_region_howmuch(
  333000,383000,147000,197000,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_amedian_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(median_diff_all)'),
  legend_title = "Δ RSRP (dB)",
  show_raster_legend = FALSE
)

### off site example for daniel ###
plot_pci_region_match_unmatch(  340000, 344000, 169000, 172000,
                                pcis_raster,        # PCI raster layer
                                subregion_data_summary_david_x_y,     # Data frame with x, y, PCI, pci_rsrp_max
                                title = "PCIs_whether_match_2024",
                                legend_title = "Voda-Of",
                                legend_ncol = 3,
                                color_range = c("#FFCCCC", "#FF6666", "#990000"),
                                match_color = "white",  # 新增参数：匹配时的颜色
                                match_label = "Match",
                                left_drift=0.02,
                                legend_ncol_left=2) 

plot_pci_match_unmatch_region_howmuch(
  340000, 344000, 169000, 172000,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_mean_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(mean_diff_match)'),
  legend_title = "Δ RSRP (dB)",
  show_raster_legend = FALSE
)


plot_pci_region_match_unmatch(  365000, 367000, 178000, 179000,
                                pcis_raster,        # PCI raster layer
                                subregion_data_summary_david_x_y,     # Data frame with x, y, PCI, pci_rsrp_max
                                title = "PCIs_whether_match_2024",
                                legend_title = "Voda-Of",
                                legend_ncol = 1,
                                color_range = c("#FFCCCC", "#FF6666", "#990000"),
                                match_color = "white",  # 新增参数：匹配时的颜色
                                match_label = "Match",
                                left_drift=0.02,
                                legend_ncol_left=2) 

plot_pci_match_unmatch_region_howmuch(
  365000, 367000, 178000, 179000,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_amean_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(mean_diff_all)'),
  legend_title = "Δ RSRP (dB)",
  show_raster_legend = FALSE
)





### Paul ####
#### interesting are ######

plot_pci_region_match_unmatch(  317500, 321500, 668500, 671500,
                                pcis_raster,        # PCI raster layer
                                subregion_data_summary_david_x_y,     # Data frame with x, y, PCI, pci_rsrp_max
                                title = "PCIs_whether_match_2024",
                                legend_title = "Voda-Of",
                                legend_ncol = 3,
                                color_range = c("#FFCCCC", "#FF6666", "#990000"),
                                match_color = "white",  # 新增参数：匹配时的颜色
                                match_label = "Match") 

plot_pci_match_unmatch_region_howmuch(
  317500, 321500, 668500, 671500,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_median_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(median_diff_match)'),
  legend_title = "Δ RSRP (dB)"
)

plot_pci_region_match_unmatch(    317000,322000,668000,672000,
                                pcis_raster,        # PCI raster layer
                                subregion_data_summary_david_x_y,     # Data frame with x, y, PCI, pci_rsrp_max
                                title = "PCIs_whether_match_2024",
                                legend_title = "Voda-Of",
                                legend_ncol = 3,
                                color_range = c("#FFCCCC", "#FF6666", "#990000"),
                                match_color = "white",  # 新增参数：匹配时的颜色
                                match_label = "Match") 


plot_pci_match_unmatch_region_howmuch(
  317000,322000,668000,672000,
  pci_raster = pcis_raster,
  point_data = subregion_data_summary_david_x_y,
  diff_field = 'rsrp_amean_diff_match',
  title = paste0("PCIs: Observed - Predicted", '(mean_diff_all)'),
  legend_title = "Δ RSRP (dB)",
  show_raster_legend = FALSE
)


dev.off()











