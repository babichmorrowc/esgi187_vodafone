latlon2tb <- function(lat, lon, bin_length = 100) {
  pts <- sgo_points(list(longitude=lon, latitude=lat), epsg=4326)
  bng.pts <- sgo_lonlat_bng(pts)
  tb.x <- bng.pts$x - bng.pts$x %% bin_length
  tb.y <- bng.pts$y - bng.pts$y %% bin_length
  terrain_bin <- paste(tb.x, tb.y, sep = "")
  return(terrain_bin)
}


plot_pci_unmatched_region <- function(xmin, xmax, ymin, ymax,
                                      pci_raster,         # PCI raster layer
                                      point_data,         # Data frame with x, y, PCI, pci_rsrp_max
                                      title = "PCI",
                                      legend_title = "Voda-Of",
                                      legend_ncol = 3,
                                      color_range = c("#FFCCCC", "#FF6666", "#990000")) {
  
  # 1. Define spatial extent
  extent_region <- ext(xmin, xmax, ymin, ymax)
  
  # 2. Crop raster to the defined extent
  cropped_raster <- crop(pci_raster, extent_region)
  cropped_raster_factor <- as.factor(cropped_raster)
  
  # 3. Subset the points inside the extent
  subset_points <- point_data[
    point_data$x > xmin & point_data$x < xmax &
      point_data$y > ymin & point_data$y < ymax, ]
  
  # 4. If no points matched, show warning and exit
  if (nrow(subset_points) == 0) {
    warning("No points found within the specified extent.")
    return(NULL)
  }
  
  # 5. Create group labels for coloring
  subset_points$group <- paste0(subset_points$PCI, "-", subset_points$pci_rsrp_max)
  groups <- unique(subset_points$group)
  
  # 6. Generate red-tone color palette
  red_palette <- colorRampPalette(color_range)
  colors <- setNames(red_palette(length(groups)), groups)
  
  # 8. Plot the PCI raster with legend settings
  plot(cropped_raster_factor, main = title,
       plg = list(
         ncol = legend_ncol,
         size = c(0.3, 0.3),
         cex = 0.7)
  )
  
  # 9. Overlay matching points with color by group
  points(subset_points$x, subset_points$y,
         col = colors[subset_points$group],
         pch = 16,
         cex = 1.2)
  
  # 10. Add external legend on the right side
  legend("topleft",
         inset = c(-0.05, 0),  # move legend outside plot area
         legend = groups,
         col = colors,
         pch = 16,
         cex = 0.8,
         title = legend_title,
         xpd = TRUE,           # allow drawing outside the plot area
         bty = "n")            # no box around the legend
}


summarize_data_unmatched_pci_rsrp_by_bin <- function(xmin, xmax, ymin, ymax,
                                                     unmatched_pcis,
                                                     coordinates_Predicted_Rsrp) {
  
  # 1. Filter points within the given extent
  subset_interested <- unmatched_pcis[
    unmatched_pcis$x > xmin & unmatched_pcis$x < xmax &
      unmatched_pcis$y > ymin & unmatched_pcis$y < ymax, ]
  
  setDT(subset_interested)
  
  # 2. Aggregate by (x, y): count, mean/sd of RSRP, PCI info
  summary_by_xy <- subset_interested[, .(
    n_obs     = .N,
    rsrp_mean = mean(rsrp_max, na.rm = TRUE),
    rsrp_sd   = sd(rsrp_max, na.rm = TRUE),
    pci_set = list(unique(PCI)),
    pci_rsrp_set = list(unique(pci_rsrp_max)),
    pci_rsrp_consistent = uniqueN(pci_rsrp_max) == 1,
    pci_mode = as.integer(names(sort(table(PCI), decreasing = TRUE)[1])),          # most frequent PCI
    pci_rsrp_mode = as.integer(names(sort(table(pci_rsrp_max), decreasing = TRUE)[1]))  # most frequent pci_rsrp_max
  ), by = .(x, y)]
  
  # 3. Join with predicted RSRP values
  setDT(coordinates_Predicted_Rsrp)
  summary_by_xy <- coordinates_Predicted_Rsrp[summary_by_xy, on = .(x, y)]
  setcolorder(summary_by_xy, c(setdiff(names(summary_by_xy), "Rsrp"), "Rsrp"))
  setnames(summary_by_xy, "Rsrp", "predicted_rsrp")
  
  # 4. Calculate difference between mean observed and predicted RSRP
  summary_by_xy[, rsrp_diff := rsrp_mean - predicted_rsrp]
  
  return(summary_by_xy)
}

plot_pci_unmatched_region_howmuch <- function(xmin, xmax, ymin, ymax,
                                              pci_raster,
                                              point_data,
                                              title = "PCI vs Prediction Diff",
                                              legend_title = "RSRP Diff (dB)") {
  
  # 1. Define spatial extent
  extent_region <- terra::ext(xmin, xmax, ymin, ymax)
  
  # 2. Crop raster to extent
  cropped_raster <- terra::crop(pci_raster, extent_region)
  cropped_raster_factor <- as.factor(cropped_raster)
  
  # 3. Subset points
  subset_points <- point_data[
    x > xmin & x < xmax &
      y > ymin & y < ymax, ]
  
  if (nrow(subset_points) == 0) {
    warning("No points found within the specified extent.")
    return(NULL)
  }
  
  # 4. Centered color scale
  diff_vals <- subset_points$rsrp_diff
  diff_max <- max(abs(diff_vals), na.rm = TRUE)
  zlim <- c(-diff_max, diff_max)
  
  n_colors <- 100
  color_palette <- colorRampPalette(c("orange", "white", "red"))(n_colors)
  
  # 5. Map rsrp_diff to color
  breaks <- seq(zlim[1], zlim[2], length.out = n_colors + 1)
  color_bins <- cut(diff_vals, breaks = breaks, include.lowest = TRUE)
  point_colors <- color_palette[as.numeric(color_bins)]
  
  # 6. Plot PCI raster
  plot(cropped_raster_factor, main = title,
       plg = list(ncol = 3, size = c(0.3, 0.3), cex = 0.7))
  
  # 7. Add points
  points(subset_points$x, subset_points$y,
         col = point_colors,
         pch = 16, cex = 1.3)
  
  # 8. Legend
  if (requireNamespace("fields", quietly = TRUE)) {
    fields::image.plot(legend.only = TRUE,
                       zlim = zlim,
                       col = color_palette,
                       legend.width = 1.5,
                       legend.shrink = 0.7,
                       smallplot = c(0.85, 0.88, 0.3, 0.7),
                       legend.args = list(text = legend_title, side = 4, line = 2.5, cex = 0.8))
  } else {
    warning("Install package 'fields' to show continuous color legend.")
  }
}

plot_pci_region_match_unmatch <- function(xmin, xmax, ymin, ymax,
                                          pci_raster,         # PCI raster layer
                                          point_data,         # Data frame with x, y, PCI, pci_rsrp_max
                                          title = "PCI",
                                          legend_title = "Voda-Of",
                                          legend_ncol = 3,
                                          color_range = c("#FFCCCC", "#FF6666", "#990000"),
                                          match_color = "white",
                                          match_label = "Match",
                                          left_drift=0.05,
                                          legend_ncol_left=1) {  # 匹配点的图例标签
  
  # 1. Define spatial extent
  extent_region <- ext(xmin, xmax, ymin, ymax)
  
  # 2. Crop raster to the defined extent
  cropped_raster <- crop(pci_raster, extent_region)
  cropped_raster_factor <- as.factor(cropped_raster)
  
  # 3. Subset the points inside the extent
  subset_points <- point_data[
    point_data$x > xmin & point_data$x < xmax &
      point_data$y > ymin & point_data$y < ymax, ]
  
  # 4. If no points matched, show warning and exit
  if (nrow(subset_points) == 0) {
    warning("No points found within the specified extent.")
    return(NULL)
  }
  
  # 5. Create a group label
  subset_points$group <- paste0(subset_points$PCI, "-", subset_points$pci_rsrp_max)
  
  # 6. Identify matched points (PCI == prediction)
  subset_points$match <- subset_points$PCI == subset_points$pci_rsrp_max
  
  # 7. Set color palette for unmatched points
  unmatched_groups <- unique(subset_points$group[!subset_points$match])
  red_palette <- colorRampPalette(color_range)
  unmatched_colors <- setNames(red_palette(length(unmatched_groups)), unmatched_groups)
  
  # 8. Plot the PCI raster
  plot(cropped_raster_factor, main = title,
       plg = list(
         ncol = legend_ncol,
         size = c(0.3, 0.3),
         cex = 0.7)
  )
  
  # 9. Plot unmatched points
  unmatched_points <- subset_points[!subset_points$match, ]
  points(unmatched_points$x, unmatched_points$y,
         col = unmatched_colors[unmatched_points$group],
         pch = 10, cex = 1.2)
  
  # 10. Plot matched points with special color
  matched_points <- subset_points[subset_points$match, ]
  if (nrow(matched_points) > 0) {
    points(matched_points$x, matched_points$y,
           col = match_color,
           pch = 16, cex = 1.4)
  }
  
  # 11. Add legend
  legend_labels <- c(unmatched_groups, match_label)
  legend_colors <- c(unmatched_colors, match_color)
  
  legend("topleft",
         inset = c(-left_drift, 0),
         legend = legend_labels,
         col = legend_colors,
         pch = 16,
         cex = 0.4,
         title = legend_title,
         xpd = TRUE,
         bty = "n",
         ncol = legend_ncol_left)
}
plot_pci_match_unmatch_region_howmuch <- function(xmin, xmax, ymin, ymax,
                                                  pci_raster,
                                                  point_data,
                                                  diff_field,
                                                  title = "PCI vs Prediction Diff",
                                                  legend_title = "RSRP Diff (dB)",
                                                  heat_legend = TRUE,
                                                  show_raster_legend = TRUE  # ✅ 
) {
  
  # 1. Define spatial extent
  extent_region <- terra::ext(xmin, xmax, ymin, ymax)
  
  # 2. Crop raster to extent
  cropped_raster <- terra::crop(pci_raster, extent_region)
  cropped_raster_factor <- as.factor(cropped_raster)
  
  # 3. Subset points
  subset_points <- point_data[
    x > xmin & x < xmax &
      y > ymin & y < ymax, ]
  
  if (nrow(subset_points) == 0) {
    warning("No points found within the specified extent.")
    return(NULL)
  }
  
  # 4. Color mapping for diff field
  diff_vals <- subset_points[[diff_field]]
  diff_max <- max(abs(diff_vals), na.rm = TRUE)
  zlim <- c(-diff_max, diff_max)
  
  n_colors <- 100
  color_palette <- colorRampPalette(c("orange", "white", "red"))(n_colors)
  
  breaks <- seq(zlim[1], zlim[2], length.out = n_colors + 1)
  color_bins <- cut(diff_vals, breaks = breaks, include.lowest = TRUE)
  point_colors <- color_palette[as.numeric(color_bins)]
  
  # 5. Plot PCI raster（是否显示raster图例由参数控制）
  if (show_raster_legend) {
    plot(cropped_raster_factor, main = title,
         plg = list(ncol = 3, size = c(0.3, 0.3), cex = 0.7))
  } else {
    plot(cropped_raster_factor, main = title, legend = FALSE)
  }
  
  # 6. Add points
  points(subset_points$x, subset_points$y,
         col = point_colors,
         pch = 16, cex = 1.3)
  
  # 7. Add heatmap legend (RSRP diff)
  if (heat_legend && requireNamespace("fields", quietly = TRUE)) {
    fields::image.plot(legend.only = TRUE,
                       zlim = zlim,
                       col = color_palette,
                       legend.width = 1.5,
                       legend.shrink = 0.7,
                       smallplot = c(0.85, 0.88, 0.3, 0.7),
                       legend.args = list(text = legend_title, side = 4, line = 2.5, cex = 0.8))
  }
}


















