## Script to customize stability plots for report

cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999)

# Install and load packages (list can be enriched if needed)
# install.packages("pacman")
pacman::p_load("dplyr", "haven", "foreign", "here", "readr",
               "stringr", "readxl", "data.table", "caret", "car", "glmnet",
               "ParBayesianOptimization", "ranger", "e1071", "randomForestSRC",
               "xgboost", "parallel", "doParallel", "fastDummies", "RANN",
               "kernlab", "ggplot2", "purrr", "tidyr", "rvest", "tidyverse",
               "rlang", "cowplot", "gridGraphics", "magick", "grid")

## Sourcing custom plot functions
# source(here::here("scripts", "functions", "functions_plotting_ML.R"))

## Plotting Stability plots (multiple at once)

## loading in all plot objects

all_plots_models_A <- readRDS(
  file = here::here(
    "data", "final", "stability", "stability_plots_model_A.rds"))

all_plots_models_B <- readRDS(
  file = here::here(
    "data", "final", "stability", "stability_plots_model_B.rds"))

all_plots_models_C <- readRDS(
  file = here::here(
    "data", "final", "stability", "stability_plots_model_C.rds"))

all_plots_models_D <- readRDS(
  file = here::here(
    "data", "final", "stability", "stability_plots_model_D.rds"))

all_plots_models_E <- readRDS(
  file = here::here(
    "data", "final", "stability", "stability_plots_model_E.rds"))

## combining all elements of the workspace into one list
list_all_plots <- lapply(ls(), get)

rf_plots <- lapply(list_all_plots, function(x){
  rf_plot <- x$rf
  return(rf_plot)
})

xgb_plots <- lapply(list_all_plots, function(x){
  rf_plot <- x$xgb
  return(rf_plot)
})



svr_plots <- all_plots_models_A$svr


rf_pred_plots <- lapply(rf_plots, function(x){
  pred_plot <- x$plot_pred_inst_model + 
    ## removing axis labels to add them later on entire plot
    labs(x = NULL, y = NULL) + 
    theme(
      # x axis tick labels: add top margin so they move further down (away from plot area)
      axis.text.x = element_text(size=12, margin = margin(t = 5)), 
      # y axis tick labels: add right margin so they move further left (away from plot area)
      axis.text.y = element_text(size=12, margin = margin(r = 5)),
    # optionally reduce tick length a bit to avoid visual collision
    #axis.ticks.length = unit(3, "pt")
  ) +
  scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0))
})

rf_cal_plots <- lapply(rf_plots, function(x){
  pred_plot <- x$plot_cal_inst_model + 
    labs(x = NULL, y = NULL, color = NULL, alpha = NULL, size = NULL) +

  theme(
    # x axis tick labels: add top margin so they move further down (away from plot area)
    axis.text.x = element_text(size=12, margin = margin(t = 5)), 
    # y axis tick labels: add right margin so they move further left (away from plot area)
    axis.text.y = element_text(size=12, margin = margin(r = 5)),
    
    legend.position="none",
    # optionally reduce tick length a bit to avoid visual collision
    #axis.ticks.length = unit(3, "pt")
  ) +
  scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0))
})

rf_mape_plots <- lapply(rf_plots, function(x){
  mape_plot <- x$plot_mape_inst_model
})


xgb_pred_plots <- lapply(xgb_plots, function(x){
  pred_plot <- x$plot_pred_inst_model + 
    ## removing axis labels to add them later on entire plot
    labs(x = NULL, y = NULL) + 
    theme(
      # x axis tick labels: add top margin so they move further down (away from plot area)
      axis.text.x = element_text(size=12, margin = margin(t = 5)), 
      # y axis tick labels: add right margin so they move further left (away from plot area)
      axis.text.y = element_text(size=12, margin = margin(r = 5)),
    # optionally reduce tick length a bit to avoid visual collision
    #axis.ticks.length = unit(3, "pt")
  ) +
  scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0))
})

xgb_cal_plots <- lapply(xgb_plots, function(x){
  pred_plot <- x$plot_cal_inst_model + 
    labs(x = NULL, y = NULL, color = NULL, alpha = NULL, size = NULL) +

  theme(
    # x axis tick labels: add top margin so they move further down (away from plot area)
    axis.text.x = element_text(size=12, margin = margin(t = 5)), 
    # y axis tick labels: add right margin so they move further left (away from plot area)
    axis.text.y = element_text(size=12, margin = margin(r = 5)),
    legend.position="none",
    # optionally reduce tick length a bit to avoid visual collision
    #axis.ticks.length = unit(3, "pt")
  ) +
  scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0))
})

xgb_mape_plots <- lapply(xgb_plots, function(x){
  mape_plot <- x$plot_mape_inst_model
})


svr_pred_plot <- svr_plots$plot_pred_inst_model +
  labs(x = "Estimated Quality of life score in the bootstrap samples",
       y = "Estimated Quality of life score from original model",
       title = "Variable set A (CBCL items only), Algorithm: Support vector regression") + 
    theme(
      # x axis tick labels: add top margin so they move further down (away from plot area)
      axis.text.x = element_text(size=12, margin = margin(t = 5)), 
      # y axis tick labels: add right margin so they move further left (away from plot area)
      axis.text.y = element_text(size=12, margin = margin(r = 5)),
      # optionally reduce tick length a bit to avoid visual collision
      #axis.ticks.length = unit(3, "pt")
    ) +
    scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 10), expand = c(0, 0))


ggsave(here::here("data", "final", "stability", "svr_plot_prediction_instability.png"),
       plot = svr_pred_plot,
       width = 15, height = 11, units = "cm",
       dpi = 600,
       bg = "white",  # ensure transparent background doesn't cut edges
       limitsize = FALSE)

svr_cal_plot <- svr_plots$plot_cal_inst_model +
    labs(x = "Predicted Quality of life score",
         y = "Observed Quality of life score",
         title = "Variable set A (CBCL items only), Algorithm: Support vector regression",
         color = NULL, alpha = NULL, size = NULL) +
    
    theme(
      # x axis tick labels: add top margin so they move further down (away from plot area)
      axis.text.x = element_text(size=12, margin = margin(t = 5)), 
      # y axis tick labels: add right margin so they move further left (away from plot area)
      axis.text.y = element_text(size=12, margin = margin(r = 5)),
      
      legend.position="none",
      # optionally reduce tick length a bit to avoid visual collision
      #axis.ticks.length = unit(3, "pt")
    ) +
    scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 10), expand = c(0, 0))


ggsave(here::here("data", "final", "stability", "svr_plot_calibration_instability.png"),
       plot = svr_cal_plot,
       width = 15, height = 11, units = "cm",
       dpi = 600,
       bg = "white",  # ensure transparent background doesn't cut edges
       limitsize = FALSE)



## combining all prediction instability plots into one column

## axis labels for the overall plot
labels_prediction <- c("Estimated Quality of life score in the bootstrap samples", 
                       "Estimated Quality of life score from original model")

labels_calibration <- c("Predicted Quality of life score",
                        "Observed Quality of life score")

labels_mape <- c("Original prediction Quality of life", "MAPE")

##-----------------------------------------------------------------------------

## Combined plot for prediction instability (random forest)
combined_plot_rf_pred <- ggdraw(
  cowplot::plot_grid(
    plotlist  = rf_pred_plots,
    labels    = paste0("variable set ", LETTERS[1:5]),
    nrow      = 5,
    ncol      = 1,
    label_size = 10,
    label_x   = 0.02,
    label_y   = 0.95
  )
) +
  # X-axis label: keep it inside the figure, slightly above the bottom edge
  draw_label(
    labels_prediction[1],
    x = 0.5,    # centered horizontally
    y = -0.02,
    vjust = 1,  # anchor the top of the text at y=0.04 (so text sits *above* that point)
    size = 10
  ) +
  # Y-axis label: keep it inside the figure, slightly right of the left edge
  draw_label(
    labels_prediction[2],
    x = -0.02,   # small positive value keeps it inside the canvas
    y = 0.5,
    angle = 90,
    hjust = 1,  # anchor the right side of the rotated text at x=0.06
    size = 10
  ) +
  # Increase outer margins a little so tick labels + these draw_labels are not clipped
  theme(plot.margin = margin(t = 20, r = 20, b = 40, l = 40))

#combined_plot_rf_pred

ggsave(here::here("data", "final", "stability", "rf_plot_prediction_instability.png"),
       plot = combined_plot_rf_pred,
       width = 15, height = 22, units = "cm",
       dpi = 600,
       bg = "white",  # ensure transparent background doesn't cut edges
       limitsize = FALSE)





## Combined plot for calibration instability (random forest)
combined_plot_rf_cal <- ggdraw(
  cowplot::plot_grid(
    plotlist  = rf_cal_plots,
    labels    = paste0("variable set ", LETTERS[1:5]),
    nrow      = 5,
    ncol      = 1,
    label_size = 10,
    label_x   = 0.02,
    label_y   = 0.95
  )
) +
  # X-axis label: keep it inside the figure, slightly above the bottom edge
  draw_label(
    labels_calibration[1],
    x = 0.5,    # centered horizontally
    y = -0.02,
    vjust = 1, 
    size = 10
  ) +
  # Y-axis label: keep it inside the figure, slightly right of the left edge
  draw_label(
    labels_calibration[2],
    x = -0.02,   # small positive value keeps it inside the canvas
    y = 0.5,
    angle = 90,
    hjust = 1,  # anchor the right side of the rotated text at x=0.06
    size = 10
  ) +
  # Increase outer margins a little so tick labels + these draw_labels are not clipped
  theme(plot.margin = margin(t = 20, r = 20, b = 40, l = 40))

combined_plot_rf_cal

ggsave(here::here("data", "final", "stability", "rf_plot_calibration_instability.png"),
       plot = combined_plot_rf_cal,
       width = 15, height = 22, units = "cm",
       dpi = 600,
       bg = "white",  # ensure transparent background doesn't cut edges
       limitsize = FALSE)



## Next: Repeat this for XGB, put together MAPE plots
## Combined plot for prediction instability (XGB)
combined_plot_xgb_pred <- ggdraw(
  cowplot::plot_grid(
    plotlist  = xgb_pred_plots,
    labels    = paste0("variable set ", LETTERS[1:5]),
    nrow      = 5,
    ncol      = 1,
    label_size = 10,
    label_x   = 0.02,
    label_y   = 0.95
  )
) +
  # X-axis label: keep it inside the figure, slightly above the bottom edge
  draw_label(
    labels_prediction[1],
    x = 0.5,    # centered horizontally
    y = -0.02,
    vjust = 1,  # anchor the top of the text at y=0.04 (so text sits *above* that point)
    size = 10
  ) +
  # Y-axis label: keep it inside the figure, slightly right of the left edge
  draw_label(
    labels_prediction[2],
    x = -0.02,   # small positive value keeps it inside the canvas
    y = 0.5,
    angle = 90,
    hjust = 1,  # anchor the right side of the rotated text at x=0.06
    size = 10
  ) +
  # Increase outer margins a little so tick labels + these draw_labels are not clipped
  theme(plot.margin = margin(t = 20, r = 20, b = 40, l = 40))

#combined_plot_xgb_pred

ggsave(here::here("data", "final", "stability", "xgb_plot_prediction_instability.png"),
       plot = combined_plot_xgb_pred,
       width = 15, height = 22, units = "cm",
       dpi = 600,
       bg = "white",  # ensure transparent background doesn't cut edges
       limitsize = FALSE)





## Combined plot for calibration instability (random forest)
combined_plot_xgb_cal <- ggdraw(
  cowplot::plot_grid(
    plotlist  = xgb_cal_plots,
    labels    = paste0("variable set ", LETTERS[1:5]),
    nrow      = 5,
    ncol      = 1,
    label_size = 10,
    label_x   = 0.02,
    label_y   = 0.95
  )
) +
  # X-axis label: keep it inside the figure, slightly above the bottom edge
  draw_label(
    labels_calibration[1],
    x = 0.5,    # centered horizontally
    y = -0.02,
    vjust = 1, 
    size = 10
  ) +
  # Y-axis label: keep it inside the figure, slightly right of the left edge
  draw_label(
    labels_calibration[2],
    x = -0.02,   # small positive value keeps it inside the canvas
    y = 0.5,
    angle = 90,
    hjust = 1,  # anchor the right side of the rotated text at x=0.06
    size = 10
  ) +
  # Increase outer margins a little so tick labels + these draw_labels are not clipped
  theme(plot.margin = margin(t = 20, r = 20, b = 40, l = 40))

combined_plot_xgb_cal

ggsave(here::here("data", "final", "stability", "xgb_plot_calibration_instability.png"),
       plot = combined_plot_xgb_cal,
       width = 15, height = 22, units = "cm",
       dpi = 600,
       bg = "white",  # ensure transparent background doesn't cut edges
       limitsize = FALSE)






##------------------------------------------------------------------------------

## MAPE plots

## RF
mape_plot_rf_E <- all_plots_models_E$rf$plot_mape_inst_model


png(here::here("data", "final", "stability", "MAPE_E_rf.png"),
    width  = 15,
    height = 22,
    units  = "cm",
    res    = 300)
replayPlot(mape_plot_rf_E)
title(main = "Variable set E (All data modalities)\n Algorithm: Random forest",
      xlab = "Predicted Quality of life score", ylab = "MAPE")

dev.off()

mape_plot_xgb_E <- all_plots_models_E$xgb$plot_mape_inst_model


png(here::here("data", "final", "stability", "MAPE_E_xgb.png"),
    width  = 15,
    height = 22,
    units  = "cm",
    res    = 300)
replayPlot(mape_plot_xgb_E)
title(main = "Variable set E (All data modalities)\n Algorithm: XGBoost",
      xlab = "Predicted Quality of life score", ylab = "MAPE")

dev.off()

# Save combined plots to a file
pdf(here::here("data", "final", "stability",
               "combined_mape_plots_rf.pdf"), width = 22/2.54, height = 30/2.54)

# Create an empty plot that will hold everything
plot.new()

# Set up outer margins on this empty plot
par(oma = c(4, 4, 2, 1))

# Now create the multi-panel layout
par(mfrow = c(length(rf_mape_plots), 1),
    mar = c(2, 2, 1, 1))

# Define titles for each panel
panel_titles <- paste0("Variable set ", LETTERS[1:5])  # Adjust as needed

# Replay each plot and add title
for(p in seq_along(rf_mape_plots)){
  replayPlot(rf_mape_plots[[p]])
  # Add title in top-left corner of the plot panel
  mtext(panel_titles[p], side = 3, line = 0, adj = 0, cex = 1.2, font = 2)
}

# Reset to the outer plot to add labels
par(mfrow = c(1, 1),
    new = TRUE,
    oma = c(4, 4, 2, 1),
    mar = c(0, 0, 0, 0))

# Create invisible plot to access outer margins
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

# Add outer labels
mtext("Original prediction", side = 1, outer = TRUE, line = 2.5, cex = 1.2)
mtext("MAPE", side = 2, outer = TRUE, line = 2.5, cex = 1.2)

dev.off()

########

## XGB

# Save combined plots to a file
pdf(here::here("data", "final", "stability",
               "combined_mape_plots_xgb.pdf"), width = 10, height = 12)

# Create an empty plot that will hold everything
plot.new()

# Set up outer margins on this empty plot
par(oma = c(4, 4, 2, 1))

# Now create the multi-panel layout
par(mfrow = c(length(xgb_mape_plots), 1),
    mar = c(2, 2, 1, 1))

# Define titles for each panel
panel_titles <- paste0("Variable set ", LETTERS[1:5])  # Adjust as needed

# Replay each plot and add title
for(p in seq_along(xgb_mape_plots)){
  replayPlot(xgb_mape_plots[[p]])
  # Add title in top-left corner of the plot panel
  mtext(panel_titles[p], side = 3, line = 0, adj = 0, cex = 1.2, font = 2)
}

# Reset to the outer plot to add labels
par(mfrow = c(1, 1),
    new = TRUE,
    oma = c(4, 4, 2, 1),
    mar = c(0, 0, 0, 0))

# Create invisible plot to access outer margins
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

# Add outer labels
mtext("Original prediction", side = 1, outer = TRUE, line = 2.5, cex = 1.2)
mtext("MAPE", side = 2, outer = TRUE, line = 2.5, cex = 1.2)

dev.off()








