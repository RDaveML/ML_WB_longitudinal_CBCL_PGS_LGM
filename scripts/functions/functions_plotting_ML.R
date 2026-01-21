## functions for plotting prediction-, calibration and MAPE stability
## plots

## Note: Essential parts of the functions were adapted 
## from Riley & Collins (2023)
## to be found under https://github.com/gscollins1973/Instability


## negation operator
`%notin%` <- Negate(`%in%`)

# options(scipen = 999, expressions = 50000)


##-----------------------------------------------------------------------------

## A) Bootstrapped assessment of model stability

## plotting prediction instabilityt aking in as input
## a dataframe that contains several bootstrapped predictions of a machine 
## learning model
plot_pred_inst <- function(df_pred, smooth_function = NULL){
  
  
  ## for each individual's original predicted probability, computing 
  ## 2.5 and 97.5% quantiles from the bootstrapped predictions, giving confidence
  ## band which can also nicely be plotted in the instability plot
  ## create df without ID and without original
  
  df_boot <- df_pred %>% 
    select(-any_of(c("original_prediction", "FISNumber", "true_y")))
  x_1 <- df_pred$original_prediction[order(df_pred$original_prediction)]
  y1_1 <- apply(df_boot, 1, function(x){
    quantile(x, probs = 0.025, na.rm = T)})[order(df_pred$original_prediction)]

  y2_1 <- apply(df_boot, 1, function(x){
    quantile(x, probs = 0.975, na.rm = T)})[order(df_pred$original_prediction)] 
  
  
  ## lowess function to smooth lines in plot
  xx1_1 <- lowess(y1_1~x_1, delta = 0.3)
  xx2_1 <- lowess(y2_1~x_1, delta = 0.3)
  
  
  
  OUT3_1 <- data.frame(x = c(xx1_1$x,
                             xx2_1$x), 
                       y = c(xx1_1$y,
                             xx2_1$y))
  
  
  OUT3_1$limit <- c(rep("lower", length(xx1_1$x)), 
                      rep("upper", length(xx2_1$x)))
  OUT3_1$limit <- factor(OUT3_1$limit, levels = c("lower", "upper"))
  
  
  # Pivot data to long format for plotting
  df_long <- df_pred %>%
    pivot_longer(cols = starts_with("b"),
                 names_to = "bootstrap", values_to = "Scatter")
  ## in plotting script, all predictions are combined, columns get the 
  ## name prefix "bootstrapped_prediction"
  
  long_data <- df_long
  
  
  ## creating the instability plot with CIs and smoothed lines
  plot_instability <- 
  ggplot(long_data, aes(x = original_prediction, y = Scatter)) +
    geom_point(size = 0.1, alpha = 0.5, color = "grey") +
    geom_line(data = OUT3_1, aes(x=x, y=y, group = limit),
              colour='black', linetype=2) + ## upper and lower limit
    geom_abline(intercept = 0, slope = 1) + ## unity line
    xlim(0, 12) +
    ylim(0, 12) +
    xlab('Estimated score from the developed model') +
    ylab('Estimated score in the (simulated) bootstrap samples') +
    theme_bw()
  
  return(plot_instability = plot_instability)
  
}

##-----------------------------------------------------------------------------

## calibration instability plotting function taking in as input
## a dataframe that contains several bootstrapped predictions of a machine 
## learning model
## rounding advised to be kept at FALSE as predictions also were not rounded
## in the model performance analysis

plot_cal_inst <- function(df_pred, round = FALSE){
  
  ## for plotting: rounding introducing
  if(round){
    ## rounding all the columns that contain the string "prediction"
    ## in column name to whole integers
    df_pred <- df_pred %>%
      mutate(across(.cols = contains("prediction"),
                    .fns = ~ round(.)))
  }
  
  ## Remove true_y column for melting
  pred_only <- df_pred %>% select(-true_y)
  
  ## Create an identifier for each prediction column
  pred_long <- reshape2::melt(pred_only,
                              variable.name = "model_id", value.name = "value")
  
  ## Repeat true_y for each prediction column
  pred_long$y <- rep(df_pred$true_y, times = ncol(pred_only))
  
  ## Identify original vs bootstrapped
  pred_long$prediction <- ifelse(
    pred_long$model_id == "original_prediction", "Original", "Bootstrapped")
  
  ## Apply lowess smoothing to each column individually
  smoothed_list <- pred_long %>%
    group_by(model_id, prediction) %>%
    group_map(~{
      df_sorted <- .x[order(.x$value), ]
      sm <- lowess(df_sorted$value, df_sorted$y, f = 2/3)
      tibble(value = sm$x, y = sm$y, model_id = .y$model_id,
             prediction = .y$prediction)
    }) %>% 
    bind_rows()
  
  ## Plot
  plot_instability_cal <- ggplot(smoothed_list,
                                 aes(x = value, y = y, group = model_id,
                                     color = prediction)) +
    geom_line(aes(linetype = prediction,
                  size = prediction, alpha = prediction)) +
    scale_size_manual(values = c("Bootstrapped" = 0.5, "Original" = 1.5)) +
    scale_alpha_manual(values = c("Bootstrapped" = 0.5, "Original" = 2)) +
    ## highlighting original predictions in red
    scale_color_manual(
      values = c("Bootstrapped" = "grey70", "Original" = "red")) + 
    ## scale limits of CL: 0 - 10
    scale_x_continuous(
      name = "predicted", limits = c(0, 10), breaks = seq(0, 10, 1)) +
    scale_y_continuous(
      name = "observed", limits = c(0, 10), breaks = seq(0, 10, 1)) +
    geom_abline(slope = 1, intercept = 0) + ## unity line
    theme_bw() +
    theme(axis.text = element_text(size = 6))
  
  return(plot_instability_cal)
}

##-----------------------------------------------------------------------------

## MAPE instability plotting function

## plotting MAPE instability plot, taking in as input
## a dataframe that contains several bootstrapped predictions of a machine 
## learning model

## note: this function uses base R plotting, combining and assembling multi
## facet plots less straightforward

plot_mape_inst <- function(df_pred, smooth_function = NULL){
  ## MAPE here: mean absolute difference between the bootstrap model predictions
  ## and the original model prediction
  
  # Determine common axis limits
  x_lim <- range(c(0, 10))
  y_lim <- range(
    apply(abs(df_pred %>% select(-any_of(c("original_prediction",
                                          "FISNumber"))) - 
                df_pred$original_prediction),
              1, median, na.rm = TRUE), na.rm = TRUE)
  
  # Plot with fixed axis limits

  ## base R plotting device
  if (dev.cur() > 1) {dev.off()}  # Close if a device is open
  dev.new()  # Open a fresh device

  plot(df_pred$original_prediction,
       apply(abs(df_pred %>%
                   select(-any_of(c("original_prediction", "FISNumber"))) -
                    df_pred$original_prediction),
             1, median, na.rm = TRUE),
       pch = 20, xlim = x_lim, ylim = y_lim,
       ## title elements optional, left out here as 
       ## easier to manipulate in composite plot, if desired, enable these 
       ## arguments
       # main = "MAPE instability",
       # xlab = "Original prediction",
       xlab = NULL,
       # ylab = "MAPE")
       ylab = NULL,
       ann=FALSE)

  # Capture the plot as an object
  plot_obj <- recordPlot()

  # Return the recorded plot object
  return(plot_obj)
  
}

## eoS
