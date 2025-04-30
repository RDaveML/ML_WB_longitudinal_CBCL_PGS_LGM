## functions for plotting in the longitudinal machine learning study

## (add more descriptive title!)


## negation operator
`%notin%` <- Negate(`%in%`)

# options(scipen = 999, expressions = 50000)


##-----------------------------------------------------------------------------

## A) Bootstrapped assessment of model stability

## plotting prediction instability


plot_pred_inst <- function(df_pred, smooth_function = NULL){
  
#  if(!is.data.frame(df_pred) | 
#     "FISNumber" %notin% colnames(df_pred) | 
#     "true_y" %notin% colnames(df_pred) | 
#     "original_prediction" %notin% colnames(df_pred) | 
#     length(grep("bootstrap", colnames(df_pred))) == 0)
#     {
#    stop("bounds_enet must be a list with elements alpha and lambda")
#  }
  
  ## About the smoothing function, spend separate effort to look it up
  ## if necessary, let it explain by CGPT, adapt function and delta to it
  
  ## CONTINUE HERE WITH THIS ON SOME OTHER DAY
  
  
  ## for each individual's original predicted probability, computing 
  ## 2.5 and 97.5% quantiles from the bootstrapped predictions, giving confidence
  ## band which can also nicely be plotted in the instability plot
  ## create df without ID and without original
  
  df_boot <- df_pred %>% 
    select(-any_of(c("original_prediction", "FISNumber", "true_y")))
  x_1 <- df_pred$original_prediction[order(df_pred$original_prediction)]
  y1_1 <- apply(df_boot, 1, function(x) quantile(x, probs = 0.025, na.rm = T))[order(df_pred$original_prediction)]
  y2_1 <- apply(df_boot, 1, function(x) quantile(x, probs = 0.975, na.rm = T))[order(df_pred$original_prediction)] 
  
  
  xx1_1 <- lowess(y1_1~x_1, delta = 0.3)
  xx2_1 <- lowess(y2_1~x_1, delta = 0.3)
  
  
  
  OUT3_1 <- data.frame(x = c(xx1_1$x,
                             xx2_1$x), 
                       y = c(xx1_1$y,
                             xx2_1$y))
  
  # OUT3$Condition <- c(rep("Condition 1",   length(xx1_1$x)), 
  #                    rep("Condition 2", length(xx1_cond2$x)),
  #                    rep("Condition 1",   length(xx2_1$x)),
  #                    rep("Condition 2", length(xx2_cond2$x))) 
  
  # OUT3$Condition <- factor(OUT3$Condition, levels = c("Condition 1", "Condition 2"))
  
  OUT3_1$limit <- c(rep("lower", length(xx1_1$x)), 
                      rep("upper", length(xx2_1$x)))
  OUT3_1$limit <- factor(OUT3_1$limit, levels = c("lower", "upper"))
  
  
  # Pivot data to long format
  ## adjust this depending on how the columns are named in the final 
  ## version of the bootstrapping script! 
  ## CONTINUE HERE!!! 
  df_long <- df_pred %>%
    pivot_longer(cols = starts_with("b"), names_to = "bootstrap", values_to = "Scatter")# %>%
  #mutate(Condition = "Condition 1")
  
  ## Optional: Re-appending if multiple conditions
  # rf2_long <- rf2_df %>%
  #  pivot_longer(cols = starts_with("b"), names_to = "bootstrap", values_to = "Scatter") %>%
  #  mutate(Condition = "Condition 2")
  
  ## long_data <- bind_rows(1_long, cond2_long)
  long_data <- df_long
  
  
  ## CONTINUE HERE! FEED MODEL NAME INTO TITLE, NEEDS TO BE FUNCTION ARGUMENT
  
  plot_instability <- 
  ggplot(long_data, aes(x = original_prediction, y = Scatter)) +
    geom_point(size = 0.1, alpha = 0.5, color = "grey") +
    geom_line(data = OUT3_1, aes(x=x, y=y, group = limit), colour='black', linetype=2) + ## upper and lower limit
    geom_abline(intercept = 0, slope = 1) + ## unity line
    xlim(0, 12) +
    ylim(0, 12) +
    xlab('Estimated score from the developed model') +
    ylab('Estimated score in the (simulated) bootstrap samples') +
    theme_bw()# +
  #facet_wrap(~ Condition)  # Creates separate plots for Condition 1 and Condition 2
  
  ## rendering plot
  # plot_instability
  
  return(plot_instability = plot_instability)
  
}


##-----------------------------------------------------------------------------

  ## calibration instability plotting function

plot_cal_inst <- function(df_pred, smooth_function = NULL){
  
  
  ## CONTINUE HERE!!!
  ## df_pred <- rf1_sim
  ## x axis: predictions for original and bootstrapped models
  df_melt_xx <- reshape2::melt(df_pred %>% select(-true_y))
  
  ## y-axis: True values
  df_melt_yy <- reshape2::melt(df_pred %>% select(true_y))
  
  ## adding the correct group to all individual values
  # cond1_df_melt_xx$N <- rep("Condition 1",   nrow(cond1_df_melt_xx))
  # cond2_df_melt_xx$N   <- rep("Condition 2", nrow(cond2_df_melt_xx))
  
  ## combining both xx
  # cal <- rbind(cond1_df_melt_xx,
  #              cond2_df_melt_xx)
  
  colnames(df_melt_xx) <- c("prediction", "value")
  # cal$Condition         <- factor(cal$Condition, labels = c("Condition 1", "Condition 2"))
  
  ## combining with y: The true values
  
  ## This one still needs to be adjusted, the numbers of rows need to match!
  
  ## adjust with the rows here!!! 
  ## CONTINUE HERE
  
  ## this might be it, check back at the original code from Riley
  OUT <- data.table(df_melt_xx)
  OUT[, y:= rep(df_melt_yy$value, (ncol(df_pred) - 1))]
  
  
  ## renaming column: 
  OUT <- OUT %>%
    mutate(prediction = ifelse(prediction == "original_prediction",
                               "Original",
                               "Bootstrapped"))
  
  ## Plotting
  plot_instability_cal <- 
  OUT %>% ggplot(aes(x = value, y = y, group = prediction, color = prediction)) +
    geom_line(alpha = 0.5) + 
    #facet_grid(~Condition) + 
    xlim(0, 10) + 
    ylim(0, 10) +
    xlab('predicted') +
    ylab('observed') +
    geom_abline(slope = 1, intercept = 0) +
    theme_bw() +
    theme(axis.text = element_text(size = 6))
  
  ## render plot
  plot_instability_cal
  
  return(plot_instability_cal = plot_instability_cal)
  
}

## function also seems to work, still checking the alignment of y values
## at line 126



##-----------------------------------------------------------------------------

## MAPE instability plotting function

plot_mape_inst <- function(df_pred, smooth_function = NULL){
  ## MAPE here: mean absolute difference between the bootstrap model predictions
  ## and the original model prediction
  # par(mfrow=c(1, 2))
  # Determine common axis limits
  x_lim <- range(df_pred$original_prediction, na.rm = TRUE)
  y_lim <- range(
    apply(abs(df_pred %>% select(-any_of(c("original_prediction",
                                          "FISNumber"))) - df_pred$original_prediction),
              1, median, na.rm = TRUE), na.rm = TRUE)
  
  # Plot with fixed axis limits
  
  ## linter this properly, for heaven's sake! 
  
  
  # Open a new plot device
  dev.new() # Ensures the plot is rendered on a new device
  
  plot(df_pred$original_prediction, 
       apply(abs(df_pred %>% select(-any_of(c("original_prediction", "FISNumber"))) - 
                   df_pred$original_prediction),
             1, median, na.rm = TRUE), 
       pch = 20, xlim = x_lim, ylim = y_lim, main = "MAPE instability")
  ## also adapt axis labels
  
  #plot(cond2_df$Original, 
  #     apply(abs(cond2_df %>% select(-Original, -ID) - cond2_df$Original), 1, median, na.rm = TRUE), 
  #     pch = 20, xlim = x_lim, ylim = y_lim, main = "Condition 2")
  
  # Capture the plot as an object
  plot_obj <- recordPlot()
  
  dev.off()
  # Return the recorded plot object
  return(plot_obj)
  
  
}

