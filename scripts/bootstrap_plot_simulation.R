## simulation to plot scatter

pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "caret", "car", "glmnet",
               "ParBayesianOptimization", "ranger", "e1071", "randomForestSRC",
               "xgboost", "parallel", "doParallel", "fastDummies", "RANN",
               "kernlab", "devtools", "pak", "filelock")


simulation <- TRUE
## simulating real data and predicted data
if(simulation){
  set.seed(123)  # For reproducibility
  # n <- 5000  # Number of rows
  n <- 100
  p <- 100  # Number of additional columns
  
  # Generate the first column with integer values between 0 and 10, mean around 7
  first_column <- pmin(pmax(rnorm(n, mean = 7, sd = 1), 0), 10)
  
  # Condition 1: Small variation
  cond1_df <- as.data.frame(matrix(NA, nrow = n, ncol = p + 1))
  colnames(cond1_df) <- c("Original", paste0("Var", 1:p))
  cond1_df$Original <- first_column
  for (i in 2:(p + 1)) {
    cond1_df[, i] <- pmin(pmax(first_column + rnorm(n, mean = 0, sd = 0.5), 0), 10)
  }
  
  # Condition 2: Stronger variation
  cond2_df <- as.data.frame(matrix(NA, nrow = n, ncol = p + 1))
  colnames(cond2_df) <- c("Original", paste0("Var", 1:p))
  cond2_df$Original <- first_column
  for (i in 2:(p + 1)) {
    cond2_df[, i] <- pmin(pmax(first_column + rnorm(n, mean = 0, sd = 2), 0), 10)
  }
  
  cond1_df$ID <- c(1:n)
  cond2_df$ID <- c(1:n)
  
  ## create df without ID and without original
  cond1_boot <- cond1_df %>% 
    select(-Original, -ID)
  
  cond2_boot <- cond2_df %>% 
    select(-Original, -ID)
  
  
  # View first few rows
  head(cond1_df)
  head(cond2_df)
  
  ## for each individual's original predicted probability, computing 
  ## 2.5 and 97.5% quantiles from the bootstrapped predictions, giving confidence
  ## band which can also nicely be plotted in the instability plot
  x_cond1 <- cond1_df$Original[order(cond1_df$Original)]
  x_cond2 <- cond2_df$Original[order(cond2_df$Original)]
  y1_cond1 <- apply(cond1_boot, 1, function(x) quantile(x, probs = 0.025, na.rm = T))[order(cond1_df$Original)]
  y1_cond2 <- apply(cond2_boot, 1, function(x) quantile(x, probs = 0.025, na.rm = T))[order(cond2_df$Original)]
  y2_cond1 <- apply(cond1_boot, 1, function(x) quantile(x, probs = 0.975, na.rm = T))[order(cond1_df$Original)]
  y2_cond2 <- apply(cond2_boot, 1, function(x) quantile(x, probs = 0.975, na.rm = T))[order(cond2_df$Original)]
  
  
  xx1_cond1 <- lowess(y1_cond1~x_cond1, delta = 0.3)
  xx1_cond2 <- lowess(y1_cond2~x_cond2, delta = 0.3)
  xx2_cond1 <- lowess(y2_cond1~x_cond1, delta = 0.3)
  xx2_cond2 <- lowess(y2_cond2~x_cond2, delta = 0.3)
  
  OUT3 <- data.frame(x = c(xx1_cond1$x, xx1_cond2$x,
                           xx2_cond1$x, xx2_cond2$x), 
                     y = c(xx1_cond1$y, xx1_cond2$y,
                           xx2_cond1$y, xx2_cond2$y))
  
  OUT3$Condition <- c(rep("Condition 1",   length(xx1_cond1$x)), 
                      rep("Condition 2", length(xx1_cond2$x)),
                      rep("Condition 1",   length(xx2_cond1$x)),
                      rep("Condition 2", length(xx2_cond2$x))) 
  
  OUT3$Condition <- factor(OUT3$Condition, levels = c("Condition 1", "Condition 2"))
  
  OUT3$limit <- c(rep("lower", length(xx1_cond1$x) + length(xx1_cond2$x)), 
                  rep("upper", length(xx2_cond1$x) + length(xx2_cond2$x)))
  OUT3$limit <- factor(OUT3$limit, levels = c("lower", "upper"))
  
  
  # Pivot data to long format
  cond1_long <- cond1_df %>%
    pivot_longer(cols = starts_with("Var"), names_to = "Variable", values_to = "Scatter") %>%
    mutate(Condition = "Condition 1")
  
  cond2_long <- cond2_df %>%
    pivot_longer(cols = starts_with("Var"), names_to = "Variable", values_to = "Scatter") %>%
    mutate(Condition = "Condition 2")
  
  long_data <- bind_rows(cond1_long, cond2_long)
  
  ggplot(long_data, aes(x = Original, y = Scatter)) +
    geom_point(size = 0.1, alpha = 0.5, color = "grey") +
    geom_line(data = OUT3, aes(x=x, y=y, group = limit), colour='black', linetype=2) + ## upper and lower limit
    geom_abline(intercept = 0, slope = 1) + ## unity line
    xlim(0, 10) +
    ylim(0, 10) +
    xlab('Estimated risk from the developed model') +
    ylab('Estimated risk in the bootstrap samples') +
    theme_bw() +
    facet_wrap(~ Condition)  # Creates separate plots for Condition 1 and Condition 2
  
}


## Perfect! This is the code I need for plotting the prediction instability plot



## MAPE instability plot: 

## MAPE here: mean absolute difference between the bootstrap model predictions
## and the original model prediction
par(mfrow=c(1, 2))
# Determine common axis limits
x_lim <- range(c(cond1_df$Original, cond2_df$Original), na.rm = TRUE)
y_lim <- range(c(
  apply(abs(cond1_df %>% select(-Original, -ID) - cond1_df$Original), 1, median, na.rm = TRUE),
  apply(abs(cond2_df %>% select(-Original, -ID) - cond2_df$Original), 1, median, na.rm = TRUE)
), na.rm = TRUE)

# Plot with fixed axis limits
plot(cond1_df$Original, 
     apply(abs(cond1_df %>% select(-Original, -ID) - cond1_df$Original), 1, median, na.rm = TRUE), 
     pch = 20, xlim = x_lim, ylim = y_lim, main = "Condition 1")

plot(cond2_df$Original, 
     apply(abs(cond2_df %>% select(-Original, -ID) - cond2_df$Original), 1, median, na.rm = TRUE), 
     pch = 20, xlim = x_lim, ylim = y_lim, main = "Condition 2")

## Even in this simulated example, we can see much higher MAPE when more noise 
## in data! 


## Great! So this is the Code I need for the MAPE instability



## Next up: Calibration Instability plot

## adding arbitrary truth as mean value of all predictions
cond1_df <- cond1_df %>%
  mutate(true_y = rowMeans(select(cond1_df, -ID), na.rm = TRUE))

cond2_df <- cond2_df %>%
  mutate(true_y = rowMeans(select(cond2_df, -ID), na.rm = TRUE))

cond1_df$ID <- NULL
cond2_df$ID <- NULL

## x axis: predictions for original and bootstrapped models
cond1_df_melt_xx <- reshape2::melt(cond1_df %>% select(-true_y))

cond2_df_melt_xx <- reshape2::melt(cond2_df %>% select(-true_y))

## y-axis: True values
cond1_df_melt_yy <- reshape2::melt(cond1_df %>% select(true_y))

cond2_df_melt_yy <- reshape2::melt(cond2_df %>% select(true_y))

## adding the correct group to all individual values
cond1_df_melt_xx$N <- rep("Condition 1",   nrow(cond1_df_melt_xx))
cond2_df_melt_xx$N   <- rep("Condition 2", nrow(cond2_df_melt_xx))

## combining both xx
cal <- rbind(cond1_df_melt_xx,
             cond2_df_melt_xx)

colnames(cal) <- c("prediction", "value", "Condition")
cal$Condition         <- factor(cal$Condition, labels = c("Condition 1", "Condition 2"))

## combining with y: The true values

## This one still needs to be adjusted, the numbers of rows need to match!
OUT.cal <- data.table(cal)
OUT.cal[, y:=c(rep(cond1_df_melt_yy$value, (nrow(cond1_df_melt_yy) + 1)),
               rep(cond2_df_melt_yy$value, (nrow(cond2_df_melt_yy) + 1)))]


## renaming column: 
OUT.cal <- OUT.cal %>%
  mutate(prediction = ifelse(prediction == "Original",
                             "Original",
                             "Bootstrapped"))

## Plotting
OUT.cal %>% ggplot(aes(x = value, y = y, group = prediction, color = prediction)) +
  geom_line(alpha = 0.5) + 
  facet_grid(~Condition) + 
  xlim(0, 10) + 
  ylim(0, 10) +
  xlab('predicted') +
  ylab('observed') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  theme(axis.text = element_text(size = 6))

## Perfect! This is what I was looking for! Sure, there needs to be some tuning
## regarding the number of rows and the plotting, but the skeleton is clear! 

##-----------------------------------------------------------------------------

## Now, simulation with N = 5000, roughly the sample size I have
simulation <- TRUE
## simulating real data and predicted data
if(simulation){
  set.seed(123)  # For reproducibility
  # n <- 5000  # Number of rows
  n <- 5000
  p <- 100  # Number of additional columns
  
  # Generate the first column with integer values between 0 and 10, mean around 7
  first_column <- pmin(pmax(rnorm(n, mean = 7, sd = 1), 0), 10)
  
  # Condition 1: Small variation
  cond1_df <- as.data.frame(matrix(NA, nrow = n, ncol = p + 1))
  colnames(cond1_df) <- c("Original", paste0("Var", 1:p))
  cond1_df$Original <- first_column
  for (i in 2:(p + 1)) {
    cond1_df[, i] <- pmin(pmax(first_column + rnorm(n, mean = 0, sd = 0.5), 0), 10)
  }
  
  # Condition 2: Stronger variation
  cond2_df <- as.data.frame(matrix(NA, nrow = n, ncol = p + 1))
  colnames(cond2_df) <- c("Original", paste0("Var", 1:p))
  cond2_df$Original <- first_column
  for (i in 2:(p + 1)) {
    cond2_df[, i] <- pmin(pmax(first_column + rnorm(n, mean = 0, sd = 2), 0), 10)
  }
  
  cond1_df$ID <- c(1:n)
  cond2_df$ID <- c(1:n)
  
  ## create df without ID and without original
  cond1_boot <- cond1_df %>% 
    select(-Original, -ID)
  
  cond2_boot <- cond2_df %>% 
    select(-Original, -ID)
  
  
  # View first few rows
  head(cond1_df)
  head(cond2_df)
  
  ## for each individual's original predicted probability, computing 
  ## 2.5 and 97.5% quantiles from the bootstrapped predictions, giving confidence
  ## band which can also nicely be plotted in the instability plot
  x_cond1 <- cond1_df$Original[order(cond1_df$Original)]
  x_cond2 <- cond2_df$Original[order(cond2_df$Original)]
  y1_cond1 <- apply(cond1_boot, 1, function(x) quantile(x, probs = 0.025, na.rm = T))[order(cond1_df$Original)]
  y1_cond2 <- apply(cond2_boot, 1, function(x) quantile(x, probs = 0.025, na.rm = T))[order(cond2_df$Original)]
  y2_cond1 <- apply(cond1_boot, 1, function(x) quantile(x, probs = 0.975, na.rm = T))[order(cond1_df$Original)]
  y2_cond2 <- apply(cond2_boot, 1, function(x) quantile(x, probs = 0.975, na.rm = T))[order(cond2_df$Original)]
  
  
  xx1_cond1 <- lowess(y1_cond1~x_cond1, delta = 0.3)
  xx1_cond2 <- lowess(y1_cond2~x_cond2, delta = 0.3)
  xx2_cond1 <- lowess(y2_cond1~x_cond1, delta = 0.3)
  xx2_cond2 <- lowess(y2_cond2~x_cond2, delta = 0.3)
  
  OUT3 <- data.frame(x = c(xx1_cond1$x, xx1_cond2$x,
                           xx2_cond1$x, xx2_cond2$x), 
                     y = c(xx1_cond1$y, xx1_cond2$y,
                           xx2_cond1$y, xx2_cond2$y))
  
  OUT3$Condition <- c(rep("Condition 1",   length(xx1_cond1$x)), 
                      rep("Condition 2", length(xx1_cond2$x)),
                      rep("Condition 1",   length(xx2_cond1$x)),
                      rep("Condition 2", length(xx2_cond2$x))) 
  
  OUT3$Condition <- factor(OUT3$Condition, levels = c("Condition 1", "Condition 2"))
  
  OUT3$limit <- c(rep("lower", length(xx1_cond1$x) + length(xx1_cond2$x)), 
                  rep("upper", length(xx2_cond1$x) + length(xx2_cond2$x)))
  OUT3$limit <- factor(OUT3$limit, levels = c("lower", "upper"))
  
  
  # Pivot data to long format
  cond1_long <- cond1_df %>%
    pivot_longer(cols = starts_with("Var"), names_to = "Variable", values_to = "Scatter") %>%
    mutate(Condition = "Condition 1")
  
  cond2_long <- cond2_df %>%
    pivot_longer(cols = starts_with("Var"), names_to = "Variable", values_to = "Scatter") %>%
    mutate(Condition = "Condition 2")
  
  long_data <- bind_rows(cond1_long, cond2_long)
  
  ggplot(long_data, aes(x = Original, y = Scatter)) +
    geom_point(size = 0.1, alpha = 0.5, color = "grey") +
    geom_line(data = OUT3, aes(x=x, y=y, group = limit), colour='black', linetype=2) + ## upper and lower limit
    geom_abline(intercept = 0, slope = 1) + ## unity line
    xlim(0, 10) +
    ylim(0, 10) +
    xlab('Estimated risk from the developed model') +
    ylab('Estimated risk in the bootstrap samples') +
    theme_bw() +
    facet_wrap(~ Condition)  # Creates separate plots for Condition 1 and Condition 2
  
}


## Perfect! This is the code I need for plotting the prediction instability plot



## MAPE instability plot: 

## MAPE here: mean absolute difference between the bootstrap model predictions
## and the original model prediction
par(mfrow=c(1, 2))
# Determine common axis limits
x_lim <- range(c(cond1_df$Original, cond2_df$Original), na.rm = TRUE)
y_lim <- range(c(
  apply(abs(cond1_df %>% select(-Original, -ID) - cond1_df$Original), 1, median, na.rm = TRUE),
  apply(abs(cond2_df %>% select(-Original, -ID) - cond2_df$Original), 1, median, na.rm = TRUE)
), na.rm = TRUE)

# Plot with fixed axis limits
plot(cond1_df$Original, 
     apply(abs(cond1_df %>% select(-Original, -ID) - cond1_df$Original), 1, median, na.rm = TRUE), 
     pch = 20, xlim = x_lim, ylim = y_lim, main = "Condition 1")

plot(cond2_df$Original, 
     apply(abs(cond2_df %>% select(-Original, -ID) - cond2_df$Original), 1, median, na.rm = TRUE), 
     pch = 20, xlim = x_lim, ylim = y_lim, main = "Condition 2")

## Even in this simulated example, we can see much higher MAPE when more noise 
## in data! 


## Great! So this is the Code I need for the MAPE instability



## Next up: Calibration Instability plot

## adding arbitrary truth as mean value of all predictions
cond1_df <- cond1_df %>%
  mutate(true_y = rowMeans(select(cond1_df, -ID), na.rm = TRUE))

cond2_df <- cond2_df %>%
  mutate(true_y = rowMeans(select(cond2_df, -ID), na.rm = TRUE))

cond1_df$ID <- NULL
cond2_df$ID <- NULL

## x axis: predictions for original and bootstrapped models
cond1_df_melt_xx <- reshape2::melt(cond1_df %>% select(-true_y))

cond2_df_melt_xx <- reshape2::melt(cond2_df %>% select(-true_y))

## y-axis: True values
cond1_df_melt_yy <- reshape2::melt(cond1_df %>% select(true_y))

cond2_df_melt_yy <- reshape2::melt(cond2_df %>% select(true_y))

## adding the correct group to all individual values
cond1_df_melt_xx$N <- rep("Condition 1",   nrow(cond1_df_melt_xx))
cond2_df_melt_xx$N   <- rep("Condition 2", nrow(cond2_df_melt_xx))

## combining both xx
cal <- rbind(cond1_df_melt_xx,
             cond2_df_melt_xx)

colnames(cal) <- c("prediction", "value", "Condition")
cal$Condition         <- factor(cal$Condition, labels = c("Condition 1", "Condition 2"))

## combining with y: The true values

## This one still needs to be adjusted, the numbers of rows need to match!

## adjust with the rows here!!! 
## CONTINUE HERE
OUT.cal <- data.table(cal)
OUT.cal[, y:=c(rep(cond1_df_melt_yy$value, (nrow(cond1_df_melt_yy))),
               rep(cond2_df_melt_yy$value, (nrow(cond2_df_melt_yy))))]


## renaming column: 
OUT.cal <- OUT.cal %>%
  mutate(prediction = ifelse(prediction == "Original",
                             "Original",
                             "Bootstrapped"))

## Plotting
OUT.cal %>% ggplot(aes(x = value, y = y, group = prediction, color = prediction)) +
  geom_line(alpha = 0.5) + 
  facet_grid(~Condition) + 
  xlim(0, 10) + 
  ylim(0, 10) +
  xlab('predicted') +
  ylab('observed') +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  theme(axis.text = element_text(size = 6))

