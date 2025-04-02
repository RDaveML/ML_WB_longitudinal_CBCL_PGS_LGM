## simulation to plot scatter

pacman::p_load("dplyr", "tidyverse", "haven", "foreign", "here", "readr", 
               "stringr", "readxl", "data.table", "caret", "car", "glmnet",
               "ParBayesianOptimization", "ranger", "e1071", "randomForestSRC",
               "xgboost", "parallel", "doParallel", "fastDummies", "RANN",
               "kernlab", "devtools", "pak", "filelock")

source(here::here("scripts", "functions", "functions_plotting_ML.R"))


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



##-----------------------------------------------------------------------------

## Working with first test run of models (Model A): 
# load(here::here("data", "intermediate", "workspace_sandbox_custom_functions.Rdata"))
workspace_model_A <- readRDS(file = here::here(
  "data" , "intermediate", "workspace_sandbox_custom_functions.rds"
))

workspace_model_0 <- readRDS(file = here::here(
  "data" , "intermediate", "workspace_sandbox_model_0.rds"
))



predictions_basic_train <- list(workspace_model_A$test_rf$preds_rf_train,
                                workspace_model_A$test_svr$preds_svr_train,
                                workspace_model_A$test_xgb$preds_xgb_train)


predictions_basic_test <- list(workspace_model_A$test_rf$preds_rf_test,
                                workspace_model_A$test_svr$preds_svr_test,
                                workspace_model_A$test_xgb$preds_xgb_test)

predictions_basic_train_0 <- list(workspace_model_0$test_rf$preds_rf_train,
                                workspace_model_0$test_svr$preds_svr_train,
                                workspace_model_0$test_xgb$preds_xgb_train)


predictions_basic_test_0 <- list(workspace_model_0$test_rf$preds_rf_test,
                               workspace_model_0$test_svr$preds_svr_test,
                               workspace_model_0$test_xgb$preds_xgb_test)

true_y_basic <- workspace_model_A$data_model_A$QoL_simple

## now, length of predictions align, but in SVR predictions, only NAs!

## Still to be added: The IDs!


## THIS CAN GO LATER, THEN, ITs ONLY ABOUT THE IDs and the 
## multiple prediction objects that will be loaded in with 
## some lapply function (list.files, etc.)

predictions_full <- vector("list", length(predictions_basic_train))
for(set in 1:length(predictions_basic_train)){
  predictions_full[[set]] <- c(predictions_basic_train[[set]],
                               predictions_basic_test[[set]])
}

## next step: add noise 
predictions_noise <- lapply(predictions_full, function(x){
  dataset <- data.frame(original_prediction = x)
  ## adding noise parameters
  noise_params <- list(
    c(0, 1),
    c(0.1, 1),
    c(-0.3, 1),
    c(0.3, 0.7)
  )
  dataset <- dataset %>%
    bind_cols(
      map_dfc(seq_along(noise_params),
              ~ tibble(!!paste0("b", .x) := dataset$original_prediction +
                         rnorm(nrow(dataset), mean = noise_params[[.x]][1],
                               sd = noise_params[[.x]][2])
                       )
              )
      )
    # mutate(b1 = original_prediction + rnorm(1, mean = 0, sd = 1),
    #       b2 = original_prediction + rnorm(1, mean = 0.1, sd = 1),
    #       b3 = original_prediction + rnorm(1, mean = -0.3, sd = 1),
    #       b4 = original_prediction + rnorm(1, mean = 0.3, sd = 0.7))
  return(dataset)
})


predictions_full_0 <- vector("list", length(predictions_basic_train_0))
for(set in 1:length(predictions_basic_train_0)){
  predictions_full_0[[set]] <- c(predictions_basic_train_0[[set]],
                               predictions_basic_test_0[[set]])
}
## next step: add noise 
predictions_noise_0 <- lapply(predictions_full_0, function(x){
  dataset <- data.frame(original_prediction = x)
  ## adding noise parameters
  noise_params <- list(
    c(0, 1),
    c(0.1, 1),
    c(-0.3, 1),
    c(0.3, 0.7)
  )
  dataset <- dataset %>%
    bind_cols(
      map_dfc(seq_along(noise_params),
              ~ tibble(!!paste0("b", .x) := dataset$original_prediction +
                         rnorm(nrow(dataset), mean = noise_params[[.x]][1],
                               sd = noise_params[[.x]][2])
              )
      )
    )
  # mutate(b1 = original_prediction + rnorm(1, mean = 0, sd = 1),
  #       b2 = original_prediction + rnorm(1, mean = 0.1, sd = 1),
  #       b3 = original_prediction + rnorm(1, mean = -0.3, sd = 1),
  #       b4 = original_prediction + rnorm(1, mean = 0.3, sd = 0.7))
  return(dataset)
})

## CONTINUE HERE!!!
## CUSTOM FUNCTION FOR PLOTTING?

## adding true outcome
# xgb_sim$true_y <- c(x_train_ML$QoL_simple, x_test_ML$QoL_simple)

## This actually needs to be coded correctly with a join because it depends on
## the split which values are in training or test set!

## Now, all the plots can be plotted, note that you also need to figure out 
## how to make the correct joins by virtue of the FISNumber link

## make the effort for the "simulated" datasets

## ----------------------------------------------------------------------------
## random forest model
rf1_sim <- predictions_noise[[1]]
rf1_sim$true_y <- true_y_basic
head(rf1_sim)

rf1_sim_0 <- predictions_noise_0[[1]]
rf1_sim_0$true_y <- true_y_basic
head(rf1_sim_0)


## Note: Optionally plotting adjusted predictions (with additionally shrunken
## down predictor space) next to the first level predictions
## Then, add second condition again as shown above (cond2)

## plotting

## 1) prediction instability
example <- FALSE
if(example){
rf1_boot <- rf1_sim %>% 
  select(-any_of(c("original_prediction", "FISNumber")))
x_rf1 <- rf1_sim$original_prediction[order(rf1_sim$original_prediction)]
y1_rf1 <- apply(rf1_boot, 1, function(x) quantile(x, probs = 0.025, na.rm = T))[order(rf1_sim$original_prediction)]
y2_rf1 <- apply(rf1_boot, 1, function(x) quantile(x, probs = 0.975, na.rm = T))[order(rf1_sim$original_prediction)]


xx1_rf1 <- lowess(y1_rf1~x_rf1, delta = 0.3)
xx2_rf1 <- lowess(y2_rf1~x_rf1, delta = 0.3)



OUT3_rf1 <- data.frame(x = c(xx1_rf1$x,
                             xx2_rf1$x), 
                       y = c(xx1_rf1$y,
                             xx2_rf1$y))

# OUT3$Condition <- c(rep("Condition 1",   length(xx1_rf1$x)), 
#                    rep("Condition 2", length(xx1_cond2$x)),
#                    rep("Condition 1",   length(xx2_rf1$x)),
#                    rep("Condition 2", length(xx2_cond2$x))) 

# OUT3$Condition <- factor(OUT3$Condition, levels = c("Condition 1", "Condition 2"))

OUT3_rf1$limit <- c(rep("lower", length(xx1_rf1$x)), 
                    rep("upper", length(xx2_rf1$x)))
OUT3_rf1$limit <- factor(OUT3_rf1$limit, levels = c("lower", "upper"))


# Pivot data to long format
rf1_long <- rf1_sim %>%
  pivot_longer(cols = starts_with("b"), names_to = "bootstrap", values_to = "Scatter")# %>%
  #mutate(Condition = "Condition 1")

## Optional: Readding if multiple conditions
# rf2_long <- rf2_df %>%
#  pivot_longer(cols = starts_with("b"), names_to = "bootstrap", values_to = "Scatter") %>%
#  mutate(Condition = "Condition 2")

## long_data <- bind_rows(rf1_long, cond2_long)
long_data <- rf1_long

ggplot(long_data, aes(x = original_prediction, y = Scatter)) +
  geom_point(size = 0.1, alpha = 0.5, color = "grey") +
  geom_line(data = OUT3_rf1, aes(x=x, y=y, group = limit), colour='black', linetype=2) + ## upper and lower limit
  geom_abline(intercept = 0, slope = 1) + ## unity line
  xlim(0, 12) +
  ylim(0, 12) +
  xlab('Estimated score from the developed model') +
  ylab('Estimated score in the (simulated) bootstrap samples') +
  theme_bw()# +
  #facet_wrap(~ Condition)  # Creates separate plots for Condition 1 and Condition 2
}

## Make this into a function before continuing!

## testing the function
plot_pred_inst(df_pred = rf1_sim)
## works

## model 0
plot_pred_inst(df_pred = rf1_sim_0)

## ----------------------------------------------------------------------------


## Calibration instability (needs true y values)

## testing the function
plot_cal_inst(df_pred = rf1_sim)
## works as well

## model 0
plot_cal_inst(df_pred = rf1_sim_0)

## ----------------------------------------------------------------------------



## MAPE instability

## testing the function
plot_mape_inst(df_pred = rf1_sim)

## model 0
plot_mape_inst(df_pred = rf1_sim_0)



## ----------------------------------------------------------------------------


