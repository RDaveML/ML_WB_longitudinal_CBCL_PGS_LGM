## Demographics Model A NTR participant longitudinal study

## covariate df (not filtered)
load(file = here::here("data", "intermediate", "data_covariates.RData"))

## training and test ids
load(here::here("data", "intermediate", "indices_train.RData"))
temp <- load(here::here("data", "intermediate", "indices_train.RData"))
cat("saved indices of participants in training set loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")

load(here::here("data", "intermediate", "indices_test.RData"))
temp <- load(here::here("data", "intermediate", "indices_test.RData"))
cat("saved indices of participants in training set loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")

load(here::here("scripts", "names_covariates.RData"))
temp <- load(here::here("scripts", "names_covariates.RData"))
cat("vector with names of covariates loaded in; name of object: ", "'", temp, "'",
    "\n", "\n")


demograph_df <- data_covariates %>%
  filter(FISNumber %in% train_ids | FISNumber %in% test_ids)

## correct number of rows

## factor conversion of factor variables
num_covariates <- c(grep("time_lag", covariates_names, value = TRUE),
                    grep("age_qol", covariates_names, value = TRUE))

factor_covariates <- setdiff(covariates_names, num_covariates)

## factor conversion of factor_covariates
for(covariate in factor_covariates){
  demograph_df[, covariate] <- as.factor(demograph_df[, covariate])
}


## factor relabelling:
demograph_df <- demograph_df %>%
  mutate(sex = ifelse(sex == 1,
                      "male",
                      ifelse(sex == 2, "female", "NA / unknown")),
         ea4fa_agg = case_when(
           ea4fa_agg == 1 ~ "lager onderwijs",
           ea4fa_agg == 2 ~ "lbo / lavo / mavo",
           ea4fa_agg == 3 ~ "mbo / havo / vwo",
           ea4fa_agg == 4 ~ "hbo / wo",
           is.na(ea4fa_agg) ~ NA),
         ea4mo_agg = case_when(
           ea4mo_agg == 1 ~ "lager onderwijs",
           ea4mo_agg == 2 ~ "lbo / lavo / mavo",
           ea4mo_agg == 3 ~ "mbo / havo / vwo",
           ea4mo_agg == 4 ~ "hbo / wo",
           is.na(ea4mo_agg) ~ NA),
         age_qol = as.numeric(age_qol)
         )

## plotting demographics

## plotting sex
ggplot(demograph_df, aes(x = sex)) + 
  geom_bar(aes(fill = sex)) + 
  scale_fill_manual(name = "Sex", labels = c("Female", "Male", "NA or no info"),
                    values = c("orange", "blue", "grey")) +
  labs(title = "Distribution of sex in sample", x = "Sex")


## educational attainment mother
color_scale <- scales::seq_gradient_pal("black", "blue", "Lab")(seq(0,1,length.out=5))
ggplot(demograph_df, aes(x = factor(ea4mo_agg, 
                                    c("lager onderwijs", 
                                      "lbo / lavo / mavo",
                                      "mbo / havo / vwo",
                                      "hbo / wo")))) + 
  geom_bar(aes(fill = factor(ea4mo_agg, 
                             c("lager onderwijs", 
                               "lbo / lavo / mavo",
                               "mbo / havo / vwo",
                               "hbo / wo")))) + 
  scale_fill_manual(name = "EA mother", labels = c("lager onderwijs", 
                                                   "lbo / lavo / mavo",
                                                   "mbo / havo / vwo",
                                                   "hbo / wo"),
                    #values = c("orange", "blue", "grey", "red"))
                    #low = "black", high = "blue")
                    values = color_scale) +
  labs(title = "Educational attainment (mother) in sample", x = "EA (mother)")

## educational attainment father
ggplot(demograph_df, aes(x = factor(ea4fa_agg, 
                                    c("lager onderwijs", 
                                      "lbo / lavo / mavo",
                                      "mbo / havo / vwo",
                                      "hbo / wo")))) + 
  geom_bar(aes(fill = factor(ea4fa_agg, 
                             c("lager onderwijs", 
                               "lbo / lavo / mavo",
                               "mbo / havo / vwo",
                               "hbo / wo")))) + 
  scale_fill_manual(name = "EA father", labels = c("lager onderwijs", 
                                                   "lbo / lavo / mavo",
                                                   "mbo / havo / vwo",
                                                   "hbo / wo"),
                    # values = c("orange", "blue", "grey", "red")) +
                    values = color_scale) +
  labs(title = "Educational attainment (father) in sample", x = "EA (father)")



ggplot(demograph_df, aes(x = factor(ea4fa_agg, 
                                    c("lager onderwijs", 
                                      "lbo / lavo / mavo",
                                      "mbo / havo / vwo",
                                      "hbo / wo")))) + 
  geom_bar(aes(fill = factor(ea4fa_agg, 
                             c("lager onderwijs", 
                               "lbo / lavo / mavo",
                               "mbo / havo / vwo",
                               "hbo / wo")))) + 
  scale_fill_manual(name = "EA father", labels = c("lager onderwijs", 
                                                   "lbo / lavo / mavo",
                                                   "mbo / havo / vwo",
                                                   "hbo / wo"),
                    values = c("orange", "blue", "grey", "red")) +
  labs(title = "Educational attainment (father) in sample", x = "EA (father)")
  

## age at quality of life assessment
ggplot(demograph_df, aes(x = age_qol)) + 
  geom_histogram(bins = length(unique(demograph_df$age_qol))) + 
  xlim(c(min(demograph_df$age_qol), max(demograph_df$age_qol))) + 
  labs(title = "Age at QoL assessment", x = "Age")