## plotting and table formatting of the analyses in script 
## 27_ML_model_comparison_significance_testing.R


## pairwise comparison lmer 

emm_alg_set <- readRDS(here::here("data", "final", "comparison_lmer.rds"))
## make apa table out of full comparison table
full_contrast_comparison <- as.data.frame(
  pairs(emm_alg_set, adjust = "fdr")) %>%
  mutate(contrast = str_replace_all(contrast, 
                                    c("lm_stack" = "LM Ensemble",
                                      "rf" = "Random Forest",
                                      "xgb" = "XGBoost")),
         p.value = round(p.value, 4),                            
         p.value = case_when(
           p.value <= 0.001 ~ paste0("< 0.001", " ***"),
           p.value <= 0.01 ~ paste0(p.value, " **"),
           p.value <= 0.05 ~ paste0(p.value, " *"),
           TRUE ~ as.character(p.value)),
         estimate = round(estimate, 3),
         SE = round(SE, 3),
         t.ratio = round(t.ratio, 3))

## to APA table (flextable)
contrast_flextable <- 
  full_contrast_comparison %>%
  flextable() %>%
  set_header_labels(
    contrast = "Comparison",
    estimate = "Estimate",
    SE = "Standard Error",
    df = "df",
    t.ratio = "tval",
    p.value = "pval"
  ) %>%
  # theme_vanilla() %>%
  fontsize(size = 12, part = "all") %>%
  bold(part = "header") %>%
  autofit()

contrast_flextable

## creating latex code for tex document
caption_ltx <- 
  "Pairwise comparisons of model performance between algorithms and feature sets"
contrast_latex <- kable(full_contrast_comparison,
                        format = "latex",
                        booktabs = TRUE, 
                        caption = caption_ltx,
                        align = c("l", "c", "c", "c", "c", "c")) %>%
  kable_styling(latex_options = c("hold_position"))

contrast_latex

############################################################################

## only significant comparisons
## pairwise comparison lmer 

sig_contrast_comparison <- as.data.frame(
  pairs(emm_alg_set, adjust = "fdr")) %>%
  filter(p.value < 0.05) %>%
  mutate(contrast = str_replace_all(contrast, 
                                    c("lm_stack" = "LM Ensemble",
                                      "rf" = "Random Forest",
                                      "xgb" = "XGBoost")),
         p.value = round(p.value, 4),                            
         p.value = case_when(
           p.value <= 0.001 ~ paste0("< 0.001", " ***"),
           p.value <= 0.01 ~ paste0(p.value, " **"),
           p.value <= 0.05 ~ paste0(p.value, " *"),
           TRUE ~ as.character(p.value)),
         estimate = round(estimate, 3),
         SE = round(SE, 3),
         t.ratio = round(t.ratio, 3))

## to APA table (flextable)
contrast_flextable_sig <- 
  sig_contrast_comparison %>%
  flextable() %>%
  set_header_labels(
    contrast = "Comparison",
    estimate = "Estimate",
    SE = "Standard Error",
    df = "df",
    t.ratio = "tval",
    p.value = "pval"
  ) %>%
  # theme_vanilla() %>%
  fontsize(size = 12, part = "all") %>%
  bold(part = "header") %>%
  autofit()

contrast_flextable_sig

## creating latex code for tex document
caption_ltx_sig <- 
  "Significant pairwise comparisons of model performance between algorithms and feature sets"
contrast_latex_sig <- kable(sig_contrast_comparison,
                        format = "latex",
                        booktabs = TRUE, 
                        caption = caption_ltx_sig,
                        align = c("l", "c", "c", "c", "c", "c")) %>%
  kable_styling(latex_options = c("hold_position"))

contrast_latex_sig

###############################################################################

## only significant comparisons between the pairs with the same algorithms
alg_constant_sig <- readRDS(
  here::here("data", "final", "comparison_lmer_sig_alg.rds"))

sig_contrast_comparison_pairs_alg <- as.data.frame(alg_constant_sig) %>%
  mutate(contrast = str_replace_all(contrast, 
                                    c("lm_stack" = "LM Ensemble",
                                      "rf" = "Random Forest",
                                      "xgb" = "XGBoost")),
         p.value = round(p.value, 4),                            
         p.value = case_when(
           p.value <= 0.001 ~ paste0("< 0.001", " ***"),
           p.value <= 0.01 ~ paste0(p.value, " **"),
           p.value <= 0.05 ~ paste0(p.value, " *"),
           TRUE ~ as.character(p.value)),
         estimate = round(estimate, 3),
         SE = round(SE, 3),
         t.ratio = round(t.ratio, 3))

## to APA table (flextable)
contrast_flextable_sig_pairs_alg <- 
  sig_contrast_comparison_pairs_alg %>%
  flextable() %>%
  set_header_labels(
    contrast = "Comparison",
    estimate = "Estimate",
    SE = "Standard Error",
    df = "df",
    t.ratio = "tval",
    p.value = "pval"
  ) %>%
  # theme_vanilla() %>%
  fontsize(size = 12, part = "all") %>%
  bold(part = "header") %>%
  autofit()

contrast_flextable_sig_pairs_alg

## creating latex code for tex document
caption_ltx_sig_pairs_alg <- 
  "Significant pairwise comparisons of model performance variable sets (within algorithm)"
contrast_latex_sig_pairs_alg <- kable(sig_contrast_comparison_pairs_alg,
                                      format = "latex",
                                      booktabs = TRUE, 
                                      caption = caption_ltx_sig_pairs_alg,
                                      align = c("l", "c", "c", "c", "c", "c")) %>%
  kable_styling(latex_options = c("hold_position"))

contrast_latex_sig_pairs_alg

###############################################################################


## model performance tables and plots
###############################################################################

## APA tables and plotting
model_performance_df <- readRDS(
  file = here::here("data", "final", "model_performance_df_OG.rds"))

## Formatting to APH style table
model_performance_APH <- model_performance_df

model_descriptions <- c(
  "A (Only CBCL)",
  "B (Only PGS)",
  "C (CBCL + PGS)",
  "D (CBCL + Long)",
  "E (CBCL + Long + PGS)"
)

levels(model_performance_APH$feature_set) <- model_descriptions

algs <- c("Stacked LM", "Random Forest", "XGBoost")

levels(model_performance_APH$algorithm) <- algs

model_performance_APH <- model_performance_APH %>%
  mutate(
    ## values should always contain 2 decimals, also if they are 1.1 or 1 
    RMSE = sprintf("%.2f", round(rmse_est, 2)),
    R2 = sprintf("%.2f", round(r2_est, 2)),
    MAE = sprintf("%.2f", round(mae_est, 2)), 
    RMSE_CI_lower = sprintf("%.2f", round(rmse_ci_lower, 2)),
    RMSE_CI_upper = sprintf("%.2f", round(rmse_ci_upper, 2)),
    R2_CI_lower = sprintf("%.2f", round(r2_ci_lower, 2)),
    R2_CI_upper = sprintf("%.2f", round(r2_ci_upper, 2)),
    MAE_CI_lower = sprintf("%.2f", round(mae_ci_lower, 2)),
    MAE_CI_upper = sprintf("%.2f", round(mae_ci_upper, 2))
  ) %>%
  mutate(RMSE = paste0(RMSE, " [",
                       RMSE_CI_lower, ", ",
                       RMSE_CI_upper, "]"),
         R2 = paste0(R2, " [",
                     R2_CI_lower, ", ",
                     R2_CI_upper, "]"),
         MAE = paste0(MAE, " [",
                      MAE_CI_lower, ", ",
                      MAE_CI_upper, "]")
  ) %>%
  select(feature_set, algorithm, RMSE, R2, MAE) %>%
  group_by(feature_set) %>%
  mutate(feature_set = ifelse(row_number() == 1, as.character(feature_set), "")) %>%
  ungroup()

## APA-style flextable
model_performance_flextable <- 
  model_performance_APH %>%
  flextable() %>%
  set_header_labels(
    feature_set = "Variable Set",
    algorithm = "Algorithm",
    RMSE = "RMSE",
    R2 = "R²",
    MAE = "MAE"
  ) %>%
  # theme_vanilla() %>%
  fontsize(size = 12, part = "all") %>%
  bold(part = "header") %>%
  autofit()



# Export as LaTeX table
latex_model_performance <- kable(model_performance_APH,
                                 format = "latex",
                                 booktabs = TRUE, 
                                 caption = "Comparison of model performance across variable sets and algorithms", 
                                 align = c("l", "l", "c")) %>%
  kable_styling(latex_options = c("hold_position"))




levels(model_performance_df$algorithm)

## purely descriptively, we see that the point estimats consistently indicate
## higher performance of the random forest models over the 
## xgb models

## plotting model performance
pd <- position_dodge(width = 0.9)
#pd <- position_dodge(width = 0.1)

model_descriptions <- c(
  # "Only CBCL",
  "Only symptoms",
  "Only PGS",
  "CBCL + PGS",
  "CBCL + Longitudinal",
  # "CBCL + Long + PGS"
  "PGS + Symptoms + Longitudinal (All modalities)"
)

levels(model_performance_df$feature_set) <- model_descriptions

algs <- c("Ensemble", "Random Forest", "XGBoost")

levels(model_performance_df$algorithm) <- algs

plot_RMSE <- 
  model_performance_df %>%
  ggplot(aes(x = algorithm, y = rmse_est, fill = algorithm)) +
  #ggtitle("Comparative model performance") +
  geom_col() + 
  scale_y_continuous(name = "RMSE") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(#subtitle = "Metric: RMSE, error bars symbolize bootstrapped 95% Confidence Intervals",
    x = "Variable set") + 
  geom_errorbar(aes(ymin = rmse_ci_lower, ymax = rmse_ci_upper),
                position = pd, width = 0.4,
                linewidth = 0.4) +
  coord_cartesian(ylim = c(0.8, NA)) +
  facet_wrap(~feature_set,
             labeller = labeller(feature_set = label_wrap_gen(width = 10)),
             nrow = 1) + 
  theme_classic() + 
  theme(#plot.title = element_text(size=15, face="bold.italic"),
    #plot.subtitle = element_text(size=12, face="italic"),
    axis.text.x=element_blank(),
    axis.title.x = element_text(face="bold", size=24, angle=0, vjust = -1.2),
    axis.text.y = element_text(face="bold", size=22, angle=0),
    axis.title.y = element_text(face="bold", size=24),
    #legend.position = "none",
    panel.spacing = unit(1, "cm"),
    strip.text = element_text(size = 16, face = "bold"),
    # Make legend labels larger
    legend.text = element_text(size = 19),
    legend.title = element_text(size = 20, face = "bold"))

ggsave(filename = "paper_model_comparison_RMSE.png",
       plot = plot_RMSE,
       #width = 9.0,
       width = 12, 
       height = 18,
       device = "png",
       path = here::here("data", "plots", "paper"),
       create.dir = TRUE)


plot_r2 <- 
  model_performance_df %>%
  ggplot(aes(x = algorithm, y = r2_est, fill = algorithm)) +
  #ggtitle("Comparative model performance") +
  geom_col() + 
  scale_y_continuous(name = "R²") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(#subtitle = "Metric: RMSE, error bars symbolize bootstrapped 95% Confidence Intervals",
    x = "Variable set") + 
  geom_errorbar(aes(ymin = r2_ci_lower, ymax = r2_ci_upper),
                position = pd, width = 0.4,
                linewidth = 0.4) +
  #coord_cartesian(ylim = c(0, NA)) +
  facet_wrap(~feature_set,
             labeller = labeller(feature_set = label_wrap_gen(width = 10)),
             nrow = 1) + 
  theme_classic() + 
  theme(#plot.title = element_text(size=15, face="bold.italic"),
    #plot.subtitle = element_text(size=12, face="italic"),
    axis.text.x=element_blank(),
    axis.title.x = element_text(face="bold", size=18, angle=0, vjust = -1.2),
    axis.text.y = element_text(face="bold", size=15, angle=0),
    axis.title.y = element_text(face="bold", size=18),
    #legend.position = "none",
    panel.spacing = unit(1, "cm"),
    strip.text = element_text(size = 16, face = "bold"),
    # Make legend labels larger
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15, face = "bold"))
## still add some margin under the figure and check how you can remove the margin between the edge on the y axis and the begin of the bars

ggsave(filename = "paper_model_comparison_R2.png",
       plot = plot_r2,
       #width = 9.0,
       width = 22, 
       height = 8,
       device = "png",
       path = here::here("data", "plots", "paper"),
       create.dir = TRUE)

plot_r2_poster <- 
  model_performance_df %>%
  filter(feature_set %in% c("Only symptoms",
                            "Only PGS",
                            "PGS + Symptoms + Longitudinal (All modalities)")) %>%
  mutate(feature_set = factor(feature_set,
                              levels=c("Only PGS",
                                       "Only symptoms",
                                       "PGS + Symptoms + Longitudinal (All modalities)"))) %>%
  ggplot(aes(x = algorithm, y = r2_est, fill = algorithm)) +
  ggtitle("Comparative model performance") +
  geom_col(width = 0.4, position = pd) + 
  scale_y_continuous(name = "R²") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(subtitle = "Metric: R², error bars symbolize bootstrapped 95% Confidence Intervals",
       x = "Variable set") + 
  geom_errorbar(aes(ymin = r2_ci_lower, ymax = r2_ci_upper),
                position = pd, width = 0.4,
                linewidth = 0.7) +
  #coord_cartesian(ylim = c(0, NA)) +
  facet_wrap(~feature_set,
             labeller = labeller(feature_set = label_wrap_gen(width = 30)),
             nrow = 3) + 
  theme_classic() + 
  theme(plot.title = element_text(size=25, face="bold.italic"),
        plot.subtitle = element_text(size=23, face="italic"),
        axis.text.x = element_text(face="bold", size=23, angle=0),
        axis.title.x = element_text(face="bold", size=28, angle=0, vjust = -1.2),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        #legend.position = "none",
        panel.spacing = unit(1, "cm"),
        strip.text = element_text(size = 24, face = "bold"),
        # Make legend labels larger
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 23, face = "bold"),
        legend.position = c(0.8, 0.5),
        legend.background = element_rect(fill = "white", color = "black")) + 
  ## still add some margin under the figure and check how you can remove the margin between the edge on the y axis and the begin of the bars
  coord_flip()

ggsave(filename = "paper_model_comparison_R2_poster.png",
       plot = plot_r2_poster,
       #width = 9.0,
       width = 14, 
       height = 18,
       device = "png",
       path = here::here("data", "plots", "paper"),
       create.dir = TRUE)



plot_mae <- 
  model_performance_df %>%
  ggplot(aes(x = algorithm, y = mae_est, fill = algorithm)) +
  #ggtitle("Comparative model performance") +
  geom_col() + 
  scale_y_continuous(name = "MAE") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  labs(#subtitle = "Metric: RMSE, error bars symbolize bootstrapped 95% Confidence Intervals",
    x = "Variable set") + 
  geom_errorbar(aes(ymin = mae_ci_lower, ymax = mae_ci_upper),
                position = pd, width = 0.4,
                linewidth = 0.4) +
  coord_cartesian(ylim = c(0.65, NA)) +
  facet_wrap(~feature_set,
             labeller = labeller(feature_set = label_wrap_gen(width = 10)),
             nrow = 1) + 
  theme_classic() + 
  theme(#plot.title = element_text(size=15, face="bold.italic"),
    #plot.subtitle = element_text(size=12, face="italic"),
    axis.text.x=element_blank(),
    axis.title.x = element_text(face="bold", size=18, angle=0, vjust = -1.2),
    axis.text.y = element_text(face="bold", size=15, angle=0),
    axis.title.y = element_text(face="bold", size=18),
    #legend.position = "none",
    panel.spacing = unit(1, "cm"),
    strip.text = element_text(size = 16, face = "bold"),
    # Make legend labels larger
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15, face = "bold"))
## still add some margin under the figure and check how you can remove the margin between the edge on the y axis and the begin of the bars

ggsave(filename = "paper_model_comparison_mae.png",
       plot = plot_mae,
       #width = 9.0,
       width = 22, 
       height = 8,
       device = "png",
       path = here::here("data", "plots", "paper"),
       create.dir = TRUE)

##----------------------------------------------------------------------------