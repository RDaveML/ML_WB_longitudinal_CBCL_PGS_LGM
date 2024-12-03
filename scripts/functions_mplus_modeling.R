## function to loop over all questions of CBCL

pacman::pload(...)

prepare_Mplus <- function(df, question, items_list){

  ## extracting item names
  items <- items_list %>% filter(question == !!question) %>%
    select(item_name) %>%
    pull()
  
  CBCL_question <- question
  
  nt <- length(items)
  ## number of timepoints that go into the longitudinal
  ## model
  
  ## creating df for longitudinal modeling
  df_question <- df %>% select(FISNumber, FamilyNr, twzyg, all_of(items)) %>%
    rename("FISNr" = FISNumber,
           "FamNr" = FamilyNumber)
  ## renaming variables for Mplus (max 8 char.)
  
  ## renaming CBCL item columns into simply t1 - tmax 
  colnames(df_question)[grep("^CBCL_", colnames(df_question))] <- 
    paste0("t", 1:nt)
  
  ## returning a list with needed information for the modeling part
  return(list_question = list(df_question,
                              CBCL_question,
                              nt))
}
## In principle, this should create a working dataset for Mplus modeling

df_question <- test_df


## next: model objects Mplus, 1-5 classes
mplus_loop_objects <- function(list_question, n_class = c(1:5), verbose = TRUE){
  
  CBCL_question <- list_question$CBCL_question
  
  df_question <- list_question$df_question
  
  ## adding print statement for server output
  if(verbose) {
    cat("modeling 1-5 class GMM models for CBCL item: ", CBCL_question)
  }
  
  
  items <- grep("^t[0-9]", colnames(df_question), value = TRUE)
  nt <- length(items)
  models <- vector("list", length = length(n_class))
  names(models) <- paste0(n_class, "-class model")
  for(nc in n_class){
    if(nc == 1){
      models[[nc]]$mod_object <- mplusObject(
        TITLE = glue("1-class model"),
        VARIABLE = 
          glue(
            "usevar = {paste(items, collapse = ' ')} FamNr twzyg;",
            "categorical = {paste(items, collapse = ' ')};",
            "cluster = FamNr;"
          ),
        ANALYSIS = 
          "type = complex; ! corrects standard errors for clustering
           estimator = mlr;",
        MODEL = 
          glue(
            "i s | t1@0 {paste0(items[2:(nt-1)], '*', collapse = ' ')} {items[nt]}@1;"
          ),
        ## still add further specifications of model! 
        OUTPUT = "sampstat standardized tech1 tech4 tech8;",
        usevariables = colnames(df_question),
        rdata = df_question
        )
    } else { # 2-5 classes
      models[[nc]]$mod_object <- mplusObject(
        TITLE = glue(nc, "-class model"),
        VARIABLE = 
          glue(
            "usevar = {paste(items, collapse = ' ')} FamNr twzyg;",
            "categorical = {paste(items, collapse = ' ')};",
            "classes = c(", nc, ");",
            "cluster = FamNr;"
          ),
        ANALYSIS = 
          "type = mixture complex; ! corrects standard errors for clustering
           estimator = mlr;",
        MODEL = 
          glue(
            "%overall%
            b0 by {paste0(items, '@1', collapse = ' ')};
            b1 by t1@0 {paste0(items[2:(nt-1)], '*', collapse = ' ')} {items[nt]}@1;
            [t1@0 t2@0 t3@0 t4@0 t5@0];"
          ),
        ## still add further specifications of model! 
        OUTPUT = "sampstat standardized tech1 tech4 tech8;",
        usevariables = colnames(df_question),
        rdata = df_question
      )
    }
  } ## end for loop

  
  return(list(models, CBCL_question))
} ## eoF


## Running models, deleting data output to not drown in datasets 
## created by mplus_loop_objects

mplus_loop_run <- function(models,
                           mplusversion = "C:/Program Files/Mplus/Mplus.exe",
                           CBCL_question) {
  
  for(model_nr in 1:length(models)) {
  ## running the models
  models[[model_nr]]$run <- mplusModeler(models[[model_nr]]$mod_object,
                              dataout = paste0(CBCL_question,
                                               "_model_data.dat"),
                              modelout = paste0(CBCL_question,
                                                "_",
                                                model_nr,
                                                "-class_mod.inp"),
                              check = TRUE, run = TRUE, hashfilename = FALSE,
                              Mplus_command = mplusversion)
  }
  
  ## delete additional data
  
  ## change directory still
  unlink(list.files(here("mplus_files"), pattern = "\\model_data.dat$",
                    full.names = TRUE))
  
  return(models) ## list object?

} ## eoF


## extracting parameters and compare models: Choose best according to 
## fit criteria and entropy, etc. 

CBCL_question <- "m_mix0_loop"
folder <- here::here("mplus_files")

choose_model <- function(CBCL_question, folder){
  
  search_string <- CBCL_question
  
  cat(search_string)
  
  models_list <- as.list(readModels(target = folder, 
                                    recursive = TRUE,
                                    paste0(".*", search_string, ".*"),
                                    quiet = FALSE)) ## for printing output
  
  
  n_models <- length(models_list)
  
  cat("number of models compared: ", n_models, "\n")
  
  cat("models compared for string: ", "'", CBCL_question, "'", "\n",
      paste(names(models_list[[1]]), sep = "; "))
  ## note that basename is still included here, thus not access the model names
  ## only the numeric index
  ## later also save the number of latent classes and whether it was a 
  ## GMM, LCGA or ML model
  
  models_drop <- c()
  for(m in 1:length(models_list)){
    print(names(models_list)[m])
    print(min(models_list[[m]]$class_counts$posteriorProb$proportion))
    print(models_list[[m]]$class_counts$posteriorProb$proportion)
    if(min(models_list[[m]]$class_counts$posteriorProb$proportion) < 0.05){
      ## indexing models with too small minimal group size
      models_drop <- c(models_drop, m)
    }
  }
  
  print(models_drop)
  ## removing indexed models from models lost
  models_list <- models_list[-models_drop]
  
  ## update length 
  n_models <- length(models_list)
  
  
  cat("number of models compared: ", n_models)
  
  cat("models compared for string: ", "'", CBCL_question, "'", "\n",
      names(models_list))
  
  if(length(models_list) == 1){
    
    model_chosen <- models_list[[1]]
    name_model <- names(models_list)[1]
  
  } else {
  ## Extracting parameters:
  
  ## BIC
  mod_BICs <- c()
  for(m in 1:length(models_list)){
    mod_BICs[m] <- models_list[[m]]$summaries$BIC
  }
  
  ind_BIC <- which.min(mod_BICs)
  
  ## BIC difference: If difference between lowest and 2nd lowest 
  ## BIC < 10, use entropy for model choice
    if(mod_BICs[ind_BIC] - sort(mod_BICs, decreasing = TRUE)[2] < 10){
      ## entropy
      mod_en <- c()
      for(m in 1:length(models_list)){
        mod_en[m] <- models_list[[m]]$summaries$Entropy
      }
    
      ind_entropy <- which.max(mod_en)
    
      ## choose model based on maximum entropy 
      model_chosen <- models_list[[ind_entropy]]
      name_model <- names(models_list)[[ind_entropy]]
    
    } else {
    
    ## choose model based on minimum BIC
      model_chosen <- models_list[[ind_BIC]]
      name_model <- names(models_list)[[ind_BIC]]
    }
  
  }
  cat("chosen model based on BIC (and entropy): ", names(models_list))
  
  ## potentially add more here! classes (for saving how often how many class 
  ## models emerged)
  
  ## saving number of latent classes
  n_class_mod <- as.numeric(gsub(".*\\((\\d+)\\).*", "\\1",
                                 models_list[[1]]$input$variable$classes))
  
  ## still add: is it GMM, LCGM or ML model?
  ## CONTINUE HERE
  return(list(model_chosen = model_chosen,
              name_model = name_model,
              n_class = n_class_mod))
  
}


test_choose_model <- choose_model(CBCL_question = "m_mix0_loop", 
                                  folder = here::here("mplus_files"))

## model object:
test_choose_model[[1]]

## model name: 
test_choose_model[[2]]

## number of latent classes winning model
test_choose_model[[3]]




## Testing the functions with 3 test questions, 3 models, 3 outputs

test_run <- FALSE

min_group_size <- 0.05

if(test_run){
  
  test_mod1 <- readModels(here::here("mplus_files", "c2_m_mix0_loop.out"), quiet = FALSE)
  
  ## checking minimal size of latent groups! By this we can 
  ## dismiss models too small group sizes 
  min(test_mod1$class_counts$posteriorProb$proportion)
  
  models_drop <- vector(length = length(models))
  for(m in models){
    if(min(models[[m]]$class_counts$posteriorProb$proportion) < 0.05){
    ## indexing models with too small minimal group size
    models_drop <- c(models_drop, m)
    }
  }
  
  ## removing indexed models from models lost
  models <- models[-models_drop]
  
  
  ## Extracting parameters:
  
  ## BIC
  mod_BICs <- vector(length = length(models))
  for(m in models){
    mod_BICs[m] <- models[[m]]$summaries$BIC
  }
  test_mod1$summaries$BIC
  
  ind_BIC <- which.min(mod_BICs)
  
  ## BIC difference: If difference between lowest and 2nd lowest 
  ## BIC < 10, use entropy for model choice
  if(mod_BICs[ind_BIC] - sort(mod_BICs, decreasing = TRUE)[2] < 10){
    ## entropy
    mod_en <- vector(length = length(models))
    for(m in models){
      mod_en[m] <- models[[m]]$summaries$Entropy
    }
    
    ind_entropy <- which.max(mod_en)
    
    ## choose model based on maximum entropy 
    model_chosen <- models[[ind_entropy]]
    
  } else {
    
    ## choose model based on minimum BIC
    model_chosen <- models[[ind_BIC]]  
  }
  
  

}






