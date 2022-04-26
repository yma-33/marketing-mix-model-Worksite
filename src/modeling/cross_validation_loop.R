source("src/modeling/startup.R")
source("src/modeling/utils/modeling_utils.R")
source("src/modeling/config_set.R")
source("src/modeling/utils/cross_validation_utils.R")

config <- list()
config <- get_config(config)

# CHOOSE OPTIMAL TIME PARAMETERS AND THEN CHOOSE OTHER OPTIMAL PARAMETERS

time_hyperparameter_table <- data.frame(
  b_max = numeric(),
  d_max = numeric()
)

for (r in 1:length(config$b_max_range)) {
  for(u in 1:length(config$d_max_range)) {
    time_hyperparameter_table[nrow(time_hyperparameter_table) + 1, ] <- c(b_max = config$b_max_range[r],
                                                                          d_max = config$d_max_range[u]
                                                                          )
  }}


print("Be sure to set the output_threshold, input_threshold, variables_method, combination_threshold, and mmo_reset_by in the config file for the time portion of the hyperparamter loop before continuing to run this script...")
nrow_time_table <- nrow(time_hyperparameter_table)

time_hyperparameter_table <- cbind(time_hyperparameter_table, mean_absolute_proportion_error = rep(999, nrow_time_table),
                                   r_squared = rep(999, nrow_time_table),
                                   adj_r_squared = rep(999, nrow_time_table),
                                   error_folds = rep(999, nrow_time_table),
                                   adj_r_squared_folds = rep(999, nrow_time_table))

# This is where hyperparameter tuning for marketing dynamics happens
for(time_loop in 1:nrow_time_table) {
  print(paste0("Time Loop: ", time_loop))
  config$b_max <- c("Life" = time_hyperparameter_table[time_loop, "b_max"], 
                      "Ann" = time_hyperparameter_table[time_loop, "b_max"], 
                      "Di" = time_hyperparameter_table[time_loop, "b_max"],
                      "Invs" = time_hyperparameter_table[time_loop, "b_max"])
  config$d_max <- c("Life" = time_hyperparameter_table[time_loop, "d_max"], 
                    "Ann" = time_hyperparameter_table[time_loop, "d_max"], 
                    "Di" = time_hyperparameter_table[time_loop, "d_max"],
                    "Invs" = time_hyperparameter_table[time_loop, "d_max"])
  source("src/modeling/data_preprocessing.R")
  
  config$do_cross_validation <-TRUE
  config$crossVal_holdout_method <- "kfold"
  model_metrics <- cross_validation_inner_loop(config, DMat0, FMat0, outputValues, outputNames)
  time_hyperparameter_table[time_loop,]$mean_absolute_proportion_error <- model_metrics[1]
  time_hyperparameter_table[time_loop,]$r_squared <- model_metrics[2]
  time_hyperparameter_table[time_loop,]$adj_r_squared <- model_metrics[3]
  time_hyperparameter_table[time_loop,]$error_folds <- model_metrics[4]
  time_hyperparameter_table[time_loop,]$adj_r_squared_folds <- model_metrics[6]
  
  config$do_cross_validation <-FALSE
  config$crossVal_holdout_method <- "random"
  model_metrics <- cross_validation_inner_loop(config, DMat0, FMat0, outputValues, outputNames)
  combinationCoeffs <- model_metrics$combinationCoeffs[[1]]
  parameterCoeffs <- model_metrics$parameterCoeffs[[1]]
  model_attr <- model_attribution(input,output_all,combinationCoeffs)
  curr_attr <- calculate_base(model_attr,output)$Life
  time_hyperparameter_table[time_loop,]$TV  <- curr_attr %>% filter(channel=="Amazon") %>% select(attribution) %>% pull()
  time_hyperparameter_table[time_loop,]$Facebook  <- curr_attr %>% filter(channel=="Facebook") %>% select(attribution) %>% pull()
  time_hyperparameter_table[time_loop,]$SEM  <- curr_attr %>% filter(channel=="SEM") %>% select(attribution) %>% pull()
  time_hyperparameter_table[time_loop,]$base  <- curr_attr %>% filter(channel=="base") %>% select(attribution) %>% pull()
}

time_hyperparameter_table <- cbind(time_hyperparameter_table, 
                                   output_threshold = rep(config$output_threshold, nrow_time_table),
                                   input_threshold = rep(config$input_threshold, nrow_time_table),
                                   variables_method = rep(config$variables_method, nrow_time_table),
                                   combination_threshold = rep(config$combination_threshold, nrow_time_table),
                                   mmo_reset_by = rep(config$mmo_reset_by, nrow_time_table))

time_hyperparameter_table$mean_absolute_proportion_error <- as.numeric(time_hyperparameter_table$mean_absolute_proportion_error)
time_hyperparameter_table$r_squared <- as.numeric(time_hyperparameter_table$r_squared)
time_hyperparameter_table$adj_r_squared <- as.numeric(time_hyperparameter_table$adj_r_squared)
write_rds(time_hyperparameter_table, "src/data/20220211_time_hyperparam_loop_results_variables_method_11_80.rda")

time_hyperparameter_table <- readRDS(file = "src/data/20220211_time_hyperparam_loop_results_all_method_11.rda")

times <- rbind(
  time_hyperparameter_table %>% filter(mean_absolute_proportion_error == min(mean_absolute_proportion_error)),
  time_hyperparameter_table %>% filter(adj_r_squared == max(adj_r_squared))
  #time_hyperparameter_table %>% filter(b_max == 8 & d_max == 13)
) 

# FIND OPTIMAL HYPERPARAMETERS AFTER TIME ADJUSTMENT HAS BEEN SELECTED
hyperparameter_table_temp <- data.frame(
  output_threshold = numeric(),
  input_threshold = numeric(),
  combination_threshold = numeric()
)


for (w in 1:length(config$output_threshold_range)) {
  for (e in 1:length(config$input_threshold_range)) {
    for (i in 1:length((config$combination_threshold_range)))
      hyperparameter_table_temp[nrow(hyperparameter_table_temp) + 1, ] <- c(
          output_threshold = config$output_threshold_range[w],
          input_threshold = config$input_threshold_range[e],
          combination_threshold = config$combination_threshold_range[i]
        )
    }
  }


temp_size_tab <- nrow(hyperparameter_table_temp)

for (t in 1:nrow(times)) {
  if (t == 1) {
    hyperparameter_table <- cbind(hyperparameter_table_temp,
                                  b_max = rep(times[t, "b_max"], temp_size_tab),
                                  d_max = rep(times[t, "d_max"], temp_size_tab)
    )
  } else {
    temp <- cbind(hyperparameter_table_temp,
                  b_max = rep(times[t, "b_max"], temp_size_tab),
                  d_max = rep(times[t, "d_max"], temp_size_tab)
    )
    hyperparameter_table <- rbind(hyperparameter_table, temp)
  }
}

hyperparameter_table <- add_factor_hyper_parameter(hyperparameter_table, config$mmo_reset_by_range, "mmo_reset_by")

nrow_hyperparameter_table <- nrow(hyperparameter_table)

hyperparameter_table <- cbind(hyperparameter_table,
                              mean_absolute_proportion_error = rep(999, nrow_hyperparameter_table),
                              r_squared = rep(999, nrow_hyperparameter_table),
                              adj_r_squared = rep(999, nrow_hyperparameter_table),
                              error_folds = rep(999, nrow_hyperparameter_table),
                              adj_r_squared_folds = rep(999, nrow_hyperparameter_table)
)

# Sort hyperparameter_table to make hyperparameter loop faster 
hyperparameter_table <- hyperparameter_table %>% dplyr::arrange(b_max, d_max, output_threshold,input_threshold)
rownames(hyperparameter_table) <- 1:nrow(hyperparameter_table)

# Initialize df to output the evaluation metrics for each fold
fold_output <- data.frame()

# This is where the 2nd hyperparameter tuning is run
for (hyperparam_loop in 1:nrow_hyperparameter_table) {
  print(paste0("Hyperparameter loop: ", hyperparam_loop))
  if (hyperparam_loop == 1) {
    config$output_threshold <- hyperparameter_table[hyperparam_loop, "output_threshold"]
    config$input_threshold <- hyperparameter_table[hyperparam_loop, "input_threshold"]
    config$combination_threshold <- hyperparameter_table[hyperparam_loop, "combination_threshold"]
    config$mmo_reset_by <- hyperparameter_table[hyperparam_loop, "mmo_reset_by"]
    
    config$b_max <- c("Life" = hyperparameter_table[hyperparam_loop, "b_max"], 
                        "Ann" = hyperparameter_table[hyperparam_loop, "b_max"], 
                        "Di" = hyperparameter_table[hyperparam_loop, "b_max"],
                        "Invs" = hyperparameter_table[hyperparam_loop, "b_max"])
    config$d_max <- c("Life" = hyperparameter_table[hyperparam_loop, "d_max"], 
                      "Ann" = hyperparameter_table[hyperparam_loop, "d_max"], 
                      "Di" = hyperparameter_table[hyperparam_loop, "d_max"],
                      "Invs" = hyperparameter_table[hyperparam_loop, "d_max"])
    source("src/modeling/data_preprocessing.R")
    
    
    if (config$crossVal_holdout_method == "kfold") {
      Weights_all <- kfold_assign_weights(outputNames, config, n_folds = config$n_folds)
    } else {
      Weights_all <- assign_weights(outputNames, config)
    }
  } else {
    last_b_max <- config$b_max
    last_d_max <- config$d_max
    last_input_threshold <- config$input_threshold
    last_output_threshold<- config$output_threshold
    last_mmo_reset_by <- config$mmo_reset_by
    
    config$output_threshold <- hyperparameter_table[hyperparam_loop, "output_threshold"]
    config$input_threshold <- hyperparameter_table[hyperparam_loop, "input_threshold"]
    config$combination_threshold <- hyperparameter_table[hyperparam_loop, "combination_threshold"]
    config$mmo_reset_by <- hyperparameter_table[hyperparam_loop, "mmo_reset_by"]
    
    config$b_max <- c("Life" = hyperparameter_table[hyperparam_loop, "b_max"], 
                        "Ann" = hyperparameter_table[hyperparam_loop, "b_max"], 
                        "Di" = hyperparameter_table[hyperparam_loop, "b_max"],
                        "Invs" = hyperparameter_table[hyperparam_loop, "b_max"])
    config$d_max <- c("Life" = hyperparameter_table[hyperparam_loop, "d_max"], 
                      "Ann" = hyperparameter_table[hyperparam_loop, "d_max"], 
                      "Di" = hyperparameter_table[hyperparam_loop, "d_max"],
                      "Invs" = hyperparameter_table[hyperparam_loop, "d_max"])
  
    if (config$b_max != last_b_max || config$d_max != last_d_max||
        config$input_threshold != last_input_threshold ||
        config$output_threshold != last_output_threshold ||
        config$mmo_reset_by != last_mmo_reset_by) {
      print("Re-sourcing data_preprocessing due to data change...")
      source("src/modeling/data_preprocessing.R")
    }
  }
  tryCatch({
    model_metrics <- cross_validation_inner_loop(config, DMat0, FMat0, outputValues, outputNames)
    hyperparameter_table[hyperparam_loop, ]$mean_absolute_proportion_error <- model_metrics$mean_absolute_proportion_error
    hyperparameter_table[hyperparam_loop, ]$r_squared <- model_metrics$r_squared
    hyperparameter_table[hyperparam_loop, ]$adj_r_squared <- model_metrics$adj_r_squared
    hyperparameter_table[hyperparam_loop, ]$error_folds <- model_metrics[4]
    hyperparameter_table[hyperparam_loop, ]$adj_r_squared_folds <- model_metrics[6]
   
  }, error = function(e) {
    print(paste0("Issue in index", hyperparam_loop, " skipping for now..."))
  })
}

write_rds(hyperparameter_table, "src/data/2_7_2022_hyperparameter_loop_variables_method_11_36.Rda")
write.csv(cross_validation_fold_output, "src/data/fold_output_results.csv")
