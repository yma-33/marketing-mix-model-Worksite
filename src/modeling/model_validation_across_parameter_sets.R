source("src/modeling/startup.R")
source("src/modeling/utils/modeling_utils.R")
source("src/modeling/config_set.R")
source("src/modeling/utils/cross_validation_utils.R")
source("src/modeling/utils/model_validation_utils.R")

print("Change the path to load the final hyperparameter table before continuing to run this script...")
hyper_table_all <- read_rds("src/data/5_12_loop_data/entire_hyper_table.rds")
hyper_table <- hyper_table_all %>% filter(r_squared != 999)

best_folds <- hyper_table %>% slice_max(order_by = adj_r_squared, n = 5)
best_folds <- rbind(best_folds, hyper_table %>% slice_min(order_by = mean_absolute_proportion_error, n = 5))

nrow_best_folds <- nrow(best_folds)
nrow_best_folds

config <- list()
config <- final_config(config)

model_metrics_list <- list()
parameter_coeffs_list <- list()
combination_coeffs_list <- list()
attribution_list <- list()
attribution_base_list <- list()

for(i in 1:nrow_best_folds){
  print(paste("starting loop:", i))
  config$output_threshold <- best_folds[i, "output_threshold"]
  config$input_threshold <- best_folds[i, "input_threshold"]
  config$combination_threshold <- best_folds[i, "combination_threshold"]
  b_max <- best_folds[i, "b_max"]
  config$b_max <- c("Life" = b_max, "Ann" = b_max, "Di" = b_max, "Invs" = b_max)
  d_max <- best_folds[i, "d_max"]
  config$d_max <- c("Life" = d_max, "Ann" = d_max, "Di" = d_max, "Invs" = d_max)
  config$mmo_reset_by <- best_folds[i, "mmo_reset_by"]
  config$variables_method <- best_folds[i, "variables_method"]
  
  source("src/modeling/data_preprocessing.R")
  
  model_metrics_list[[i]] <- cross_validation_inner_loop(config, DMat0, FMat0, outputValues, outputNames)
  
  parameter_coeffs_list[[i]] <- model_metrics_list[[i]]$parameterCoeffs
  combination_coeffs_list[[i]] <- model_metrics_list[[i]]$combinationCoeffs[[1]]
  attribution_list[[i]] <- model_attribution(input,output_all,combination_coeffs_list[[i]])
  attribution_base_list[[i]] <- calculate_base(attribution_list[[i]],output)
}

# check attribution across folds

life_attribution_tab <- attribution_base_list[[1]][[1]]
colnames(life_attribution_tab) <- c("channel", paste("model", 1, sep = ""))
invs_attribution_tab <- attribution_base_list[[1]][[2]]
colnames(invs_attribution_tab) <- c("channel", paste("model", 1, sep = ""))
ann_attribution_tab <- attribution_base_list[[1]][[3]]
colnames(ann_attribution_tab) <- c("channel", paste("model", 1, sep = ""))
dis_attribution_tab <- attribution_base_list[[1]][[4]]
colnames(dis_attribution_tab) <- c("channel", paste("model", 1, sep = ""))

for (i in 2:nrow_best_folds){
  life_temp <- attribution_base_list[[i]][[1]]
  colnames(life_temp) <- c("channel", paste("model", i, sep = ""))
  life_attribution_tab <- left_join(life_attribution_tab, life_temp, by = "channel")
  
  invs_temp <- attribution_base_list[[i]][[2]]
  colnames(invs_temp) <- c("channel", paste("model", i, sep = ""))
  invs_attribution_tab <- left_join(invs_attribution_tab, invs_temp, by = "channel")
  
  ann_temp <- attribution_base_list[[i]][[3]]
  colnames(ann_temp) <- c("channel", paste("model", i, sep = ""))
  ann_attribution_tab <- left_join(ann_attribution_tab, ann_temp, by = "channel")
  
  dis_temp <- attribution_base_list[[i]][[4]]
  colnames(dis_temp) <- c("channel", paste("model", i, sep = ""))
  dis_attribution_tab <- left_join(dis_attribution_tab, dis_temp, by = "channel")
}

life_plots <- make_attribution_plots(life_attribution_tab, "Life")
life_plots

life_summary <- make_attribution_summary_table(life_attribution_tab)
life_summary

invs_plots <- make_attribution_plots(invs_attribution_tab, "Investments")
invs_plots

invs_summary <- make_attribution_summary_table(invs_attribution_tab)
invs_summary

ann_plots <- make_attribution_plots(ann_attribution_tab, "Annuity")
ann_plots

ann_summary <- make_attribution_summary_table(ann_attribution_tab)
ann_summary

dis_plots <- make_attribution_plots(dis_attribution_tab, "Disability")
dis_plots

dis_summary <- make_attribution_summary_table(dis_attribution_tab)
dis_summary

# Select the final model

life_median <- life_summary[which(life_summary$channel == "base"),]$median
which(life_attribution_tab == life_median, arr.ind=TRUE)

ann_median <- ann_summary[which(ann_summary$channel == "base"),]$median
which(ann_attribution_tab == ann_median, arr.ind=TRUE)

dis_median <- dis_summary[which(dis_summary$channel == "base"),]$median
which(dis_attribution_tab == dis_median, arr.ind=TRUE)

potential_models <- best_folds[c(1,19),]

# Final Model Selected
config$output_threshold <- 0.99
config$input_threshold <- 0.99
config$combination_threshold <- 0.2
b_max <- 5
config$b_max <- c("Life" = b_max, "Ann" = b_max, "Di" = b_max)
d_max <- 7
config$d_max <- c("Life" = d_max, "Ann" = d_max, "Di" = d_max)
config$mmo_reset_by <- "state"
config$variables_method <- 10
config$seasonality_method <- "seasonality"

