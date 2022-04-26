source("src/modeling/startup.R")
source("src/modeling/utils/modeling_utils.R")
source("src/modeling/config_set.R")
source("src/modeling/utils/cross_validation_utils.R")
source("src/modeling/utils/model_validation_utils.R")

config <- list()
config <- final_config(config)

source("src/modeling/data_preprocessing.R")

possible_features_tab <- data.table("features" = c("(Intercept)",colnames(DMat0), colnames(FMat0)))
adj_r_squared_tab <- data.frame()
mape_tab <- data.frame()
coeff_tab <- data.frame()
model_metrics_list <- list()

for (i in 1:10){
  
  config$weights_seed <- sample(1:10000, 1)
  model_metrics <- cross_validation_inner_loop(config, DMat0, FMat0, outputValues, outputNames)
  
  adj_r_squared_tab <- rbind(adj_r_squared_tab, data.frame(
    cross_val_loop = rep(i, config$n_folds),
    folds = seq(1:config$n_folds),
    r_squared = model_metrics$adj_r_square_folds))
  
  mape_tab <- rbind(mape_tab, data.frame(
    cross_val_loop = rep(i, config$n_folds),
    folds = seq(1:config$n_folds),
    mape = model_metrics$error_folds))
  
  
  model_metrics_list[[i]] <- model_metrics
}

# plot error 

adj_r_squared_tab %>% ggplot(aes(x = r_squared)) + geom_dotplot(binwidth = 0.01) +
  ggtitle("Adjusted R-squared Across Folds")
mape_tab %>% ggplot(aes(x = mape)) + geom_dotplot(binwidth = 0.01) +
  ggtitle("MAPE Across Folds")

# get parameter_coeffs, combination_coeffs, attribution, and attribution base across all folds

parameter_coeffs_list <- list()
combination_coeffs_list <- list()
attribution_list <- list()
attribution_base_list <- list()
adj_r_squared_positive_vect <- c()

for(i in 1:10){
  for(j in 1:10){
    parameter_coeffs_list[[10*(i-1)+j]] <- model_metrics_list[[i]]$parameterCoeffs[[j]]
    combination_coeffs_list[[10*(i-1)+j]] <- model_metrics_list[[i]]$combinationCoeffs[[j]]
    attribution_list[[10*(i-1)+j]] <- model_attribution(input,output,model_metrics_list[[i]]$combinationCoeffs[[j]])
    attribution_base_list[[10*(i-1)+j]] <- calculate_base(attribution_list[[10*(i-1)+j]],output)
    
    adj_r_squared_positive_vect[10*(i-1)+j] <- model_metrics_list[[i]]$adj_r_square_folds[j] > 0
  }
}

# check for consistency in parameter_coeff values and feature selection across folds

for(k in 1:length(adj_r_squared_positive_vect)){
  if(adj_r_squared_positive_vect[k]){
    
    temp <- data.table(names(parameter_coeffs_list[[k]]), parameter_coeffs_list[[k]])
    colnames(temp) <- c("features", paste("fold", k, sep = ""))
    
    possible_features_tab <- possible_features_tab %>% left_join(temp, by = "features")
  }
}

possible_features_tab$feature_count <- rowSums(!is.na(possible_features_tab[,2:101]))

# This plot is useless... still thinking about how to visualize the occurence of features across folds
possible_features_tab %>% filter(feature_count > 0) %>% 
  ggplot(aes(x = features, y = feature_count, fill = feature_count)) + 
  geom_col() +
  coord_flip()

# check for consistency in combination coeff values across folds (this will be the same plot as the parameter_coeff plot)

# check attribution across folds
life_attribution_tab <- attribution_base_list[[1]][[1]]
colnames(life_attribution_tab) <- c("channel", paste("fold", 1, sep = ""))
ann_attribution_tab <- attribution_base_list[[1]][[2]]
colnames(ann_attribution_tab) <- c("channel", paste("fold", 1, sep = ""))
dis_attribution_tab <- attribution_base_list[[1]][[3]]
colnames(dis_attribution_tab) <- c("channel", paste("fold", 1, sep = ""))

for (i in 2:100){
  life_temp <- attribution_base_list[[i]][[1]]
  colnames(life_temp) <- c("channel", paste("fold", i, sep = ""))
  life_attribution_tab <- left_join(life_attribution_tab, life_temp, by = "channel")
  
  ann_temp <- attribution_base_list[[i]][[2]]
  colnames(ann_temp) <- c("channel", paste("fold", i, sep = ""))
  ann_attribution_tab <- left_join(ann_attribution_tab, ann_temp, by = "channel")
  
  dis_temp <- attribution_base_list[[i]][[3]]
  colnames(dis_temp) <- c("channel", paste("fold", i, sep = ""))
  dis_attribution_tab <- left_join(dis_attribution_tab, dis_temp, by = "channel")
}

life_plots <- make_attribution_plots(life_attribution_tab, "Life")
life_plots

life_summary <- make_attribution_summary_table(life_attribution_tab)
life_summary

ann_plots <- make_attribution_plots(ann_attribution_tab, "Annuity")
ann_plots

ann_summary <- make_attribution_summary_table(ann_attribution_tab)
ann_summary

dis_plots <- make_attribution_plots(dis_attribution_tab, "Disability")
dis_plots

dis_summary <- make_attribution_summary_table(dis_attribution_tab)
dis_summary

# Looking at full model

# residuals
calc_model_predictions <- as.data.frame(cbind(DMat0,FMat0)) %>%
  mutate("(Intercept)"=1) %>% 
  select(names(parameterCoeffs)) %>%
  as.matrix() %*% parameterCoeffs

calc_model_predictions_df <- as.data.frame(list(outputNames, pred_values = exp(calc_model_predictions))) %>%
  mutate(
    out_comb = paste0(lob, "_", state, "_", segment),
    Week = as.Date(Week)
  ) %>%
  group_by(Week) %>%
  summarise(pred_tot_conversions = sum(pred_values))

true_conversions <- output %>%
  mutate(true_values = rowSums(across(where(is.numeric)))) %>%
  select(Week, true_values) %>% mutate(Week = as.Date(Week))

conversions_by_week <- true_conversions %>% left_join(calc_model_predictions_df, by = "Week") %>%
  mutate(residuals = true_values - pred_tot_conversions)

conversions_by_week %>%
  ggplot(aes(x = Week, y = residuals)) + 
  geom_line() + 
  ylab("true - predicted") + 
  ggtitle("residuals")

# temporal conversion pattern

conversions_by_week %>% mutate(actual = true_values, 
                               predicted = pred_tot_conversions) %>%
  select(-c(true_values, pred_tot_conversions)) %>%
           gather("conversions", "count", 3:4) %>%
  ggplot(aes(x = Week, y=count)) +
  geom_line(aes(color = conversions)) +
  ggtitle("Predicted and Actual Conversions By Week") 

conversions_by_week %>%
  ggplot(aes(x = Week, y = true_values)) + 
  geom_line() + 
  ggtitle("conversions by week")

conversions_by_week %>%
  ggplot(aes(x = Week, y = pred_tot_conversions)) + 
  geom_line() + 
  ggtitle("predicted conversions by week")

# residuals vs fitted

conversions_by_week %>%
  ggplot(aes(x = pred_tot_conversions, y = residuals)) +
  geom_point() +
  ggtitle("residuals vs fitted")







