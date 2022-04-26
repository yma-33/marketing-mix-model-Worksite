source("src/modeling/startup.R")
source("src/modeling/utils/modeling_utils.R")
source("src/modeling/config_set.R")
source("src/modeling/utils/cross_validation_utils.R")

config <- list()
config <- final_config(config)

source("src/modeling/data_preprocessing.R")

model_metrics <- cross_validation_inner_loop(config, DMat0, FMat0, outputValues, outputNames)
combinationCoeffs <- model_metrics$combinationCoeffs[[1]]
parameterCoeffs <- model_metrics$parameterCoeffs[[1]]
print(model_metrics$mean_absolute_proportion_error)
print(model_metrics$adj_r_squared)

model_attr <- model_attribution(input,output_all,combinationCoeffs)
attr_base <- calculate_base(model_attr,output_all)
attr_base

model_data <- list(input=INPUT, output=output, combinationCoeffs=combinationCoeffs, model_attr=model_attr)
model_object <- list(parameterCoeffs=parameterCoeffs, combinationCoeffs=combinationCoeffs, model_attr=model_attr, attr_base=attr_base)
saveRDS(model_object, file = 'model_object_2022Q1Rec.rds')

# ~~~~~ R INPUTS FOR OPTIMIZATION CODE ~~~~~~
combinationCoeffs <- combinationCoeffs %>% 
  mutate(channel_campaign=paste0(extract_dimension(input, "channel"),"_", extract_dimension(input, "campaign")))

# Write csvs for optimization
dir_file_path <- "src/data/2022q1_model_refresh"
dir.create(dir_file_path)
write.csv(cbind(outputNames,DMat0,FMat0),paste0(dir_file_path,"/dat.csv") ,row.names=FALSE)
write.csv(combinationCoeffs, paste0(dir_file_path,"/combinationCoeffs.csv"), row.names = FALSE)
write.csv(cbind(param_name = names(parameterCoeffs), parameterCoeffs), paste0(dir_file_path,"/parameterCoeffs.csv"), row.names = FALSE)
write.csv(data.frame(outputNames, FMat0), paste0(dir_file_path,"/FMat0.csv"), row.names = FALSE)

# Get average base per quarter
base_numbers <- c()
for (output_dimension in config$output_dimentions$lob){
  base_numbers <- c(base_numbers,
                    attr_base[[output_dimension]] %>% filter(channel=="base") %>% 
                      select(attribution) %>% pull() / 100)
}
base_table <- data.frame(LOB=config$output_dimentions$lob, base=base_numbers)
avg_base_per_quarter <- conversion_table %>%
       mutate(yr_quarter = as.yearqtr(as.Date(Week, "%Y-%m-%d"))) %>%
       filter(LOB != "Ltc", year(yr_quarter) == 2020) %>%
       inner_join(base_table) %>%
       group_by(yr_quarter) %>%
       summarise(Total = sum(conversion * base, na.rm = TRUE)) %>%
       mutate(curr_quarter= lubridate::quarter(yr_quarter)) %>% 
       group_by(curr_quarter) %>%
       summarise(avg_base_per_quarter = mean(Total))
print("Add the average base for the current quarter to the optimization YAML")
print(avg_base_per_quarter)