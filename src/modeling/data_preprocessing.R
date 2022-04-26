source("src/modeling/utils/preprocessing_utils.R")
source("src/modeling/utils/bandwidth_delay_utils.R")
source("src/modeling/utils/adstock_utils.R")

vertica_setup(server = "aws_qa",
              user = Sys.getenv("mmm_user"),
              password = Sys.getenv("mmm_pass"))

# Pull in data from Vertica
sql_fp <- "src/modeling/sql/" # sql filepath

impression_table <- get_query(sql = read_sql(paste0(sql_fp,"multi_level_impressions.sql")))
conversion_table <- get_query(sql = read_sql(paste0(sql_fp,"multi_level_conversions_tera.sql")))
factor_table <- get_query(sql = read_sql(paste0(sql_fp,"external_factors.sql")))

# state to region mapping
state_region_dat <- get_query(sql = read_sql(paste0(sql_fp,"state_region.sql")))

vertica_disconnect()

# Aggregate output by rules set in the config file
output_config <- data_preprocessing(conversion_table, config, type="output")
output <- output_config$data_table
config <- output_config$config
output_all <- output_config$data_table_all

# Pivot dataframes
input_config <- data_preprocessing(impression_table, config, type="input")
INPUT <- input_config$data_table
config <- input_config$config
  
variables <- make_variables_list(setdiff(names(INPUT),"Week"), setdiff(names(output),"Week"))

if(config$choose_time_window_method == "adstock"){
  # --- ADSTOCK ----
  input <- list()
  # --- Optimize adstock rate for each channel by LOB
  for(lob in config$output_dimentions$lob){
    if(config$optimize_adstock_rate){
        for(use_ext_factors in c(TRUE, FALSE)){
          # Find optimial adstock rates
          config$optimize_adstock_with_ext_factors <- use_ext_factors
          print(sprintf("OPTIMIZING ADSTOCK, LOB: %s, multiplicative: %s, ext_factors: %s", 
                        lob, config$adstock_multiplicative, use_ext_factors))
          adstock_rate_df <- df_optimal_adstock_rate(INPUT, output, lob)
          best_rates <- optimize_adstock_rate(adstock_rate_df, config, factor_table)
          # Append results to config file
          write_to_config(best_rates, lob, config)
        }
      } else {
      # Use pre-optimized adstock rates
      get_optimal_from_config <- paste0(lob, "_adstock_ext_factors_", tolower(config$optimize_adstock_with_ext_factors),
                              "_multiplicative_", tolower(config$adstock_multiplicative))
      optimal_adstock_rates <- config[[get_optimal_from_config]] 
    }
    #--- Adstock transformation
    print(paste0("Adstock transformation with ", lob, " optimal rates..."))
    input[lob] <- list(perform_adstock_transformation(INPUT, optimal_adstock_rates))
  }
  # --- BANDWIDTH / DELAY ----
} else if (config$choose_time_window_method == "delay"){
  input <- perform_bandwidth_delay(INPUT, output, variables, config)
} else if (config$choose_time_window_method == "none"){
  for(lob in config$output_dimentions$lob){
    input[lob] <- list(INPUT)
  }
}

resp_dimentions <- resp_dims(output,config)
outputNames <- resp_dimentions$outputNames
outputValues <- resp_dimentions$outputValues

FMat0 <- create_FMat(outputNames, outputValues, factor_table, config)

matrix_multipliers_output <- matrix_multipliers(config,colnames(input[[1]]),outputNames,variables,state_region_dat)
DMat0 <- DMatMaker(matrix_multipliers_output,input,outputNames,config)
