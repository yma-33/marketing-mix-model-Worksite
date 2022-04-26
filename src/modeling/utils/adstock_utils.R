
####################
# ADSTOCK TRANSFORMATION
####################

#' HELPER FUNCTION in adstock_transformation() function
#' performs adstock transformation (Auto regressive linear filtering) to a column sorted by week
#' @param impressions impressions sorted by week
#' @param adstock_rate the AR rate used recursively
#' @return transformed impressions
adstock_transform_function <- function(impressions, adstock_rate) {
  return(stats::filter(x = impressions, filter = adstock_rate, method = "recursive"))
}

#' This function is used in optimize_adstock_rate() function in variable lm_eq
#' to perform adstock transformation grouping by state
#' @param impressions impressions for specific channel and state
#' @param adstock_rate optimized parameter for adstock
#' @param state_column state column associated with impressions
#' @return transformed impresssions by adstock
adstock_transformation <- function(impressions, adstock_rate, state_column = c()) {
  # Perform adstock transformation for every state in channel
  state_df <- data.frame(state = state_column, channel = impressions)
  state_df <- state_df %>%
    group_by(state) %>%
    mutate(transformed_impressions = adstock_transform_function(channel, adstock_rate))
  return(state_df$transformed_impressions)
}

#' This function uses the nlsLM() function to find the optimal adstock
#' rates for a simple linear regression or multiplicative model.
#' @param df A dataframe with both the impressions and conversions for each week & state
#' @param config configurations for optimization
#' @param factor_table external factors dataframe to join
optimize_adstock_rate <- function(df,
                                  config,
                                  factor_table) {
  # Get inputs from config
  include_external_factors <- config$optimize_adstock_with_ext_factors
  multiplicative_model <- config$adstock_multiplicative
  rate_lower <- config$adstock_rate_lower
  rate_upper <- config$adstock_rate_upper
  rate_start <- config$adstock_rate_start
  beta_lower <- config$adstock_beta_lower
  beta_upper <- config$adstock_beta_upper
  beta_start <- config$adstock_beta_start
  
  # ----- DEFINE OPTIMIZATION INPUTS
  # Multiplicative model optimizes over log(y) ~ log(X)
  if (multiplicative_model) {
    log_beginning <- "rlog("
    log_ending <- ")"
  } else { # Simple linear model
    log_beginning <- ""
    log_ending <- ""
  }

  # Programatically define the regression function (lm_eq)
  input_column_names <- df %>%
    ungroup() %>%
    select(-c(Week, state, conversions)) %>%
    colnames()
  num_column_names <- length(input_column_names)
  lm_eq <- paste0(log_beginning, "conversions", log_ending, "~b0")
  for (i in c(1:num_column_names)) {
    lm_eq <- paste0(
      lm_eq, " + b", i, "*", log_beginning, "adstock_transformation(",
      input_column_names[i], ",rate", i, ",state_column=state)", log_ending
    )
  }

  # Define the starting, upper, and lower value of regression parameters
  channel_parameter_start <- rep(c(beta_start, rate_start), num_column_names)
  parameter_start_names <- paste0(rep(c("b", "rate"), num_column_names), rep(1:num_column_names, each = 2))
  names(channel_parameter_start) <- parameter_start_names

  # The order of these parameters matches channel_parameter_start i.e. c(Intercept, b1, rate1, b2, rate2)
  parameter_start <- c(b0 = beta_start, channel_parameter_start)
  parameter_lower <- c(-Inf, rep(c(beta_lower, rate_lower), num_column_names)) # i.e. c(-Inf, -Inf, 0, -Inf, 0, -Inf, 0...)
  parameter_upper <- c(Inf, rep(c(beta_upper, rate_upper), num_column_names)) # i.e. c(Inf, Inf, 0, Inf, 0, Inf, 0...)

  # Update the regression function if adding external factors
  if (include_external_factors == TRUE) {
    # pull in external factors
    external_factors <- factor_table %>%
      select(-c(awareness, consideration, affinity, week))
    
    df <- df %>% left_join(external_factors, by = c("Week", "state"))

    # Interpolate missing agent scores
    df$agent_score <- df$agent_score %>% na.approx()
    list_external_factors <- external_factors %>%
      select(-c(Week, state)) %>%
      colnames()

    # Add external factors to lm equation
    beta_num <- num_column_names + 1
    for (external_factor in list_external_factors) {
      lm_eq <- paste0(lm_eq, " + b", beta_num, "*", external_factor)
      beta_num <- beta_num + 1
    }

    num_ext_factors <- length(list_external_factors)
    factor_start <- num_column_names + 1
    factor_parameter_start <- rep(beta_start, num_ext_factors)
    parameter_start_names <- paste0(rep("b", num_ext_factors), rep(factor_start:(factor_start - 1 + num_ext_factors)))
    names(factor_parameter_start) <- parameter_start_names

    parameter_start <- c(parameter_start, factor_parameter_start)
    parameter_lower <- c(parameter_lower, rep(-Inf, num_ext_factors))
    parameter_upper <- c(parameter_upper, rep(Inf, num_ext_factors))
  }

  # ----- OPTIMIZATION
  # Non linear model fit
  modFit <- nlsLM(lm_eq,
    data = df,
    control = nls.lm.control(maxiter = 100),
    start = parameter_start,
    lower = parameter_lower,
    upper = parameter_upper
  )

  print(summary(modFit))

  if (multiplicative_model) {
    true_values <- rlog(df$conversions)
  } else {
    true_values <- df$conversions
  }

  mse <- mean(residuals(modFit)^2)
  rss <- sum(residuals(modFit)^2)
  print(sprintf("MSE: %0.2f", mse))
  print(sprintf("RMSE: %0.2f", sqrt(mse)))
  print(sprintf("RSS: %0.2f", rss))
  print(sprintf("R^2: %0.2f", r_squared(predict(modFit), true_values)))

  # save best rates in array
  best_rates <- c()
  for (i in c(1:num_column_names)) {
    best_rates <- c(best_rates, coef(modFit)[paste0("rate", i)])
  }
  names(best_rates) <- input_column_names

  print(best_rates)
  return(best_rates)
}

#' Writes the optimal adstock rates to the bottom of the config
#' @param optimal_adstock_rates optimal adstock rates
#' @param LOB current LOB
#' @param config current config file
#' @return newly written lines in config gile
write_to_config <- function(optimal_adstock_rates, LOB, config) {
  # Append results to config file
  optimal_adstock_append <- paste(names(optimal_adstock_rates), paste0(as.vector(optimal_adstock_rates), ","), sep = "=")
  output_text <- paste0(
    "config$", LOB, "_adstock_ext_factors_", tolower(config$optimize_adstock_with_ext_factors),
    "_multiplicative_", tolower(config$adstock_multiplicative), " <- c(", str_sub(paste(optimal_adstock_append, collapse = ""), end = -2), ")"
  )
  write(output_text, "src/modeling/config_set.R", append = TRUE)
}

#' preprocess the optimal adstock rate dataframe to optimize on
#' @param INPUT Impressions table
#' @param output Conversions table
#' @param lob current line of business being transformed
#' @return dataframe used in adstock rate optimization
df_optimal_adstock_rate <- function(INPUT, output, lob) {
  # Pivot (week state) x channel table
  INPUT_derive_adstock_rate <- INPUT %>%
    pivot_longer(cols = -c(Week)) %>%
    mutate(channel = extract_dimension(name, "channel"), state = extract_dimension(name, "geo")) %>%
    select(-c(name)) %>%
    group_by(Week, channel, state) %>%
    summarise(impressions = sum(value)) %>%
    pivot_wider(names_from = channel, values_from = impressions, values_fill = 0)

  OUTPUT_derive_adstock_rate <- output %>%
    pivot_longer(cols = -c(Week)) %>%
    mutate(state = extract_dimension_output(name, "geo"), lob = extract_dimension_output(name, "lob")) %>%
    select(-c(name)) %>%
    group_by(Week, state, lob) %>%
    summarise(conversion = sum(value)) %>%
    pivot_wider(names_from = lob, values_from = conversion, values_fill = 0) %>%
    select(c(Week, state, lob)) %>%
    rename("conversions" = lob)

  # Join input/output in dataframe for the nlsLM function
  df <- dplyr::left_join(INPUT_derive_adstock_rate, OUTPUT_derive_adstock_rate, by = c("Week", "state"))
  df$conversions <- df$conversions %>% replace_na(0)
  return(df)
}


#' Performs adstock transformation with the optimal adstock rate
#' for a single input column
#' e.g. "Facebook_DTC_NY" would use the optimal adstock rate for Facebook
#' @param impressions the current impressions column
#' @param optimal_adstock_rates named list of optimal adstock rates
#' @param adstock_channel_names List of channels where optimal adstock rate was calculated
#' @return Adstock transformed impressions
adstock_transformation_optimal <- function(impressions, optimal_adstock_rates, adstock_channel_names) {
  # if Channel adstock rate was not optimized, no adstock transformation
  adstock_rate <- 0
  # Loop through all channels to find the specific channel of the current column
  for (channel in adstock_channel_names) {
    # If impressions column name contains channel
    if (grepl(paste0("_", channel, "|", channel, "_"), colnames(impressions))) {
      # Use the adstock rate for that channel
      adstock_rate <- optimal_adstock_rates[channel]
      return(stats::filter(x = impressions, filter = adstock_rate, method = "recursive"))
    }
  }
  return(stats::filter(x = impressions, filter = adstock_rate, method = "recursive"))
}


#' Loops through all input columns and perofrms adstock transformation
#' with given optimal adstock rate
#' @param INPUT impressions table
#' @param adstock_rate optimal adstock rates by channel
#' @return INPUT dataframe with adstock transformation
perform_adstock_transformation <- function(INPUT, adstock_rate) {
  # Do not apply adstock transformation to week
  INPUT_no_Week <- subset(INPUT, select = -c(Week))

  # Loop through each channel and apply adstock tranformation by channel
  for (column_name in colnames(INPUT_no_Week)) {
    INPUT_no_Week[column_name] <- adstock_transformation_optimal(INPUT_no_Week[column_name],
      optimal_adstock_rates,
      adstock_channel_names = names(optimal_adstock_rates)
    )
  }
  return(cbind(Week = INPUT$Week, INPUT_no_Week))
}
