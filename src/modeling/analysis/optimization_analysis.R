# ------------------------------------------
# ---- AD HOC ANALYSIS FOR OPTIMIZATION ----
# ------------------------------------------
# Calculate VNB attribution pull in premium
vertica_setup(server = "aws_qa",
              user = Sys.getenv("mmm_user"),
              password = Sys.getenv("mmm_pass"))
sql_fp <- "src/optimization/sql/" # sql filepath
premium_table <- get_query(sql = read_sql(paste0(sql_fp,"premiums.sql")))
vertica_disconnect()

vnb_factor <- data.frame(lob=c("Ann","Di","Life", "Ltc"), vnb_perc =c(0.0214,1.536,0.5, 1))
revenue_table <- premium_table %>% 
                    left_join(vnb_factor, by="lob") %>% 
                    mutate(vnb_num=premium*vnb_perc) %>%
                    mutate(out_comb = paste0(lob, "_", state, "_", segment))


# Get attribution VNB
View(model_attr %>%
  pivot_longer(-c(output, lob, segment, geo_input, input, channel, campaign, geo_output, total)) %>%
  mutate(week_date = as.character(gsub("X", "", name)), state=geo_input) %>%
  mutate(week_date = as.Date(gsub("\\.+", "/", week_date))) %>%
  inner_join(revenue_table, by=c("state","lob","segment")) %>%
  group_by(week_date) %>%
  summarise(Total = sum(value*vnb_num, na.rm = TRUE)))



View(as.data.frame(cbind(outputNames,outputValues)) %>%
       group_by(Week) %>%
       summarise(Total = sum(as.numeric(outputValues))))


View(as.data.frame(cbind(outputNames,outputValues)) %>%
  inner_join(revenue_table, by=c("state","lob","segment")) %>%
  mutate(revenue = as.numeric(vnb_num)*as.numeric(outputValues)) %>%
    group_by(Week) %>%
    summarise(Total = sum(revenue)))

# -------- MULTIPLICATIVE MODEL PREDICTED VALUES -------- 
# Loop through each LOB and get the predicted number of conversions
# from the multiplicative model with the combinationCoeffs data
comparison <- data.frame()
combinationCoeffs["input_names"] <- combinationCoeffs$input
long_alpha <- combinationCoeffs %>% 
                pivot_wider(id_cols = output, 
                            names_from = input_names, 
                            values_from = coeff)
for (i in long_alpha$output) {
  print(i)
  input_Life <- input[[extract_dimension_output(i, "lob")]] %>%
                      select(cols = c("Week", colnames(long_alpha %>%
                      select(-output))))
  colnames(input_Life) <- c("Week", colnames(long_alpha %>% select(-output)))
  specific_alpha <- as.matrix(long_alpha %>% filter(output == i) %>% select(-c(output)))

  # Multiplicative model
  specific_alpha[is.na(specific_alpha)] <- 0 # Replace NAs
  alpha_rep <- as.data.frame(lapply(specific_alpha, rep, nrow(input_Life)))
  colnames(alpha_rep) <- colnames(specific_alpha)
  input_Life[input_Life == 0] <- 1
  out_alpha <- cbind(Week = input_Life[1], model_out = apply(input_Life[, -1]^alpha_rep, 1, prod))
  conversions_multiplicative <- out_alpha %>%
    mutate(out_comb = i, week_date = as.Date(Week)) %>%
    left_join(revenue_table, by = c("out_comb")) %>%
    group_by(out_comb, week_date) %>%
    summarise(Total = sum(model_out, na.rm = TRUE))

  # Add new multiplicative predictions to df
  comparison <- rbind(
    comparison,
    conversions_multiplicative
  )
}

# -------- PREDICTED VALUES JUST DMat0 (parameters) -------- 
# These are predicted values if the model only uses model parameters
param_names_no_F <- setdiff(names(parameterCoeffs), 
                            setdiff(names(parameterCoeffs), colnames(DMat0)))
params_no_F <- as.data.frame(parameterCoeffs) %>%
                    mutate(paramnames = rownames(.)) %>%
                    filter(paramnames %in% param_names_no_F) %>%
                    select(parameterCoeffs) %>%
                    as.matrix()
pred_values_no_F <- as.matrix(as.data.frame(DMat0) %>% 
                                select(param_names_no_F)) %*% params_no_F
pred_values <- as.data.frame(list(outputNames, pred = exp(pred_values_no_F))) %>%
                  mutate(
                    out_comb = paste0(lob, "_", state, "_", segment),
                    week_date = as.yearmon(as.Date(Week), "%Y/%m/%d")
                  ) %>%
                  group_by(week_date, out_comb) %>%
                  summarise(predicted_by_model = sum(parameterCoeffs))


# -------- REAL NUMBER OF CONVERSIONS BY MONTH -------- 
# Get the real number of conversions from the outputValues
real_conversions_df <- as.data.frame(list(outputNames, real_values = outputValues)) %>%
  mutate( out_comb = paste0(lob, "_", state, "_", segment),
          week_date = as.Date(Week), "%Y/%m/%d") %>%
  group_by(week_date) %>%
  summarise(real_conversions = sum(outputValues))

# -------- TOTAL MODEL PREDICTIONS BY MONTH -------- 
calc_model_predictions <- as.data.frame(cbind(DMat0,FMat0)) %>%
  mutate("(Intercept)"=1) %>% 
  select(names(parameterCoeffs)) %>%
  as.matrix() %*% parameterCoeffs
calc_model_predictions_df <- as.data.frame(list(outputNames, real_values = exp(calc_model_predictions))) %>%
  mutate(
    out_comb = paste0(lob, "_", state, "_", segment),
    week_date = as.Date(Week)
  ) %>%
  group_by(week_date) %>%
  summarise(pred_tot_conversions = sum(real_values))

residual_df <- real_conversions_df %>% 
  inner_join(calc_model_predictions_df) %>% 
  mutate(residual = pred_tot_conversions-real_conversions) %>%
  select(week_date, residual)
ggplot(residual_df, aes(week_date, residual)) + geom_line()
inner_join(revenue_table, by=c("state","lob","segment")) %>%
# Compare alpha df with predicted values
# TODO double check the difference
View(comparison %>%
  inner_join(pred_values))

# Compare alpha with real values
comp_df_preds <- pred_values %>%
  group_by(week_date) %>%
  summarise(param_pred_conversions=sum(predicted_by_model)) %>%
  inner_join(real_conversions_df) %>%
  inner_join(calc_model_predictions_df) %>%
  mutate(yr_month=week_date) %>%
  select(yr_month, param_pred_conversions, pred_tot_conversions, real_conversions)
  
View(comp_df_preds)

# -------- R_squared for Q4 2020  -------- 
Q4_data <- as.data.frame(cbind(outputNames,outputValues,DMat0,FMat0)) %>% 
  mutate("(Intercept)"=1) %>% 
  filter(Week >= '2020-10-01') 
DMat0_Q4 <- Q4_data %>% select(names(parameterCoeffs)) 
pred_Q4 <- as.matrix(sapply(DMat0_Q4, as.numeric)) %*% parameterCoeffs
r_squared(as.numeric(Q4_data$outputValues), exp(pred_Q4))


# ----- Additional ad hoc analysis -----
# Model attribution * vnb by year_month
View(model_attr %>%
  pivot_longer(-c(output, lob, segment, geo_input, input, channel, campaign, geo_output, total)) %>%
  mutate(week_date = as.character(gsub("X", "", name)), state = geo_input) %>%
  mutate(week_date = as.yearmon(as.Date(gsub("\\.+", "/", week_date), "%Y/%m/%d"))) %>%
  inner_join(revenue_table, by = c("state", "lob", "segment")) %>%
  group_by(week_date) %>%
  summarise(Total = sum(value * vnb_num, na.rm = TRUE)))

# Model attribution by year_quarter
View(model_attr %>%
  pivot_longer(-c(output, lob, segment, geo_input, input, channel, campaign, geo_output, total)) %>%
  mutate(week_date = as.character(gsub("X", "", name)), state = geo_input) %>%
  mutate(week_date = as.yearqtr(as.Date(gsub("\\.+", "/", week_date), "%Y/%m/%d"))) %>%
  group_by(week_date) %>%
  summarise(Total = sum(value, na.rm = TRUE)))

# Model attribution for Life NY New 2020 Q3
View(model_attr %>%
  pivot_longer(-c(output, lob, segment, geo_input, input, channel, campaign, geo_output, total)) %>%
  filter(lob == "Life" & segment == "New" & geo_input == "NY") %>%
  mutate(week_date = as.character(gsub("X", "", name))) %>%
  mutate(week_date = as.yearqtr(as.Date(gsub("\\.+", "/", week_date), "%Y/%m/%d"))) %>%
  filter(week_date == "2020 Q3") %>%
  group_by(lob, segment, geo_input, week_date))

# Total conversions by year-quarter
base_table <- data.frame(LOB=c("Di", "Ann", "Life", "Invs"), base=c(0.95406691923,.9057021,.85881927280, .8504297))
View(conversion_table %>%
  mutate(quarter = as.yearqtr(as.Date(Week, "%Y-%m-%d"))) %>%
  filter(LOB != "Ltc") %>%
  inner_join(base_table) %>%
  group_by(quarter) %>%
  summarise(Total = sum(conversion * base, na.rm = TRUE)))

View(conversion_table %>%
       mutate(quarter = as.yearqtr(as.Date(Week, "%Y-%m-%d"))) %>%
       filter(LOB != "Ltc") %>%
       inner_join(base_table) %>%
       group_by(quarter) %>%
       summarise(Total = sum(conversion)))

# Model conversions by year-quarter
View(data.frame(outputNames, conversion = exp(model_metrics$predValues)) %>%
  mutate(quarter = as.yearqtr(as.Date(Week, "%Y-%m-%d"))) %>%
  group_by(quarter) %>%
  summarise(Total = sum(conversion, na.rm = TRUE)))


conversions_in_attribution <- model_attr %>%
  pivot_longer(-c(output, lob, segment, geo_input, input, channel, campaign, geo_output, total)) %>%
  mutate(week_date = as.character(gsub("X", "", name))) %>%
  mutate(week_date = as.yearqtr(as.Date(gsub("\\.+", "/", week_date), "%Y/%m/%d"))) %>%
  filter(week_date == "2020 Q3") %>%
  group_by(lob, segment, geo_input, week_date) %>%
  summarise(Total = sum(value, na.rm = TRUE))

write.csv(conversions_in_attribution, "test2.csv")

# Compare to conversions in log linear model
comparison <- data.frame()
combinationCoeffs["input_names"] <- combinationCoeffs$input
long_alpha <- combinationCoeffs %>% pivot_wider(id_cols = output, names_from = input_names, values_from = coeff)
for (i in long_alpha$output) {
  input_Life <- input[[extract_dimension_output(i, "lob")]] %>%
    select(cols = c("Week", colnames(long_alpha %>%
      select(-output))))
  colnames(input_Life) <- c("Week", colnames(long_alpha %>% select(-output)))
  specific_alpha <- as.matrix(long_alpha %>% filter(output == i) %>% select(-c(output)))
  # Log linear
  # alpha_rep <- as.data.frame(lapply(specific_alpha, rep, nrow(input_Life)))
  # colnames(alpha_rep) <- colnames(specific_alpha)
  # out_alpha <- cbind(Week = input_Life[1], model_out = exp(rowSums(rlog(input_Life[, -1]) * alpha_rep, na.rm = TRUE)))
  # conversions_log_linear <- out_alpha %>%
  #   mutate(out_comb = i, week_date = as.yearmon(as.Date(Week), "%Y/%m/%d")) %>%
  #   group_by(out_comb, week_date) %>%
  #   summarise(Total = sum(model_out, na.rm = TRUE))

  # Multiplicative
  specific_alpha[is.na(specific_alpha)] <- 0
  alpha_rep <- as.data.frame(lapply(specific_alpha, rep, nrow(input_Life)))
  colnames(alpha_rep) <- colnames(specific_alpha)
  input_Life[input_Life == 0] <- 1
  out_alpha <- cbind(Week = input_Life[1], model_out = apply(input_Life[, -1]^alpha_rep, 1, prod))
  conversions_multiplicative <- out_alpha %>%
    mutate(out_comb = i, week_date = as.yearmon(as.Date(Week), "%Y/%m/%d")) %>%
    group_by(out_comb, week_date) %>%
    summarise(Total = sum(model_out, na.rm = TRUE))

  # aggregating first
  # aggregated_M <- input_Life %>%
  #   mutate(week_date = as.yearqtr(as.Date(Week), "%Y/%m/%d")) %>%
  #   filter(week_date == "2020 Q3") %>%
  #   ungroup() %>%
  #   select(-c(Week, week_date)) %>%
  #   colSums() %>%
  #   t()
  # conversions_aggregated_ll <- exp(sum(rlog(aggregated_M) * specific_alpha, na.rm = TRUE))
  # aggregated_M[aggregated_M == 0] <- 1
  # conversions_aggregated <- apply(aggregated_M^specific_alpha, 1, prod)
  # comparison <- rbind(comparison, c(
  #   i,
  #   conversions_log_linear,
  #   conversions_multiplicative,
  #   conversions_aggregated,
  #   conversions_aggregated_ll
  # ))
  comparison <- rbind(comparison,
    conversions_multiplicative)
}

# get the CPI

aggr_impr_by_channel <- input$Di %>%
  pivot_longer(-c(Week)) %>%
  mutate(channel = extract_dimension(name, "channel")) %>%
  group_by(channel) %>%
  summarise(impr_after = sum(value))

impr_by_channel_before <- impression_table %>%
  group_by(channel) %>%
  summarise(impr_before = sum(impressions))

View(aggr_impr_by_channel %>% inner_join(impr_by_channel_before) %>% mutate(perc_diff = (impr_after - impr_before) / impr_before))

# aggregated impressions
aggr_impr_by_channel <- input$Ann %>%
  pivot_longer(-c(Week)) %>%
  mutate(channel_campaign = paste0(
    extract_dimension(name, "channel"),
    "_", extract_dimension(name, "campaign")
  )) %>%
  group_by(channel_campaign) %>%
  summarise(impr_after_transform = sum(value))

impr_by_channel_before <- spend_table %>%
  mutate(channel_campaign = paste0(channel, "_", campaign)) %>%
  group_by(channel_campaign) %>%
  summarise(spend = sum(spend))
View(aggr_impr_by_channel %>% inner_join(impr_by_channel_before) %>% mutate(spend / impr_after_transform))

aggr_impr_by_channel <- input$Di %>%
  pivot_longer(-c(Week)) %>%
  mutate(channel = extract_dimension(name, "channel")) %>%
  group_by(channel) %>%
  summarise(impr_after = sum(value))

impr_by_channel_before <- impression_table %>%
  group_by(channel) %>%
  summarise(impr_before = sum(impressions))

View(aggr_impr_by_channel %>% inner_join(impr_by_channel_before) %>% mutate(perc_diff = (impr_after - impr_before) / impr_before))
