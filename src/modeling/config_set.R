
get_config <-function(config){
  
    
  config$start_date        <- as.POSIXlt(as.character("07/1/2018"), format = "%m/%d/%Y")
  config$end_date          <- as.POSIXlt(as.character("09/30/2021"), format = "%m/%d/%Y")
  config$agregate_output <- TRUE

  config$remove_output_dimention <- list(lob="Ltc")
  config$remove_input_dimention <- list()
  config$output_aggregation_rules <- c("lob","state","segment") # Options are: 'state', 'lob', 'product', 'region'
  config$mmo_reset_by <- "state" # either reset MMO by "state" or "region"
  config$mmo_reset_by_range <- c("state", "region")
  config$input_aggregation_rules <- c("channel", "campaign", "state")
  config$output_threshold <- 0.99
  config$output_threshold_range <- c(0.95, 0.98, 0.99)
  config$input_threshold <- 0.99
  config$input_threshold_range <- c(0.99, 0.999, 0.9999)
  config$input_missingness_threshold <- 1
  config$output_missingness_threshold <- 1
  config$hyper_parameter <- c()
  config$automatic_holdout_weeks <- FALSE
  config$automatic_holdout_weeks_count <- 10
  config$z_score <- 1.3 #DON"T CHANGE THIS ONE
  config$lm_function <- "lm" #biglm = lm for datasets too large to fit into memory
  config$vcov_fun <- "vcov" #vcov = variance/covariance matrix

  
  # Initial horizontal to vertical run
  config$metrics <- c("impressions","conversions") 

  # Adstock transformation
  config$optimize_adstock_rate <- FALSE

  # -- Optimal adstock rates
  config$Ann_adstock_ext_factors_true_multiplicative_true <- c(Audio_Streaming=0.102628313756746,Audio_Terrestrial=0.106685297806453,Cinema=0.1014549482674,Direct_Mail=0.0899392149539582,Display=0.0706276797499615,Email=0.0949360179461053,Facebook=0.107961182399458,Instagram=0.108835326980329,Newsletter=0.0942403798103454,OOH=0.0979001659367715,Pinterest=0.096455288685818,Print_Newspaper=0.106028194158356,SEM=0.157101708881143,Snapchat=0.0815690190263237,TV=0.283790192142141,Twitter=0.0913580002228181)
  config$Ann_adstock_ext_factors_false_multiplicative_true <- c(Audio_Streaming=0.101335524157403,Audio_Terrestrial=0.0985364868501433,Cinema=0.0975321508204326,Direct_Mail=0.124275599537234,Display=0.0662033414211599,Email=0.0973361908664306,Facebook=0.109993446124361,Instagram=0.0990180307616859,Newsletter=0.0982235783674604,OOH=0.103666982509105,Pinterest=0.0961771068162284,Print_Newspaper=0.113829970777631,SEM=0.173186183685556,Snapchat=0.10440090188269,TV=0.27788684537843,Twitter=0.0910934129323496)
  config$Di_adstock_ext_factors_true_multiplicative_true <- c(Audio_Streaming=0.0920916120168348,Audio_Terrestrial=0.10141161825579,Cinema=0.0984124306202615,Direct_Mail=0.0723954095066686,Display=0.0897662627814606,Email=0.0815774632155016,Facebook=0.10266186932142,Instagram=0.106305602504274,Newsletter=0.100561170147818,OOH=0.0991335436352062,Pinterest=0.100909837352968,Print_Newspaper=0.113202028031943,SEM=0.142730468354877,Snapchat=0.199126766890882,TV=0.19417585526335,Twitter=0.098774547644187)
  config$Di_adstock_ext_factors_false_multiplicative_true <- c(Audio_Streaming=0.0918164575065094,Audio_Terrestrial=0.0993688254979055,Cinema=0.0980997492962644,Direct_Mail=0.159739110493628,Display=0.0927732254315703,Email=0.0794543648307841,Facebook=0.101081455118287,Instagram=0.105277125295194,Newsletter=0.110856362612486,OOH=0.1052417081641,Pinterest=0.10431167740814,Print_Newspaper=0.117942749506499,SEM=0.147239865519275,Snapchat=0.215194155142788,TV=0.159762625055334,Twitter=0.0976684400949608)
  config$Life_adstock_ext_factors_true_multiplicative_true <- c(Audio_Streaming=0.0998710394105523,Audio_Terrestrial=0.106323178592157,Cinema=0.106411973656215,Direct_Mail=0.0900458017678233,Display=0.0989764419707107,Email=0.105727759592643,Facebook=0.0974330705347626,Instagram=0.111786108428906,Newsletter=0.0959260742396715,OOH=0.0965052941152229,Pinterest=0.0965912598900247,Print_Newspaper=0.100910108670788,SEM=0.269819491217251,Snapchat=0.0973132077438542,TV=0.363741187806805,Twitter=0.112803192339617)
  config$Life_adstock_ext_factors_false_multiplicative_true <- c(Audio_Streaming=0.0999447008616787,Audio_Terrestrial=0.100793486655152,Cinema=0.105154341589585,Direct_Mail=0.249883728976497,Display=0.0970109892565042,Email=0.103028079070574,Facebook=0.0964414757581642,Instagram=0.119040569225529,Newsletter=0.110419901662772,OOH=0.102941072581965,Pinterest=0.0998400885802473,Print_Newspaper=0.11452403488708,SEM=0.386946492935457,Snapchat=0.119177918523009,TV=0.395002464938421,Twitter=0.108659083339767)

  # --- Adstock rate optimization configurations
  config$optimize_adstock_with_ext_factors <- as.logical(NA)
  config$adstock_multiplicative <- TRUE
  config$adstock_rate_lower <- 0.001
  config$adstock_rate_upper <- 0.999
  config$adstock_rate_start <- 0.001
  config$adstock_beta_lower <- 0.001
  config$adstock_beta_upper <- Inf
  config$adstock_beta_start <- 1

  # Filtering
  config$choose_time_window_method <- "delay" # options: adstock, delay
  config$variables_list <- c("channel","campaign")
  config$b_max_range <- c(4:12) 
  config$d_max_range <- c(4:25)
  config$bw_type <- c("channel")  
  config$bw_mode <- 1  
  config$BW_DD <- c() 
  config$save_BWDD <- TRUE
  config$bw_rule <- c("channel") #TODO: due to `extract_dimension()` this only works with 1D
  config$output_bw_rule <- c("lob")

  # Factors
  config$factor_class_funs <- c()
  config$factor_normalize <- FALSE
  config$log_seasonality <- TRUE
  config$seasonality_method <- "seasonality"   ### values : seasonality, trend, both
  config$seas_trend_all <- FALSE
  
  # Model Method
  config$modeling_method <- "QP" # "linear" or "QP"
  config$variables_method <- 11 # 1 no interactive, 2 only channel_lob, 3 only campaign_lob, 4 only channel_segment
                                # 5 only campaign_segment, 6 channel_lob and campaign_lob, 
                                # 7 channel_lob and campaign_segment, 8 campaign_lob and campaign_segment
                                # 9 channel_lob and channel_segment and campaign_segment, 
                                # 10 all
  config$geo <- TRUE
  config$load_mmo <- FALSE

  # Weights
  config$ff <- 0 
  config$ff_range <- c(0.0, 0.01, 0.02)
  config$weights_seed = 105
  
  # Modeling Loop
  config$combination_threshold <- 0.2
  config$combination_threshold_range <- c(0.2, 0.3, 0.4)
  config$keep_parameters <- FALSE
  config$model_class <- 'log'
  config$PD_solve <- 1
  
  
  # crossVal
  config$do_cross_validation <- TRUE
  config$crossVal_holdout_method <- "kfold"   # options: third_week, random, costume -> 1-fold CV
  config$n_folds <- 10                                          #          kfold -> k-fold CV
  config$HoldOutStart    <- c()
  config$HoldOutEnd      <- c()
  
  # hyper-parameter tuning
  config$do_hyper_parameter_tuning <- TRUE
  config$return_coeffs <- FALSE

  
  return(config)
}


final_config <-function(config){
  
  config$start_date        <- as.POSIXlt(as.character("07/1/2018"), format = "%m/%d/%Y")
  config$end_date          <- as.POSIXlt(as.character("09/30/2021"), format = "%m/%d/%Y")
  config$agregate_output <- TRUE

  config$remove_output_dimention <- list(lob="Ltc") #default is list()
  config$remove_input_dimention <- list()
  config$output_aggregation_rules <- c("lob","state","segment") # Options are: 'state', 'lob', 'product', 'region'
  config$input_aggregation_rules <- c("channel", "campaign", "state")
  config$input_missingness_threshold <- 1 # filter input with > 80% missing values
  config$output_missingness_threshold <- 1
  config$hyper_parameter <- c()
  config$automatic_holdout_weeks <- FALSE
  config$automatic_holdout_weeks_count <- 10
  config$z_score <- 1.3 #DON"T CHANGE THIS ONE
  config$lm_function <- "lm" #biglm = lm for datasets too large to fit into memory
  config$vcov_fun <- "vcov" #vcov = variance/covariance matrix
  
  # Initial horizontal to vertical run
  config$metrics <- c("impressions","conversions") 
  
  # Adstock transformation
  config$optimize_adstock_rate <- FALSE
  
  # -- Optimal adstock rates
  config$Ann_adstock_ext_factors_true_multiplicative_true <- c(Audio_Streaming=0.102628313756746,Audio_Terrestrial=0.106685297806453,Cinema=0.1014549482674,Direct_Mail=0.0899392149539582,Display=0.0706276797499615,Email=0.0949360179461053,Facebook=0.107961182399458,Instagram=0.108835326980329,Newsletter=0.0942403798103454,OOH=0.0979001659367715,Pinterest=0.096455288685818,Print_Newspaper=0.106028194158356,SEM=0.157101708881143,Snapchat=0.0815690190263237,TV=0.283790192142141,Twitter=0.0913580002228181)
  config$Ann_adstock_ext_factors_false_multiplicative_true <- c(Audio_Streaming=0.101335524157403,Audio_Terrestrial=0.0985364868501433,Cinema=0.0975321508204326,Direct_Mail=0.124275599537234,Display=0.0662033414211599,Email=0.0973361908664306,Facebook=0.109993446124361,Instagram=0.0990180307616859,Newsletter=0.0982235783674604,OOH=0.103666982509105,Pinterest=0.0961771068162284,Print_Newspaper=0.113829970777631,SEM=0.173186183685556,Snapchat=0.10440090188269,TV=0.27788684537843,Twitter=0.0910934129323496)
  config$Di_adstock_ext_factors_true_multiplicative_true <- c(Audio_Streaming=0.0920916120168348,Audio_Terrestrial=0.10141161825579,Cinema=0.0984124306202615,Direct_Mail=0.0723954095066686,Display=0.0897662627814606,Email=0.0815774632155016,Facebook=0.10266186932142,Instagram=0.106305602504274,Newsletter=0.100561170147818,OOH=0.0991335436352062,Pinterest=0.100909837352968,Print_Newspaper=0.113202028031943,SEM=0.142730468354877,Snapchat=0.199126766890882,TV=0.19417585526335,Twitter=0.098774547644187)
  config$Di_adstock_ext_factors_false_multiplicative_true <- c(Audio_Streaming=0.0918164575065094,Audio_Terrestrial=0.0993688254979055,Cinema=0.0980997492962644,Direct_Mail=0.159739110493628,Display=0.0927732254315703,Email=0.0794543648307841,Facebook=0.101081455118287,Instagram=0.105277125295194,Newsletter=0.110856362612486,OOH=0.1052417081641,Pinterest=0.10431167740814,Print_Newspaper=0.117942749506499,SEM=0.147239865519275,Snapchat=0.215194155142788,TV=0.159762625055334,Twitter=0.0976684400949608)
  config$Life_adstock_ext_factors_true_multiplicative_true <- c(Audio_Streaming=0.0998710394105523,Audio_Terrestrial=0.106323178592157,Cinema=0.106411973656215,Direct_Mail=0.0900458017678233,Display=0.0989764419707107,Email=0.105727759592643,Facebook=0.0974330705347626,Instagram=0.111786108428906,Newsletter=0.0959260742396715,OOH=0.0965052941152229,Pinterest=0.0965912598900247,Print_Newspaper=0.100910108670788,SEM=0.269819491217251,Snapchat=0.0973132077438542,TV=0.363741187806805,Twitter=0.112803192339617)
  config$Life_adstock_ext_factors_false_multiplicative_true <- c(Audio_Streaming=0.0999447008616787,Audio_Terrestrial=0.100793486655152,Cinema=0.105154341589585,Direct_Mail=0.249883728976497,Display=0.0970109892565042,Email=0.103028079070574,Facebook=0.0964414757581642,Instagram=0.119040569225529,Newsletter=0.110419901662772,OOH=0.102941072581965,Pinterest=0.0998400885802473,Print_Newspaper=0.11452403488708,SEM=0.386946492935457,Snapchat=0.119177918523009,TV=0.395002464938421,Twitter=0.108659083339767)

  # --- Adstock rate optimization configurations
  config$optimize_adstock_with_ext_factors <- TRUE
  config$adstock_multiplicative <- TRUE
  config$adstock_rate_lower <- 0.001
  config$adstock_rate_upper <- 0.999
  config$adstock_rate_start <- 0.1
  config$adstock_beta_lower <- 0.001
  config$adstock_beta_upper <- Inf
  config$adstock_beta_start <- 1
  
  # Filtering
  config$choose_time_window_method <- "delay" # options: adstock, delay
  config$variables_list <- c("channel","campaign")
  #config$b_max <- c("Life" = 6, "Ann" = 6, "Di" = 6)
  #config$d_max <- c("Life" = 20, "Ann" = 12, "Di" = 12)
  config$bw_type <- c("channel")   
  config$bw_mode <- 1  
  config$BW_DD <- c() 
  config$save_BWDD <- TRUE
  config$bw_rule <- c("channel") #TODO: due to `extract_dimension()` this only works with 1D
  config$output_bw_rule <- c("lob")
  
  # Factors
  config$factor_class_funs <- c()
  config$factor_normalize <- FALSE
  config$log_seasonality <- TRUE  
  config$seasonality_method <- "seasonality"   ### values : seasonality, trend, both
  config$seas_trend_all <- FALSE

  # Model Method
  config$modeling_method <- "QP" # "linear" or "QP"
  config$variables_method <- 11 # 1 no inteactive, 2 only channel_lob, 3 only campaign_lob, 4 only channel_segment
                                # 5 only campaign_segment, 6 channel_lob and campaign_lob, 
                                # 7 channel_lob and campaign_segment, 8 campaign_lob and campaign_segment
                                # 9 channel_lob and channel_segment and campaign_segment, 
                                # 10 all
  config$geo <- TRUE
  config$load_mmo <- FALSE
  

  # Weights
  config$ff <- 0.00
  config$weights_seed = 105

  # Modeling Loop
  #config$combination_threshold <- 0.2
  config$keep_parameters <- FALSE
  config$model_class <- 'log'
  config$PD_solve <- 1
  
  
  # crossVal
  config$do_cross_validation <- FALSE
  config$crossVal_holdout_method <- "random"   # options: third_week, random, costume -> 1-fold CV
  config$n_folds <- 10                                           #          kfold -> k-fold CV
  config$HoldOutStart    <- c()
  config$HoldOutEnd      <- c()
  
  # hyper-parameter tuning
  config$do_hyper_parameter_tuning <- FALSE  
  config$return_coeffs <- TRUE
  
  config$output_threshold <- 0.99
  config$input_threshold <- 0.99
  config$combination_threshold <- 0.2
  config$mmo_reset_by <- "state"
  b_max_life <- 12
  b_max_ann <- 12
  b_max_di <- 12
  b_max_invs <- 12
  config$b_max <- c("Life" = b_max_life, "Ann" = b_max_ann, "Di" = b_max_di, "Invs" = b_max_invs)
  d_max_life <- 25
  d_max_ann <- 25
  d_max_di <- 25
  d_max_invs <- 25
  config$d_max <- c("Life" = d_max_life, "Ann" = d_max_ann, "Di" = d_max_di, "Invs" = d_max_invs)

  return(config)
}
