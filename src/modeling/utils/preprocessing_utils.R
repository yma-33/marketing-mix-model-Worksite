#' HELPER FUCTION for filtering out dimensions with a low cumulative percentage of the data
#' @param output table object
#' @param threshold float object
#' @return output: output filtered by threshold
filter_low_cum_percent <- function(output, threshold){
  output_names <- colSums(output[,-1]) 
  output_names <- output_names[order(output_names, decreasing = TRUE)]
  output_names <- cumsum(output_names)/sum(output_names)
  output_names <- names(output_names[output_names <= threshold])
  
  # Filter dimensions under a certain cumulative percent threshold
  output <- output[,c("Week",output_names)]
  return(output)
}

#' HELPER FUCTION for filtering out dimensions with a high cumulative percentage of the data
#' @param input table object
#' @param threshold float object
#' @return input: input filtered by threshold
select_high_cum_percent <- function(input, threshold){
  input_names <- colSums(input[,-1]) 
  input_names <- input_names[order(input_names, decreasing = TRUE)]
  input_names <- cumsum(input_names)/sum(input_names)
  input_names <- names(input_names[input_names <= threshold])
  
  return(input_names)
}

#' Remove any input or output column that has a greater percent
#' of missing values than a certain threshold. For example, if 
#' missingness_threshold=0.8, then all columns that are more than 80%
#' Missing are removed
#' @param data_table either impressions or conversions
#' @param missingness_threshold bound for percentage of missingness 
#' @return data_table excluding columns with high missingness
filter_by_missingness <- function(data_table, missingness_threshold){
  columns_to_keep <- data_table %>% ungroup() %>% select(!Week) %>%
    summarise_all(~sum(is.na(.)) / nrow(data_table)) %>% # calculate % missingness
    pivot_longer(cols=everything()) %>% 
    filter(value < missingness_threshold) %>% # only include if % missingness < threshold 
    select(name) %>% pull()
  
  data_table <- data_table %>% select(c(Week, all_of(columns_to_keep)))
  return(data_table)
}

#' This function takes either the raw conversion or impression table
#' and performs aggregation of either conversion or impression counts on the rules specified 
#' @param data_table either raw conversion or raw impression table
#' @param config config object
#' @param values the column names to use as values - either "conversion" or "impressions"
#' @return transformed impression or conversion table
data_preprocessing <- function(data_table, config, type="output") {
  
  data_table <- data_table[as.Date(data_table$Week) >= as.Date(config$start_date) & as.Date(data_table$Week) <= as.Date(config$end_date), ]
  # Pull in configurations
  if(type=="output"){
    remove_dimension <- config$remove_output_dimention
    perc_threshold <- config$output_threshold
    config$output_dimentions <- list() 
    aggregation_rules <- c("Week", config$output_aggregation_rules)
    values <- "conversion"
    colnames(data_table)[-1] <- tolower(colnames(data_table)[-1])
    missingness_threshold <- config$output_missingness_threshold
  }
  else if(type=="input"){
    remove_dimension <- config$remove_input_dimention
    perc_threshold <- config$input_threshold
    aggregation_rules <- c("Week", config$input_aggregation_rules)
    values <- "impressions"
    missingness_threshold <- config$input_missingness_threshold
  }
  
  if (length(remove_dimension) > 0) {
    for (dim in names(remove_dimension)){
      data_table <- data_table[!(data_table[[dim]] %in% remove_dimension[[dim]]),] 
    }
  }
  
  data_table <- data_table %>%
    group_by_at(vars(all_of(aggregation_rules))) %>%
    summarise(value_column = sum(get(values)), .groups="keep") 
  
  # Rename data_table column to "conversion" or "impression"
  names(data_table)[names(data_table) == 'value_column'] <- values
  
  # Save output dimensions in config
  if(type=="output"){
    for (dims in setdiff(colnames(data_table),c("Week","conversion"))){
      config$output_dimentions[[dims]] <- unique(data_table[[dims]])
    }
  }
  
  # Pivot wider function for preprocessing
  data_table <- data_table %>% 
    pivot_wider(names_from = aggregation_rules[-1], 
                values_from = all_of(values), names_sep = "_")
  
  # Filter by percent missingness under a certain threshold
  data_table <- filter_by_missingness(data_table, missingness_threshold)
  data_table[is.na(data_table)] <- 0
  
  # select top combinations
  if(type=="input"){
    config$combinations <- select_high_cum_percent(data_table, config$combination_threshold)
  }
  
  # Filter low cumulative percent combinations
  data_table_all <- data_table 
  data_table <- filter_low_cum_percent(data_table, perc_threshold)
  return(list(data_table=data_table,data_table_all=data_table_all,config=config))
}


#' HELPER FUCTION extract_dimension_output
#' @param output_column_names vector object
#' @param select_rules string set to "lob", "geo", "segment"
#' @return dims
extract_dimension_output <- function(output_column_names, select_rules){
  
  split_names <- str_split(output_column_names, "_")
  if (select_rules=="lob"){
    dims <- sapply(split_names, function(x) x[1])
  } else if (select_rules=="geo"){
    dims <- sapply(split_names, function(x) x[2])
  } else if (select_rules=="segment"){
    dims <- sapply(split_names, function(x) x[3])
  } else {
    print("invalid input to extract_dimension_output, no action taken... ")
  }
  return(dims)
}


#' HELPER FUCTION extract_dimension for input (conversions table)
#' @param input_column_names vector object
#' @param select_rules string set to "geo", "campaign", or "channel"
#' @return dims
extract_dimension <- function(input_column_names, select_rules){
  
  split_names <- str_split(input_column_names, "_")
  if (select_rules=="geo"){
    dims <- sapply(split_names, function(x) x[length(x)])
  } else if (select_rules=="campaign"){
    dims <- sapply(split_names, function(x) x[length(x)-1])
  } else if (select_rules=="channel"){
    dims <- sapply(split_names, function(x) paste0(x[1:(length(x)-2)],collapse = "_"))
  } else {
    print("invalid input to extract_dimension, no action taken... ")
  }
  return(dims)
}

#' HELPER FUCTION  takes column names from input,output to create variables.csv as used in MML_creator function
#' @param input_column_names vector object
#' @param output_column_names vector object
#' @return table object with columns Variable, Rule, Match_Type
make_variables_list <- function(input_column_names, output_column_names){
  
  geos <- cbind(unique(extract_dimension(input_column_names,select_rules="geo")), "GEO", "INPUT")
  campaign <- cbind(unique(extract_dimension(input_column_names,select_rules="campaign")), "CAMPAIGN", "INPUT")
  channels <- cbind(unique(extract_dimension(input_column_names,select_rules="channel")), "CHANNEL", "INPUT")
  lobs <- cbind(unique(extract_dimension_output(output_column_names,select_rules="lob")), "LOB", "INPUT")
  segments <- cbind(unique(extract_dimension_output(output_column_names,select_rules="segment")), "SEGMENT", "INPUT")
  
  # Add combinations variables for channel_lob and campaign_lob
  channel_lob <- expand.grid(lobs[,1],channels[,1]) %>% 
    mutate(channel_lob = paste(Var2, Var1, sep = '_'), rule="CHANNEL_LOB") %>%
    select(channel_lob, rule) %>% 
    as.matrix()
  campaign_lob <- expand.grid(lobs[,1],campaign[,1]) %>% 
    mutate(campaign_lob = paste(Var2, Var1, sep = '_'), rule="CAMPAIGN_LOB") %>%
    select(campaign_lob, rule) %>% 
    as.matrix()
  channel_segment <- expand.grid(segments[,1],channels[,1]) %>% 
    mutate(channel_segment = paste(Var2, Var1, sep = '_'), rule="CHANNEL_SEGMENT") %>%
    select(channel_segment, rule) %>% 
    as.matrix()
  campaign_segment <- expand.grid(segments[,1],campaign[,1]) %>% 
    mutate(campaign_segment = paste(Var2, Var1,  sep = '_'), rule="CAMPAIGN_SEGMENT") %>%
    select(campaign_segment, rule) %>% 
    as.matrix()
  
  channel_lob <- cbind(channel_lob, "INPUT_OUTPUT")
  campaign_lob <- cbind(campaign_lob, "INPUT_OUTPUT")
  channel_segment <- cbind(channel_segment, "INPUT_OUTPUT")
  campaign_segment <- cbind(campaign_segment, "INPUT_OUTPUT")
  
  if (config$variables_method==1) {
    variables <- rbind(channels, campaign, geos)
  } else if (config$variables_method==2){
    variables <- rbind(channels, campaign, geos, channel_lob)
  } else if (config$variables_method==3){
    variables <- rbind(channels, campaign, geos, campaign_lob)
  } else if (config$variables_method==4){
    variables <- rbind(channels, campaign, geos, channel_segment)
  } else if (config$variables_method==5) {
    variables <- rbind(channels, campaign, geos, campaign_segment)
  } else if (config$variables_method==6){
    variables <- rbind(channels, campaign, geos, channel_lob, campaign_lob)
  } else if (config$variables_method==7){
    variables <- rbind(channels, campaign, geos, channel_lob, campaign_segment)
  } else if (config$variables_method==8){
    variables <- rbind(channels, campaign, geos, campaign_lob, campaign_segment)
  } else if (config$variables_method==9) {
    variables <- rbind(channels, campaign, geos, channel_lob, channel_segment, campaign_segment)
  } else if (config$variables_method==10){
    variables <- rbind(channels, campaign, geos, channel_lob, campaign_lob, channel_segment, campaign_segment)
  } else if (config$variables_method==11){
    variables <- rbind(channels, campaign, channel_lob, campaign_lob, channel_segment, campaign_segment)
  }
  colnames(variables) <- c("Variable", "Rule", "Match_Type")
  variables <- as.data.frame(variables)
  
  return(variables)
}

#' HELPER FUNCTION resp_dims
#' Creates a list containing: outputValues <- ordered list of output table values equal in length to DMat, FMat
#' outputNames <- table with column Week and columns named corresponding the values in config$output_aggregation_rules
#' @param output table object
#' @param config config object
#' @return list of outputValues and outputNames
resp_dims <-function(output,config){
  
  outputValues <- as.numeric(as.matrix(output[,-1]))
  outputValues <- cbind(outputValues)
  
  outputNames<- as.character(rep(output$Week,ncol(output[,-1])))
  dims <- strsplit(colnames(subset(output, select = -c(Week))), "_")
  outputNames_dims <- c()
  for(i in 1:length(dims)){
    outputNames_dims <- rbind(outputNames_dims,matrix(rep(dims[[i]],each=length(output$Week)),nrow=length(output$Week))) 
  } 
  outputNames_dims <- outputNames_dims[,1:ncol(outputNames_dims)]
  outputNames <- cbind(outputNames,outputNames_dims )
  colnames(outputNames)<-c("Week",config$output_aggregation_rules)
  
  return(list(outputValues=outputValues,outputNames=outputNames))
}

#' HELPER FUNCTION create_external_factors
#' @param outputNames table object
#' @param outputValues vector object
#' @param config config object
#' @return external factors
create_external_factors <- function(outputNames, outputValues, config) {
  
  external_factor <- c()
  factor_files <- list.files(file.path(paste0(getwd(),'/src/data/')),pattern="*.csv")
  
  if ("external_factors.csv" %in% factor_files){
    filedata <- read.csv("src/data/external_factors.csv",header=TRUE,stringsAsFactors=FALSE)
   
    # TO DO: THE FOLLOWING LINE ASSUMES COLUMN 1 IS THE WEEK COLUMN. WE'RE TRYING TO AVOID HARDCODING COLUMN NUMBERS.
    names(filedata)[2:ncol(filedata)] <- tolower(names(filedata)[2:ncol(filedata)])
    colnames(filedata)[2:ncol(filedata)] <- sapply(colnames(filedata)[2:ncol(filedata)], function(x) ifelse(x == "state" | x == "lob", x, paste0("E_",x)))
    outputNames_dataframe <- data.frame(outputNames)
    
    if (config$log_seasonality){
      outputValues_matrix <- matrix(rlog(outputValues),nrow=length(unique(outputNames[,"Week"])))
    }
    else{
      outputValues_matrix <- matrix(outputValues,nrow=length(unique(outputNames[,"Week"])))
    }
    
      outputNames_all <- apply(outputNames[,-1],1,paste,collapse="_")
      outputNames_all <- matrix(outputNames_all,nrow=length(unique(outputNames[,"Week"])))
      output_comb <- outputNames_all[1,]
      output_seas <- matrix(0,nrow=(nrow(outputValues_matrix)*ncol(outputValues_matrix)),ncol=ncol(outputValues_matrix))
      colnames(output_seas) <- paste0("E_seas_",output_comb)
      output_trend <- matrix(0,nrow=(nrow(outputValues_matrix)*ncol(outputValues_matrix)),ncol=ncol(outputValues_matrix))
      colnames(output_trend) <- paste0("E_trend_",output_comb)

      
      for (comb in 1:dim(output_seas)[2]){
        output_fit <- outputValues_matrix[,comb]
        output_fit <- ts(output_fit,frequency=52)
        output_fit <- stl(output_fit,s.window="period")
        output_seas[(((comb-1)*nrow(outputValues_matrix)+1):(comb*nrow(outputValues_matrix))),comb] <- output_fit$time.series[,"seasonal"]
        output_trend[(((comb-1)*nrow(outputValues_matrix)+1):(comb*nrow(outputValues_matrix))),comb] <- output_fit$time.series[,"trend"]
      }
      output_seas <- output_seas[,which(!apply(output_seas,2,FUN = function(x){all(x == 0)}))]
      output_trend <- output_trend[,which(!apply(output_trend,2,FUN = function(x){all(x == 0)}))]
      
      filedata <- filedata[,setdiff(colnames(filedata), c("E_season","E_trend"))]
      if ("state" %in% config$output_aggregation_rules){
        external_factor <- plyr::join(outputNames_dataframe,filedata,by=c("Week", "state"), type = "left")
        external_factor <- external_factor[,setdiff(colnames(external_factor),colnames(outputNames))]
      } else {
        # Interpolate agent_score
        filedata$E_agent_score <- filedata$E_agent_score %>% na.approx() 
        # Find the mean aggregate by week
        filedata <- filedata %>% select(!c("state"))
        filedata <- filedata %>% group_by(Week) %>% summarize_all(mean)
        filedata <- as.data.frame(filedata)
        # Join the external factors data on outputNames by Week
        external_factor <- dplyr::left_join(outputNames_dataframe,filedata,by="Week")
        external_factor <- external_factor %>% select(!(c(Week, paste0(config$output_aggregation_rules,".y"))))
      } 
      
      if (config$seasonality_method=="both"){
        external_factor <- cbind(external_factor,output_seas,output_trend)
      } else if (config$seasonality_method=="trend") {
        external_factor <- cbind(external_factor,output_trend)
      } else if (config$seasonality_method=="seasonality"){
        external_factor <- cbind(external_factor,output_seas)
      }
  }
  return(external_factor)
}

#' HELPER FUNCTION default_factors
#' CLARIFICATION: THIS FUNCTION DOESN'T SEEM TO BE COMPLETE? 
#' IS IT USED?
#' @param 
#' @return external factors
default_factors <- function(resp_extract_table_name){
  
  return(factor_class_funs)	
}

#' HELPER FUNCTION create_FMat
#' Builds table of model factors
#' @param outputNames table object
#' @param outputValues vector object
#' @param factor_table table object
#' @param config config object
#' @return FMat matrix object
create_FMat <-function(outputNames, outputValues, factor_table, config){
  
  list_of_columns <- list()
  factor_class_funs<- c()
  for (i in c(1:length(config$output_aggregation_rules))){
    factor_class_funs[toupper(config$output_aggregation_rules[i]) ]  = "identity"
  }
  factor_class_funs <- c(config$factor_class_funs, factor_class_funs)
  remove<-c()
  
  #identity factors
  
  for (i in 1:length(factor_class_funs)){
    vars_and_fun <- factor_class_funs[i]
    var_name <- names(factor_class_funs)[i]
    fun <-get(vars_and_fun)
    if (tolower(var_name) %in% colnames(outputNames)){
      value <- as.character(sapply(tolower(var_name), function(x,y) y[,x],y=outputNames))
    } else {
      value <- as.character( apply(outputNames,1,fun))
    }
    if (length(unique(value))==1){
      remove<-c(remove,var_name)
    } else {
      list_of_columns[[var_name]] <- value
    }
  }
  
  if(length(list_of_columns)==0){
    FMat<-c()
  }else{
    list_of_columns <- list_of_columns[ !(names(list_of_columns) %in% remove)]
    
    factor_class_funs<-factor_class_funs[!(names(factor_class_funs) %in% remove)]
    factors_matrix <- data.frame( list_of_columns,stringsAsFactors=FALSE ) 
    names_for_factor_class_funs<- setdiff(names(factor_class_funs), remove)
    colnames(factors_matrix)<-names_for_factor_class_funs
    
    # One hot encoding
    dmy <- dummyVars(" ~ .", data = factors_matrix)
    FMat <- predict(dmy, factors_matrix)
    colnames(FMat) <- paste0("F_",colnames(FMat))
  }	
  
  factor_table <- subset(factor_table, select = -c(week, year_iso))
  factor_table <- factor_table[factor_table$Week >= config$start_date & factor_table$Week <= config$end_date,]
  other_factor <- c()
  
  # TO DO: THE FOLLOWING LINE ASSUMES COLUMN 1 IS THE WEEK COLUMN. WE'RE TRYING TO AVOID HARDCODING COLUMN NUMBERS.
  names(factor_table)[2:ncol(factor_table)] <- tolower(names(factor_table)[2:ncol(factor_table)])
  colnames(factor_table)[2:ncol(factor_table)] <- sapply(colnames(factor_table)[2:ncol(factor_table)], function(x) ifelse(x == "state" | x == "lob", x, paste0("E_",x)))
  outputNames_dataframe <- data.frame(outputNames)

  
  if (config$seas_trend_all){
    if (config$log_seasonality){
      outputValues_matrix <- matrix(rlog(outputValues),nrow=length(unique(outputNames[,"Week"])))
    } else{
      outputValues_matrix <- matrix(outputValues,nrow=length(unique(outputNames[,"Week"])))
    }
    
    outputNames_all <- apply(outputNames[,-1],1,paste,collapse="_")
    outputNames_all <- matrix(outputNames_all,nrow=length(unique(outputNames[,"Week"])))
    output_comb <- outputNames_all[1,]
    output_seas <- matrix(0,nrow=(nrow(outputValues_matrix)*ncol(outputValues_matrix)),ncol=ncol(outputValues_matrix))
    colnames(output_seas) <- paste0("E_seas_",output_comb)
    output_trend <- matrix(0,nrow=(nrow(outputValues_matrix)*ncol(outputValues_matrix)),ncol=ncol(outputValues_matrix))
    colnames(output_trend) <- paste0("E_trend_",output_comb)
    
    
    for (comb in 1:dim(output_seas)[2]){
      output_fit <- outputValues_matrix[,comb]
      output_fit <- ts(output_fit,frequency=52)
      output_fit <- stl(output_fit,s.window="period")
      output_seas[(((comb-1)*nrow(outputValues_matrix)+1):(comb*nrow(outputValues_matrix))),comb] <- output_fit$time.series[,"seasonal"]
      output_trend[(((comb-1)*nrow(outputValues_matrix)+1):(comb*nrow(outputValues_matrix))),comb] <- output_fit$time.series[,"trend"]
    }
    output_seas<-output_seas[,which(!apply(output_seas,2,FUN = function(x){all(x == 0)}))]
    output_trend <- output_trend[,which(!apply(output_trend,2,FUN = function(x){all(x == 0)}))]
  } else {
    outputValues_lob <- data.frame(cbind(outputNames,outputValues))
    outputValues_lob$outputValues <- as.numeric(outputValues_lob$outputValues)
    outputValues_lob <- aggregate(outputValues ~ Week + lob, outputValues_lob, FUN = sum)
    if (config$log_seasonality){
      outputValues_matrix <- matrix(rlog(outputValues_lob$outputValues),nrow=length(unique(outputNames[,"Week"])))
    } else{
      outputValues_matrix <- matrix(outputValues_lob$outputValues,nrow=length(unique(outputNames[,"Week"])))
    }
    output_comb <- unique(outputValues_lob$lob)
    output_seas <- matrix(0,nrow=nrow(outputValues_matrix),ncol=ncol(outputValues_matrix))
    colnames(output_seas) <- paste0(output_comb)
    output_trend <- matrix(0,nrow=nrow(outputValues_matrix),ncol=ncol(outputValues_matrix))
    colnames(output_trend) <- paste0(output_comb)
    
    
    for (comb in 1:dim(output_seas)[2]){
      output_fit <- outputValues_matrix[,comb]
      output_fit <- ts(output_fit,frequency=52)
      output_fit <- stl(output_fit,s.window="period")
      output_seas[,comb] <- output_fit$time.series[,"seasonal"]
      output_trend[,comb] <- output_fit$time.series[,"trend"]
    }
    output_seas <- do.call(rbind, replicate(length(outputValues)/dim(output_seas)[1], output_seas, simplify=FALSE))
    output_trend <- do.call(rbind, replicate(length(outputValues)/dim(output_trend)[1], output_trend, simplify=FALSE))
    
    for (i in colnames(output_seas)){
      output_seas[which(outputNames[,"lob"]!=i),i] <- 0
      output_trend[which(outputNames[,"lob"]!=i),i] <- 0
    }
    colnames(output_seas) <- paste0("E_seas_",colnames(output_seas))
    colnames(output_trend) <- paste0("E_trend_",colnames(output_trend))
    
  }
    factor_table <- factor_table[,setdiff(colnames(factor_table), c("E_season","E_trend"))]
    if ("state" %in% config$output_aggregation_rules){
      external_factor <- plyr::join(outputNames_dataframe,factor_table,by=c("Week", "state"), type = "left")
      external_factor <- external_factor[,setdiff(colnames(external_factor),colnames(outputNames))]
    } else {
      # Interpolate agent_score
      factor_table$E_agent_score <- factor_table$E_agent_score %>% na.approx() 
      # Find the mean aggregate by week
      factor_table <- factor_table %>% select(!c("state"))
      factor_table <- factor_table %>% group_by(Week) %>% summarize_all(mean)
      factor_table <- as.data.frame(factor_table)
      # Join the external factors data on outputNames by Week
      external_factor <- dplyr::left_join(outputNames_dataframe,factor_table,by="Week")
      external_factor <- external_factor %>% select(!(c(Week, paste0(config$output_aggregation_rules,".y"))))
    } 
    
    if (config$seasonality_method=="both"){
      external_factor <- cbind(external_factor,output_seas,output_trend)
    } else if (config$seasonality_method=="trend") {
      external_factor <- cbind(external_factor,output_trend)
    } else if (config$seasonality_method=="seasonality"){
      external_factor <- cbind(external_factor,output_seas)
    }
    
  if (!is.null(external_factor)) {
    # This line is repeated
    external_factor <- as.matrix(external_factor)
    # cbind is okay in this instance because we create both by joining to dptNames
    # TODO: Maybe investigate wether it's better to do a join on Week, state, product?
    FMat <- cbind(FMat,external_factor)	
  }
  if(!is.null(FMat)){
    FMat[is.na(FMat)] <- 0
    FMat <- apply(FMat,2,as.numeric)
  }else{
    FMat<-c()
  }
  return( FMat )
}	


#' HELPER FUNCTION for assign_weights
#' @param start_date
#' @param end_date
#' @return list of start dates and end dates
random_week_holdouts <-function(start_date,end_date){
  
  number_of_testing_weeks <- difftime(end_date,start_date,unit="weeks")
  dayspan <- seq(from=ymd(start_date),to=ymd(end_date),by="weeks")
  startdates <- sample(dayspan, floor(number_of_testing_weeks/5), replace = FALSE)
  enddates <- as.Date(startdates) + 7
  enddates <- as.POSIXlt(as.character(enddates), format = "%Y-%m-%d")
  return(list(startdates=startdates,enddates=enddates))
}

#' HELPER FUNCTION assigns weights for holdout methods third_week, random, costume
#' @param outputNames table object
#' @param config config object
#' @return List of weights and cross val weeks
assign_weights <- function(outputNames,config){
  ff <- config$ff
  W <- exp(ff*as.numeric(as.Date(outputNames[,"Week"])-max(as.Date(outputNames[,"Week"]))))
  crossValWeeks_indices <- rep(1,length(W))
  crossValWeeks <- list()
  if (config$do_cross_validation){
    if (config$crossVal_holdout_method == "third_week"){
      default_holdout_values <- default_holdouts(config$start_date,config$end_date)
      crossValWeeks$HoldOutStart    <- default_holdout_values$startdates
      crossValWeeks$HoldOutEnd      <- default_holdout_values$enddates
    } else if (config$crossVal_holdout_method == "random"){
      default_holdout_values <- random_week_holdouts(config$start_date,config$end_date)
      crossValWeeks$HoldOutStart    <- default_holdout_values$startdates
      crossValWeeks$HoldOutEnd      <- default_holdout_values$enddates
    } else if (config$crossVal_holdout_method == "costume"){
      crossValWeeks$HoldOutStart <- config$HoldOutStart
      crossValWeeks$HoldOutEnd <- config$HoldOutEnd
    } 
    for(Num in c(1:length(crossValWeeks$HoldOutStart))){
      W[as.Date(outputNames[,"Week"])>=as.Date(crossValWeeks$HoldOutStart[Num]) & as.Date(outputNames[,"Week"])<as.Date(crossValWeeks$HoldOutEnd[Num])]<-0.0001
      crossValWeeks_indices[as.Date(outputNames[,"Week"])>=as.Date(crossValWeeks$HoldOutStart[Num]) & as.Date(outputNames[,"Week"])<as.Date(crossValWeeks$HoldOutEnd[Num])]<-0
    }	
    crossValWeeks$indices <- crossValWeeks_indices
  }
  
  return(list(W=W,crossValWeeks=crossValWeeks))
}

#' HELPER FUNCTION default_holdouts
#' @param start_date 
#' @param end_date
#' @return list of start dates and end dates
default_holdouts <-function(start_date,end_date){
  
  dayspan <- seq(from=start_date,to=end_date,by="weeks")
  startdates_index <- c()
  for (aday in c(1:length(dayspan))){
    daynum <- dayspan[aday]
    daynum <-day(daynum)
    if (daynum <= 21 & daynum >=15){
      startdates_index <- c(startdates_index,aday)
    }
  }
  
  startdates <- dayspan[startdates_index]
  enddates <- as.Date(startdates) + 7
  enddates <- as.POSIXlt(as.character(enddates), format = "%Y-%m-%d")
  
  return(list(startdates=startdates,enddates=enddates))
}

#' kfold_assign_weights: Sets cross validation week weights to 0 for model building for k-folds
#' @param config config object
#' @param dptNames table object 
#' @param n_folds number of cross validation folds to return
#' @return Weights List containing weights and holdout weeks
kfold_assign_weights <- function(dptNames, config, n_folds = 5) {
  start_date <- config$start_date
  end_date <- config$end_date
  
  dayspan <- seq(from=ymd(start_date),to=ymd(end_date),by="weeks")
  
  holdout_indices <- split(sample(length(dayspan), length(dayspan), replace=F), as.factor(1:n_folds))
  
  ff <- config$ff
  W_backup <- exp(ff*as.numeric(as.Date(dptNames[,"Week"])-max(as.Date(dptNames[,"Week"]))))
  
  to_return <- list()
  
  for(i in 1:n_folds){
    startdates <- dayspan[holdout_indices[[i]]]
    enddates <- startdates + 7
    
    W <- W_backup
    crossValWeeks_indices <- rep(1,length(W))
    crossValWeeks <- list()
    
    for(Num in 1:length(startdates)){
      W[as.Date(dptNames[,"Week"])>=as.Date(startdates[Num]) & as.Date(dptNames[,"Week"])<as.Date(enddates[Num])] <- 0.0001
      crossValWeeks_indices[as.Date(dptNames[,"Week"])>=as.Date(startdates[Num]) & as.Date(dptNames[,"Week"])<as.Date(enddates[Num])]<-0
    }
    
    crossValWeeks$indices <- crossValWeeks_indices
    crossValWeeks$startdates <- startdates
    crossValWeeks$enddates <- enddates
    
    to_return[[i]] <- list(W = W, crossValWeeks = crossValWeeks)
  }
  
  return(to_return)
}

#' HELPER FUNCTION match_dimensions: called in matrix_multipliers
#' @param mrule
#' @param mmo_i
#' @param Rules
#' @param match_logic
#' @param resp_i
#' @param VARNAMES
#' @param INPUTNAMES
#' @return MML index with the appropriate matchings
match_dimensions <- function(mrule, mmo_i, Rules, match_logic, resp_i, VARNAMES, INPUTNAMES){
  matching <- match_logic[mrule] 
  if(matching == "INPUT"){
    # Repeat row (INPUT) names across each column and match on column (VARIABLE) names
    input_unit <- extract_dimension(rownames(mmo_i), tolower(mrule))
    input_unit <- matrix(rep(input_unit,ncol(VARNAMES)),ncol=ncol(VARNAMES))
    mmo_i_index <- Rules[colnames(INPUTNAMES)] == mrule  
  }
  else if (mrule == "COMBINATION"){
    input_unit <- matrix(INPUTNAMES,nr=nrow(INPUTNAMES))
    mmo_i_index <- Rules[colnames(INPUTNAMES)]== mrule
  }
  else if(matching == "INPUT_OUTPUT"){
    # TODO: This is dependent on the order of INPUT_OUTPUT. So it will not work on OUTPUT_INPUT
    # Get input and output dimension names
    input_dimension_name <- strsplit(mrule, "_")[[1]][1]
    output_dimension_name <- strsplit(mrule, "_")[[1]][2]
    
    # Start by extracting specific input for each row in mmo_i
    input_unit <- extract_dimension(rownames(mmo_i), tolower(input_dimension_name))
    
    # Repeat row names across all VARIABLE columns
    input_unit <- matrix(rep(input_unit,ncol(VARNAMES)),ncol=ncol(VARNAMES))
    
    # Add "_<output dim>" to all row names to match on the INPUT_OUTPUT variable column
    output_in_mml_index <- resp_i[tolower(output_dimension_name)]
    input_unit <- apply(input_unit,c(2), function(x) paste0(x, "_", output_in_mml_index))
    
    # Determine which indices match both INPUT_OUTPUT and 
    # the output variable i.e. ("Dia", "Ann", ...) in the current mml index
    extract_output <- sapply(str_split(colnames(INPUTNAMES), "_"), function(x) x[length(x)])
    mmo_i_index <- Rules[colnames(INPUTNAMES)] == mrule & extract_output == output_in_mml_index
  }
  # Update MML index that matches the current rule with the approproate mappings
  mmo_i[,mmo_i_index] <- 1*(input_unit==VARNAMES)[,mmo_i_index]	
  return(mmo_i)
}

#' matrix_multipliers: a list of indicator matrices indexed by combinations in config$dptFields.
#' * columns of MML: independent variables to run model on
#' * rows of MML: input impression matrix column names
#' * values of MML: 0 or 1 indicating the variable name (column name) corresponds to the input name (row name)
#' @param config config object
#' @param inputnames column names from impressions (input) table
#' @param outputNames the product x state combinations
#' @param variables variables.csv as a dataframe with the model parameters
#' @param state_region_dat state --> region mapping as a dataframe
#' @return MML
matrix_multipliers <- function(config, inputnames, outputNames, variables, state_region_dat){
  
  if (config$load_mmo){
    mmo <- load("src/data/mmo_recent.Rda")
    outputCombinations <- load("src/data/outputCombinations_recent.Rda")
  } else {
    variables <- data.frame(variables)
    if (length(config$combinations)>0){
      variables <- rbind(variables, 
                         data.frame(Variable=config$combinations,Rule="COMBINATION", Match_Type="COMBINATION"))
    }
    
    VarNames <- as.character(variables$Variable) # From src/data/variables.csv
    InputNames <- setdiff(inputnames, "Week") # From Impressions table
    
    # Map all Variables to their Rule (i.e. "CHANNEL", "PRODUCT_PRODUCT")
    Rules <- as.character(variables$Rule)
    names(Rules) <- VarNames
    
    match_logic <- as.character(variables$Match_Type)
    names(match_logic) <- variables$Rule
    
    # Get all product x state combinations 
    respFields <- config$output_aggregation_rules
    outputCombinations <- cbind(outputNames[!duplicated(outputNames[,respFields]),respFields])
    colnames(outputCombinations) <- respFields
    
    VARNAMES <-matrix(rep(VarNames,each=length(InputNames)),nr=length(InputNames))
    INPUTNAMES <-matrix(rep(InputNames,length(VarNames)),     nr=length(InputNames))
    rownames(VARNAMES) <-InputNames
    colnames(VARNAMES) <-VarNames
    rownames(INPUTNAMES)<-InputNames
    colnames(INPUTNAMES)<-VarNames
    match_rules <- unique(as.character(Rules))
    
    mmo <- list() #matrix_multiplier_output
    # Loop through all unique product_state combinations
    for(i in seq(1,nrow(outputCombinations))){
      resp_i<-outputCombinations[i,] 
      names(resp_i) <- respFields
      mmo_i <- matrix(0,nrow(INPUTNAMES),ncol(VARNAMES))
      colnames(mmo_i)<-colnames(VARNAMES)
      rownames(mmo_i)<-rownames(INPUTNAMES)  
      
      ###### Matching
      for (mrule in match_rules){
        mmo_i <- match_dimensions(mrule, mmo_i, Rules, match_logic, resp_i, VARNAMES, INPUTNAMES)
      }
      
      ### Set all media from other states to zero 
      if ("state" %in% respFields && config$mmo_reset_by == "state"){
        ## Hard reset on only states that match
        mmo_i[!(extract_dimension(row.names(mmo_i),"geo") %in% outputCombinations[i,"state"]),] <- 0
      }
      else if ("state" %in% respFields && config$mmo_reset_by == "region"){
        ## Soft reset on all regions that match current output state
        curr_out_state <- outputCombinations[i,"state"]
        curr_out_region <- state_region_dat %>% filter(state==curr_out_state) %>% select(region) %>% pull()
        states_in_region <- state_region_dat %>% filter(region==curr_out_region) %>% select(state) %>% pull()
        mmo_i[!(extract_dimension(row.names(mmo_i),"geo") %in% states_in_region),] <- 0
      }
      
      ### Update feature column names
      colnames(mmo_i) <- paste0("P_", colnames(mmo_i))
      mmo[[paste(outputCombinations[i,],collapse='_')]] <- Matrix(mmo_i,sparse=TRUE)
    }
    save(mmo, file="src/data/mmo_recent.Rda")
    save(outputCombinations, file="src/data/outputCombinations_recent.Rda")
  }
  
  return(list(mmo=mmo,outputCombinations=outputCombinations))
}

#' rlog: takes the log of x values if those values are less than or equal to mn
#' and the log of mn if x is greater than mn
#' @param x vector object
#' @param mn integer object
#' @return log vector x 
rlog<-function(x,mn=1){
  x[x<=mn]<- mn
  return(log(x))
}

#' DMatMaker: creates model DMat
#' @param matrix_multipliers_output output from the function matrix_multipliers
#' @param input table object
#' @param outputNames table object
#' @param config config object
#' @return DMat
DMatMaker <- function(matrix_multipliers_output,input,outputNames,config){
  
  mmo <- matrix_multipliers_output$mmo
  outputCombinations <- matrix_multipliers_output$outputCombinations
  Dmat <- matrix(NA,nrow(outputNames),ncol(mmo[[1]]))
  Dmat_rows <- apply(cbind(outputNames[,colnames(outputCombinations)]),1,function(x){paste(x,collapse="_")})
  
  for(Dmat_value in unique(Dmat_rows)){
    if (length(config$output_bw_rule)>0){
      input_cut <- input[[names(input)[sapply(names(input), function(x) grepl(x, Dmat_value))]]]
      input_cut <- as.matrix(input_cut[,-1])
    } else {
      input_cut <- as.matrix(input[,-1])
    }
    
    m=nrow(input_cut)
    ind <- which(Dmat_rows==Dmat_value)
    ind_n <- sapply(ind,function(x){((x-1)%%m)+1})
    input_cut<- input_cut[ind_n,]
    
    if(config$model_class=='log'){
      Dmat[Dmat_rows==Dmat_value,]<- as.matrix(rlog(input_cut))%*%as.matrix(mmo[[Dmat_value]])
    } else {
      Dmat[Dmat_rows==Dmat_value,]<- input_cut%*%as.matrix(mmo[[Dmat_value]])
    }
  }
  
  colnames(Dmat)<- colnames(mmo[[1]])
  
  return(Dmat)
}



