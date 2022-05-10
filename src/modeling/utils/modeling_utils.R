#' rbind_mml: 
#' @param MML
#' @return intermediMML: unlisted MML matrices, all rowbinded together
rbind_mml <- function(MML){
  intermediMML<-MML[[1]]
  for (MML_no in seq(2,length(MML))){
    intermediMML<-rbind(intermediMML,MML[[MML_no]])
  }
  return(intermediMML)
}



#' solve_linear_model: 
#' @param YL
#' @param DMat
#' @param FMat
#' @param W vector of rowwise model weights
#' @param ff default is 0
#' @param lm_function default is lm
#' @param model_class default is 'log'
#' @return m: linear model fit to predict YL ~ DMat + FMat given the model class and model function
solve_linear_model <- function(YL, DMat,FMat, W, ff=0,lm_function=lm,model_class='log'){
  
  if (model_class=='log'){
    the_formula<-as.formula( "rlog(c(YL))~DMat + FMat" ) 
  } else {
    the_formula<-as.formula( "c(YL)~DMat + FMat" ) 
  }
  comb_df<- cbind( data.frame(DMat), FMat )
  comb_df <- Filter(function(x)(length(unique(x))>1), comb_df)
  if (lm_function=="biglm"){
    m <- biglm(the_formula,data=comb_df,weights=~W)
  } else {
    m <- lm(the_formula, data=comb_df,weights=W)
  }
  return(m)
}


#' solve_constrained_model: 
#' @param DMat
#' @param FMat
#' @param outputValues
#' @param mmo_constraints
#' @param model_class default is 'log'
#' @param model_weights default is 1
#' @return mc2
solve_constrained_model<-function(DMat,FMat,outputValues,mmo_constraints,model_class='log', model_weights=rep(1,nrow(DMat))){
  DMat_intercept<-cbind(1,DMat,FMat)
  colnames(DMat_intercept)<-c("Intercept",c(colnames(DMat),colnames(FMat)))
  if(length(which(model_weights!=1))){
    WDM<-DMat_intercept
    WDM<-as.matrix(sweep(WDM,1,model_weights,'*'),ncol=ncol(DMat_intercept))
  }else{
    WDM<-DMat_intercept
  }
  #DM_temp <- crossprod(DMat_intercept,WDM)
  DM_temp <- round(crossprod(DMat_intercept,WDM),digits = 4)
  if (!is.symmetric.matrix(DM_temp)){
    print("In solve_constrained_model, DM_temp is not symmetric. Using nearest pd matrix... ")
    DM_max_eig <- max(eigen(DM_temp)$values)
    threshold <-  1e-4/DM_max_eig
    DM_list<-nearPD(DM_temp, conv.tol = 1e-15, conv.norm = "F", ensureSymmetry = TRUE
                    ,eig.tol=threshold)
    DM<-DM_list$mat
  } else if (!is.positive.definite(DM_temp)) {
    if (config$PD_solve==1){
      DM_max_eig <- max(eigen(DM_temp)$values)
      threshold <-  1e-4/DM_max_eig
      DM_list<-nearPD(DM_temp, conv.tol = 1e-15, conv.norm = "F", ensureSymmetry = TRUE
                      ,eig.tol=threshold)     
      DM<-DM_list$mat
    } else if (config$PD_solve==2){
      DM <- DM_temp + diag(-min(as.vector(DM_temp_e$values)),dim(DM_temp)[1])
    } else if (config$PD_solve==3){
      DM_temp_e   = eigen(DM_temp)
      D = as.vector(DM_temp_e$values)
      D[D<0] <- 0
      D <- D * diag(length(D))
      V = DM_temp_e$vectors
      BB      = V %*% D %*% t(V)
      Tr       = 1 / sqrt(diag(BB))
      TT      = Tr %*% t(Tr)
      DM       = BB * TT
    }
    print("In solve_constrained_model, DM_temp is not positive semi-definite. Using nearest pd matrix...")
    
  } else {
    DM<-DM_temp
  }
  if (model_class=='log'){
    dv <- crossprod(rlog(outputValues),WDM)
  } else {
    dv <- crossprod(outputValues,WDM)
  }
  mc2 <- solve.QP(DM,dv, t(mmo_constraints))  
  return(mc2)
}

#' compute_insignificant: 
#' @param newCoeff
#' @param active_constraints
#' @param m
#' @param MML.ALL0
#' @param vcov_fun
#' @param base_restrict default is FALSE
#' @param z_score
#' @param replace_inverse default is FALSE
#' @return inSig
compute_insignificant<-function(newCoeff,active_constraints,m, MML.ALL0,vcov_fun,base_restrict=FALSE,z_score,replace_inverse=FALSE){
  if(base_restrict){
    R   <- as.matrix(MML.ALL0[active_constraints,setdiff(colnames(MML.ALL0),"b_value")])
  }else{
    R   <- as.matrix(MML.ALL0[active_constraints,])
  }
  if(vcov_fun=="vcovHC"){
    Sig <- vcovHC(m)
    if (any(is.na(Sig))){
      flog.warn("    Compute insignificant cannot use vcovHC!! NAs output")
      Sig <- vcov(m)}
  }else{
    Sig <- vcov(m)
  }
  if ( identical(active_constraints,as.integer(0)) ){ 
    flog.info("    active_constraints = c(0) in compute_insignificant, not solving eq.")
    newSig <- Sig
  } else {
    if (length(active_constraints)==1){R<-t(R)}
    R_sig_TR <- (R%*%Sig)%*%t(R) 
    if (any(is.na(Sig))){
      flog.warn("    Compute insignificant cannot use vcov!! NAs output")
      newSig <- Sig
    } else if (rcond(R_sig_TR) < .Machine$double.eps){
      if (replace_inverse){
        flog.warn("    Compute insignificant cannot invert matrix: returning small coefficients! (Be careful)")
        inSig<-names(newCoeff)[newCoeff<1e-10]
        return(inSig)
      } else {
        flog.warn("    Compute insignificant cannot invert matrix: returning small coefficients!")
        newSig <- Sig-Sig%*%t(R)%*%ginv(R_sig_TR)%*%R%*%Sig
      }
    } else {
      newSig <- Sig-Sig%*%t(R)%*%inv(R_sig_TR)%*%R%*%Sig #SVD decomposition
    }
  }
  
  varVal<-abs(diag(newSig))
  names(varVal)<-names(newCoeff)
  zScore<-abs(newCoeff)/sqrt(varVal)
  inSig<-names(newCoeff)[zScore<z_score & !is.na(zScore)]
  return(inSig)
}

#' combination_coefficients: 
#' @param matrix_multipliers_outputs
#' @param parameterCoeffs
#' @return coeff_all
combination_coefficients<-function(matrix_multipliers_output,parameterCoeffs){
  mmo<-matrix_multipliers_output$mmo
  coeff_list<-list()
  mmo_n<- names(mmo)
  mmo_s  <- length(mmo)
  i<-0
  for(n in mmo_n){
    coeff_cut <- data.frame(output=character(),input=character(),coeff=numeric(),stringsAsFactors=FALSE)
    mmo_cut <- mmo[[n]]
    non_zero_rows<- rownames(mmo_cut)[rowSums(mmo_cut)>0]
    i<-i+length(non_zero_rows)
    for(r in non_zero_rows){
      r_n <- mmo_cut[r,]
      pf <-  parameterCoeffs[paste(names(r_n[r_n>0]),sep='')]
      names(pf)<-paste(names(r_n[r_n>0]),sep='')
      pf[is.na(pf)]<-0
      coeff_cut[nrow(coeff_cut)+1,]<- c(n,r,sum(pf))
    }
    coeff_list[[n]] <- coeff_cut
  }
  coeff_all <- rbindlist(coeff_list)
  coeff_all <- coeff_all[abs(as.numeric(coeff_all$coeff))>0,]
  coeff_all <- as.data.frame(coeff_all,stringsAsFactors =FALSE)
  coeff_all$coeff<-as.numeric(coeff_all$coeff)
  return(coeff_all)
}

#' cross_validation: 
#' @param crossValWeeks
#' @param outputNames
#' @param outputValues
#' @param predValues
#' @param do_cross_validation
#' @return list
cross_validation <- function(crossValWeeks,outputNames,outputValues,predValues,do_cross_validation, k){
  crossValError <- list()
  if (do_cross_validation){
    # Calculate mean_absolute_proportion_error
    indices <- crossValWeeks$indices
    pred<- exp(predValues[indices==0])
    YL_ind <- outputValues[indices==0]        
    timeAgg<-outputNames[indices==0,"Week"]
    predAgg<-aggregate(pred, by=list(timeAgg), sum)
    YLAgg <- aggregate(YL_ind, by=list(timeAgg), sum)
    colnames(predAgg)<-c("Week","Predicted")
    allPred  <- data.frame(Predicted=predAgg,Actual=YLAgg[,2])
    mean_absolute_proportion_error <- mean(abs(allPred[,2]-allPred[,3])/allPred[,3])
    
    # Calculate r^2 and adjusted r^2 
    pred <- exp(predValues[indices==1])
    actual <- outputValues[indices==1]
    n = length(pred)
    r_square <- r_squared(pred, actual)
    r_square_adj <- adj_r_squared(pred, actual, n, k)
  } else {
    mean_absolute_proportion_error <- 0
    r_square <- 0
    r_square_adj <- 0
    allPred <- c()
  }
  return(list(cross_val_error=c(mean_absolute_proportion_error = mean_absolute_proportion_error, r_squared = r_square, r_squared_adj = r_square_adj),allPred=allPred,HoldOutStart=crossValWeeks$HoldOutStart,HoldOutEnd=crossValWeeks$HoldOutEnd))
}


#' model_attribution: 
#' @param input
#' @param output_all
#' @param combinationCoeffs
#' @return attr
model_attribution <- function(input,output_all,combinationCoeffs){
  
  attr <- c()
  for (dim in names(input)){
    output_cut <- output_all[,-1]
    output_cut <- output_cut[,extract_dimension_output(colnames(output_cut),'lob')==dim]
    input_cut <- input[[dim]][,setdiff(colnames(input[[dim]]),"Week")]
    combinationCoeffs_cut <- combinationCoeffs[extract_dimension_output(combinationCoeffs$output,'lob')==dim,]
    combinationCoeffs_cut <- combinationCoeffs_cut[order(combinationCoeffs_cut$output), ]
    output_transpose <- data.frame(t(output_cut))
    combinationCoeffs_output <- cbind(combinationCoeffs_cut,output_transpose[as.character(combinationCoeffs_cut$output),])
    output_min <- cbind(combinationCoeffs_output[,(ncol(combinationCoeffs_cut)+1):ncol(combinationCoeffs_output)])
    coeff_min <- matrix(rep(combinationCoeffs_output[,3],ncol(output_min)),nr=nrow(output_min),nc=ncol(output_min))
    
    input_transpose <- as.data.frame(t(input_cut),stringsAsFactors=F)
    input_transpose <- input_transpose[as.character(combinationCoeffs_cut$input),]
    input_transpose[input_transpose<=1]<-0
    input_min <- matrix(0,ncol= ncol(input_transpose),nrow = nrow(input_transpose))
    A <- 1*(input_transpose>input_min)
    attr<-rbind(attr,data.frame(combinationCoeffs_output[,c('output','input')],output_min*coeff_min*(A)))
  }
  
  week_names <- sapply(output_all$Week, toString)
  colnames(attr) <- c("output","input", week_names)
  attr_input <- data.frame(channel= extract_dimension(attr$input,select_rules="channel"),
                           campaign = extract_dimension(attr$input,select_rules="campaign"),
                           geo_output = extract_dimension(attr$input,select_rules="geo"))
  attr_output <- data.frame(lob= extract_dimension_output(attr$output,select_rules="lob"),
                            segment = extract_dimension_output(attr$output,select_rules="segment"),
                            geo_input = extract_dimension_output(attr$output,select_rules="geo"))
  attr <- data.frame(output=attr$output,attr_output,input=attr$input, attr_input, total=rowSums(attr[,week_names]), attr[,week_names])
  return(attr)
}

#' compute_MMLALL0: 
#' @param MML
#' @param FMat
#' @param DMat0
#' @param c2r
#' @return matrix
compute_MMLALL0 <- function(MML,FMat,DMat0,c2r){
  MML.ALL <- rbind_mml(MML)
  colnames(MML.ALL)<-colnames(DMat0)
  MML.ALL <- MML.ALL[,setdiff(colnames(DMat0),c2r),drop=FALSE]
  MML.ALL <-MML.ALL[rowSums(MML.ALL)>0,,drop=FALSE] 
  MML.ALL <- as( MML.ALL ,"matrix")
  MML.ALL <-MML.ALL[!duplicated(MML.ALL),,drop=FALSE]
  MML.ALL0<-cbind(0,MML.ALL,matrix(0,dim(MML.ALL)[1],dim(FMat)[2]))
  colnames(MML.ALL0)<-c("Intercept",colnames(MML.ALL),colnames(FMat))
  return(MML.ALL0)
}

#' add_factor_hyper_parameter: This function appends factors 
#' to the hyperparameter_table
#' @param hyperparameter_table data.frame of hyperparameter values
#' @param param_levels vector of parameter levels
#' @param parameter_name name of parameter column to append, matches hyperparameter name in config
#' @return hyperparameter_table data.frame of hyperparameter values
add_factor_hyper_parameter <- function(hyperparameter_table, param_levels, parameter_name){
  num_levels <- length(param_levels)
  nrow_table <- nrow(hyperparameter_table)
  names_vec <- c(names(hyperparameter_table), parameter_name)
  
  if(num_levels == 1){
    hyperparameter_table <- cbind(hyperparameter_table, rep(param_levels[1], nrow_table))
  } else{
    backup_hyperparameter_table <- hyperparameter_table
    for(i in 1:num_levels){
      if(i == 1){
        hyperparameter_table <- cbind(hyperparameter_table, param_levels[i])
      } else {
        temp <- cbind(backup_hyperparameter_table, param_levels[i])
        hyperparameter_table <- rbind(hyperparameter_table, temp)
      }
    }}
  names(hyperparameter_table) <- names_vec
  return(hyperparameter_table)
}

#' r_squared: This function calculate R squared
#' @param preds predicted Y values
#' @param actual actual Y values
#' @return R squared
r_squared <- function(preds, actual) {
  rss <- sum((preds - actual) ^ 2)
  tss <- sum((actual - mean(actual)) ^ 2)
  rsq <- 1 - rss/tss
  return(rsq)
}

#' adj_r_squared: This function calculate adjusted R squared
#' @param preds predicted Y values
#' @param actual actual Y values
#' @param n sample size
#' @param k number of variables
#' @return adjusted R squared
adj_r_squared <- function(preds, actual, n, k) {
  rss <- sum((preds - actual) ^ 2)
  tss <- sum((actual - mean(actual)) ^ 2)
  rsq <- 1 - rss/tss
  adj_rsq = (rsq * (n - 1) - k) / (n- k - 1)
  return(adj_rsq)
}

#' calculate_base: This uses model output and attribution to calculate base attribution
#' @param model_attr data.frame of attribution values by week and factor level
#' @param output data.frame of conversion counts by week
#' @return list of total attribution values by channel for each lob
calculate_base <- function(model_attr,output){
  output_no_date <- output[-1]
  attr <- list()
  lobs <- unique(extract_dimension_output(colnames(output_no_date),"lob"))
  for (l in lobs){
    attr[[l]] <- list()
    output_lob <- output_no_date[,extract_dimension_output(colnames(output_no_date),"lob")==l]
    attr_lob <- model_attr[model_attr$lob==l,]
    input_channels <- as.character(unique(attr_lob$channel))
    total_lob <- sum(colSums(output_lob))
    attr[[l]] <- data.frame(channel=input_channels,attribution=0)
    for (c in input_channels){
      attr[[l]]$attribution[attr[[l]]$channel==c] <- 100*sum(attr_lob$total[attr_lob$channel==c])/total_lob
    }
    attr[[l]] <- rbind(attr[[l]], data.frame(channel="base", attribution=100-100*(sum(attr_lob$total)/total_lob)))
  }
  return(attr)
}



