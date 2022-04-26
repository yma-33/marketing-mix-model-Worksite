
cross_validation_inner_loop <- function(config, DMat0, FMat0, outputValues, outputNames) {
  models <- list()
  combinationCoeffsList <- list()
  parameterCoeffsList <- list()
  
  if(config$crossVal_holdout_method == "kfold" && config$do_cross_validation == TRUE){
    set.seed(config$weights_seed)
    Weights_all <- kfold_assign_weights(outputNames, config, n_folds = config$n_folds)
    n_fold <- config$n_folds
  } else{
    Weights_all  <- assign_weights(outputNames,config)
    n_fold <- 1
  }
  
  for (validation_loop in 1:n_fold) {
    
    print(paste0("LOOP # ", validation_loop))
    if (config$crossVal_holdout_method=="kfold") {
      Weights <- Weights_all[[validation_loop]]
    } else{
      Weights <- Weights_all
    }
    
    excluded_parameters <- c()
    model_stabilized<-FALSE
    loop_count1 <- 0

    while (!model_stabilized){
      loop_count1 <- loop_count1 + 1
      #flog.info( paste0("Model Loop ",loop_count1,"-th "))
      excluded_parameters_log <- excluded_parameters
      
      do_linear_loop <- TRUE
      loop_count2 <- 0
      
      Dmat_X <- excluded_parameters
      Fmat_X <- excluded_parameters
      DMat1<- DMat0[,setdiff(colnames(DMat0),Dmat_X),drop=FALSE]
      FMat1<- FMat0[,setdiff(colnames(FMat0),Fmat_X),drop=FALSE]
      
      while(do_linear_loop){
        loop_count2 <- loop_count2 + 1
        #flog.info( paste0("Linear Model Loop ",loop_count2,"-th "))
        
        lin_model  <- solve_linear_model(outputValues, DMat1,FMat1, Weights$W, ff=config$ff,lm_function=config$lm_function,model_class=config$model_class)
        lin_model_X <- names(coef(lin_model)[is.na(coef(lin_model))])
        
        if (length(lin_model_X)>0){
          Dmat_X  <- str_replace( lin_model_X[substr(lin_model_X,1,4)=="DMat"], 'DMat', '' )
          Fmat_X  <- str_replace( lin_model_X[substr(lin_model_X,1,4)=="FMat"], 'FMat', '' )
          Dmat_X <- unique(c(excluded_parameters,Dmat_X))
          Fmat_X <- unique(c(excluded_parameters,Fmat_X))
          DMat1<- DMat1[,setdiff(colnames(DMat1),Dmat_X),drop=FALSE]
          FMat1<- FMat1[,setdiff(colnames(FMat1),Fmat_X),drop=FALSE]
        } else {
          do_linear_loop <- FALSE
        }
      }
      
      #print(paste0("The r.square of linear model is ", summary(lin_model)$r.squared, " and its adjusted r2 is ", summary(lin_model)$adj.r.squared))
      
      mmo_constraints <- compute_MMLALL0(matrix_multipliers_output$mmo,FMat1,DMat0,Dmat_X) 
      qp_model <- solve_constrained_model(DMat1,FMat1,outputValues,mmo_constraints,config$model_class,
                                     Weights$W)

      parameterCoeffs<-qp_model$solution
      DMat_intercept<-cbind(1,DMat1,FMat1)
      predValues <- DMat_intercept %*% parameterCoeffs
      names(parameterCoeffs)<-c("(Intercept)",c(colnames(DMat1),colnames(FMat1)))
      
      active_constraints<-qp_model$iact
      excluded_parameters_qp <- compute_insignificant(parameterCoeffs,active_constraints,lin_model, mmo_constraints,config$vcov_fun,base_restrict=FALSE,config$z_score)
      
      excluded_parameters_qp <- setdiff( excluded_parameters_qp, c('(Intercept)',excluded_parameters_log))
      excluded_parameters_factors <- excluded_parameters_qp[substr(excluded_parameters_qp,1,2) %in% c("F_","E_")]
      
      if (config$keep_parameters) {
        excluded_parameters_qp <- excluded_parameters_factors
      }
      
      if (length(excluded_parameters_qp)==0) {
        #flog.info("No new parameter to exclude")
        excluded_parameters <- excluded_parameters_log
        exit_status <- 2
      } else if ( length(excluded_parameters_factors)>0){
        exit_status<-1
        #flog.info("Excluding factors only")
        excluded_parameters <- union(excluded_parameters_log,excluded_parameters_factors)
      } else {
        excluded_parameters <- union(excluded_parameters_log,excluded_parameters_qp)
        exit_status <- 2
      }
      
      
      new_excludes <- setdiff(excluded_parameters,excluded_parameters_log)
      #flog.info( paste0("  New excludes: ",paste(new_excludes,collapse=",")))
      
      if ( length(new_excludes) == 0 ){
        #flog.info( "  Stabilized model")
        if (exit_status==2){
          model_stabilized<-TRUE
        }
      }
      
    }
    
    
    combinationCoeffs <- combination_coefficients(matrix_multipliers_output,parameterCoeffs)
    cross_validation_results <- cross_validation(Weights$crossValWeeks,outputNames,outputValues,predValues,config$do_cross_validation, length(parameterCoeffs))
    
    #attribution_matrix <- model_attribution(input,output,combinationCoeffs)
    models[[validation_loop]] <- cross_validation_results
    if(config$return_coeffs){
      combinationCoeffsList[[validation_loop]] <- combinationCoeffs
      parameterCoeffsList[[validation_loop]] <- parameterCoeffs
    }
  }
  
  avg_error <- c()
  avg_r_square <- c()
  avg_adj_r_square <- c()
  
  for(k in 1:n_fold){
    avg_error[k] <- models[[k]]$cross_val_error[["mean_absolute_proportion_error"]]
    avg_r_square[k] <- models[[k]]$cross_val_error[["r_squared"]]
    avg_adj_r_square[k] <- models[[k]]$cross_val_error["r_squared_adj"]
  }
 
  
  print(avg_error)
  print(avg_adj_r_square)
  
  if(config$return_coeffs){
    to_return <- list(mean_absolute_proportion_error = mean(avg_error), 
                     r_squared = mean(avg_r_square), 
                     adj_r_squared = mean(avg_adj_r_square), 
                     error_folds = avg_error,
                     r_square_folds = avg_r_square, 
                     adj_r_square_folds = avg_adj_r_square, 
                     combinationCoeffs = combinationCoeffsList, 
                     parameterCoeffs = parameterCoeffsList)
  } else {
    to_return <- list(mean_absolute_proportion_error = mean(avg_error), 
                      r_squared = mean(avg_r_square), 
                      adj_r_squared = mean(avg_adj_r_square), 
                      error_folds = avg_error,
                      r_square_folds = avg_r_square, 
                      adj_r_square_folds = avg_adj_r_square)
  }
  
  return(to_return)
}

