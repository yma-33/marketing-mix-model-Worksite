#' This function runs StimRespCorr over all unique entries of RSMAP$Response 
#' using corresponding set of RSMAP$Stimulation 
#' for the correlation analysis, see StimRespCorr for details
#' @param DAT
#' @param RSMAP
#' @param dmax default is 4, max number of weeks to delay
#' @param bmax default is 12, max number of weeks to include in the bandwidth
#' @param CVAR default is an empty vector
#' @param datefield name of column containing the date in DAT
#' @param save_BWDD default is TRUE
#' @return R
StimRespCorrBATCH <- function(DAT,RSMAP,dmax=4,bmax=12,side=1,CVAR=c(),datefield=datefield,save_BWDD=TRUE){
  RSMAP<-apply(RSMAP,2,as.character)
  R<-c()
  for (rname in unique(RSMAP[,'Response'])){
    stnames<-RSMAP[RSMAP[,'Response']==rname,'Stimulation']
    unames<-setdiff(colnames(RSMAP),c('Response','Stimulation','BDate','EDate','bmax','dmax'))
    Unit<-unique(RSMAP[RSMAP[,'Response']==rname,unames])
    names(Unit)<-unames
    rrname<-rname
    if(!is.null(CVAR)){
      if(length(CVAR[CVAR[,'Response']==rname,'Residual'])>0){
        rrname<-as.character(CVAR[CVAR[,'Response']==rname,'Residual'])
      }
    }
    DATE_AND_FILT_MAP<-rbind(RSMAP[RSMAP[,'Response']==rname,])
    temp<-StimRespCorr(DAT,rrname,stnames,dmax=dmax,bmax=bmax,side=side,Unit=Unit,DATE_AND_FILT_MAP,datefield=datefield,save_BWDD)
    temp$Response[temp$Response==rrname]<-rname
    R<-rbind(R,temp)
  }
  return(R)
}


#'This function runs automated correlation analysis between the variable specified by response string 
#'  in DAT and stimulation variables in DAT specified by array of strings stimulations. The outcome will 
#'  be a matrix the provides correlations between the response-stimulation pairs after application of 
#'  lagCalc(stimulation,response,dmax,bmax) to optimize the correlation wrt lookbackwindow (bmax) and 
#'  conversion window(dmax). The results are merged with Unit
#' Example
#'  StimulationResponseCorr(data.frame(Y1=Y1,Y2=Y2,Z1=Z1,X1=X1,X2=X2,X3=X3,X4=X4),"Y1",c("X1","X2"),
#'  4,12,c(Unit="A",Type="Y")) may create an output like
#'
#'     Unit Type  Response Stimulation     Corr      bopt   dopt
#'     'A'   'Y'    'Y1'      X1          0.6         5      1
#'     'A'   'Y'    'Y1'      X2          0.7         8      3
#' If bdate and edate are specified only data within the specified period used in the correlation 
#'   analysis. By default it is assumed that the field specifying date is 'Week'. If not the name 
#'   of the field can be supplied using datefield
#' @param DAT
#' @param response 
#' @param stimulations
#' @param dmax default is 4, max number of weeks to delay
#' @param bmax default is 12, max number of weeks to include in the bandwidth
#' @param side default is 1
#' @param Unit default is an empty vector
#' @param DATE_AND_FILT_MAP 
#' @param datefield default is 'Week'; indicates the name of the column in DAT containing the date
#' @param save_BWDD 
#' @return R
StimRespCorr<-function(DAT,response,stimulations,dmax=4,bmax=12,side=1,Unit=c(),DATE_AND_FILT_MAP,datefield='Week',save_BWDD){
  datevec<-as.Date(as.character(DAT[[datefield]]))
  R<-c()
  tailing<-c()
  #Loop through every stimulation to find optimal correlation to response vi call to lagCalc
  for (stname in stimulations){
    cond<-rep(TRUE,nrow(DAT))
    this_bmax<-bmax
    this_dmax<-dmax
    
    bdate<-DATE_AND_FILT_MAP[DATE_AND_FILT_MAP[,'Stimulation']==stname,'BDate']
    edate<-DATE_AND_FILT_MAP[DATE_AND_FILT_MAP[,'Stimulation']==stname,'EDate']
    temp1<-DATE_AND_FILT_MAP[DATE_AND_FILT_MAP[,'Stimulation']==stname,'bmax']
    temp2<-DATE_AND_FILT_MAP[DATE_AND_FILT_MAP[,'Stimulation']==stname,'dmax']
    
    if (!is.na(bdate)){cond[datevec < as.Date(as.character(bdate))]<-FALSE}
    if (!is.na(edate)){cond[datevec > as.Date(as.character(edate))]<-FALSE}
    if (!is.na(temp1)){this_bmax<-as.numeric(temp1)}
    if (!is.na(temp2)){this_dmax<-as.numeric(temp2)}
    #Run correlations over different values of b and d (lookback and conversion windows)
    Ri<-lagCalc(DAT[cond,stname],DAT[cond,'response'],dmax=this_dmax,bmax=this_bmax)
    Ri[is.na(Ri[,3]),3] <- -100
    colnames(Ri) <- c(paste0(stname,"_DD"),paste0(stname,"_BW"),paste0(stname,"_correlation"))
    tailing<-cbind(tailing,Ri)
    if (side==1){
      #Select the row with maximum correlation
      ri<-rbind(Ri[Ri[,3]==max(Ri[,3]),])[1,] 
    } else {
      #Select the row with maximum correlation
      ri<-rbind(Ri[abs(Ri[,3])==max(abs(Ri[,3])),])[1,] 
    }
    #Bind the row found to R
    R<-rbind(R,c('Response'=response,'Stimulation'=stname,'Corr'=ri[3],'bopt'=ri[2],'dopt'=ri[1])) 
  }
  if (save_BWDD) {
    file_name <- paste0("src/data/",response,"_tailingEffect.csv")
    write.csv(tailing,file_name,row.names=FALSE)
  }
  #Bind the columns that label the entries of the matrix according to any arbitrary set of labels
  m2add <- matrix(rep(Unit,each=nrow(R)),nr=nrow(R)) 
  colnames(m2add)<-names(Unit)
  R<-cbind(m2add,R)
  R<-data.frame(R,stringsAsFactors=FALSE) #Make R into a data.frame
  R$Corr<-as.numeric(R$Corr) 
  R$bopt<-as.numeric(R$bopt)
  R$dopt<-as.numeric(R$dopt)
  return(R)
}


#' Runs correlations between variable y and different lagged and delayed transformations of x (up to maximum lag and delay values
#' specified by bmax and dmax) and creates a summary table of the result. It is used to automatically calculate the right level
#' stimulation lag and delay.
#' @param x
#' @param y
#' @param dmax default is 4, max number of weeks to delay
#' @param bmax default is 12, max number of weeks to include in the bandwidth
#' @return R
lagCalc<-function(x,y,dmax=4,bmax=12){
  R<-c()
  for (d in seq(0,dmax)){  # delay = how many weeks to look back
    for (b in seq(1,bmax)){ # bandwidth = moving average window length
      w<-c(rep(0,d),rep(1,b))
      w<-w/sum(w)
      xf<- stats::filter(rlog(x),w,sides=1)
      R<-rbind(R,c(d,b,cor(xf,rlog(y),use="complete.obs")))
    }
  }
  return(R)
}


#' make_t_INPUT_aggregate
#' Note: This function assumes column naming convention of CHANNEL_PRODUCT_GEO and assumes
#' no additional factors beyond these three. (In short, this function is idiosyncratic to 
#' the data we used at the beginning of this work in January 2021.) 
#' Further, this function assumes that subsetting by more than 2 rules will not be necessary.
#' @param input
#' @param rule_table
#' @param by_rules
#' @return to_return
make_t_INPUT_aggregate <- function(input, rule_table, by_rules) {
  
  if(length(by_rules) > 2){
    stop("by_rules must be of length 1 or 2. Subsetting by more than 2 rules is not supported.")
  }
  
  if(length(by_rules) > 1){
    rule1_list <- rule_table[tolower(rule_table$Rule) %in% by_rules[1],][1]
    rule2_list <- rule_table[tolower(rule_table$Rule) %in% by_rules[2],][1]
    to_return <- as.data.frame(matrix(nrow = length(rule1_list$Variable)*length(rule2_list$Variable), ncol = nrow(input)+1))
    names(to_return) <- c("Group.1", names(to_return)[-ncol(to_return)])
    iterator = 1
    for(rule1 in rule1_list$Variable){
      rule1_columns <- grepl(rule1, colnames(input), fixed = TRUE)
      for(rule2 in rule2_list$Variable){
        rule2_columns <- grepl(rule2, colnames(input), fixed = TRUE)
        to_return[iterator,1] <- str_c(c(rule1, rule2), collapse = "-")
        subset_input <- input[,rule1_columns & rule2_columns]
        to_return[iterator, 2:ncol(to_return)] <- rowSums(subset_input)
        iterator = iterator + 1
      }
    }
  } else {
    rule_list <- rule_table[tolower(rule_table$Rule) %in% by_rules,][1]
    to_return <- as.data.frame(matrix(nrow = length(rule_list$Variable), ncol = nrow(input)+1))
    names(to_return) <- c("Group.1", names(to_return)[-ncol(to_return)])
    iterator = 1
    for(rule in rule_list$Variable){
      rule_columns <- grepl(rule, colnames(input), fixed = TRUE)
      to_return[iterator, 1] <- rule 
      subset_input <- input[, rule_columns]
      to_return[iterator, 2:ncol(to_return)] <- rowSums(subset_input)
      iterator = iterator + 1
    }
  }
  return(to_return)
}

#' ### BANDWIDTH/DELAY MAIN FUNCTION ###
#' This function runs the bandwidth delay calculation over the entire INPUT dataset.
#' In addition to the bandwidth delay transformed input matrix, this function writes the table
#' 'src/data/bandwidth_delay_table.csv', which is used to created the attribution_table
#' for the Tableau team at the end of this pipeline.
#' @param INPUT table object
#' @param output table object
#' @param variables table object
#' @param config config object
#' @return input table transformed by band
perform_bandwidth_delay <- function(INPUT, output, variables, config) {
  
  ord   <- 1
  t_INPUT <- make_t_INPUT_aggregate(INPUT, variables, by_rules = config$bw_rule)
  #test_two_rules_t_INPUT <- make_t_INPUT_aggregate(INPUT, by_rules = c("CHANNEL", "PRODUCT"))
  rownames(t_INPUT) <- t_INPUT[,1]
  t_INPUT <- t_INPUT[,-1]
  input   <- t(t_INPUT)
  
  Week <- output$Week
  INPUT_LIST <- list()
  
  delay_tab <- data.frame(lob = character,
                          channel = character(),
                          bandwidth = numeric(),
                          delay = numeric())
  for (res_comb in config$output_bw_rule){
    for (res_comb_dim in config$output_dimentions[[config$output_bw_rule]]){
      
      b_max <- config$b_max[res_comb_dim]
      d_max <- config$d_max[res_comb_dim]
      
      INPUT_r <- INPUT
      output_delay_calc <- as.matrix(rowSums(data.frame(output[,which(grepl(res_comb_dim, colnames(output)))])))
      colnames(output_delay_calc) <- "response"
      DAT <- data.frame(Week,input,output_delay_calc)
      RSMAP <- data.frame(rep(colnames(output_delay_calc),ncol(input)),
                          rep(colnames(input),each=ncol(output_delay_calc)),
                          rep(config$start_date,ncol(input)),
                          rep(config$end_date,  ncol(input)),
                          rep(b_max,     ncol(input)),
                          rep(d_max,     ncol(input)),
                          rep(ord,       ncol(input)))
      colnames(RSMAP)<-c("Response","Stimulation","BDate","EDate","bmax","dmax","Order")
      Ri <- StimRespCorrBATCH(DAT,RSMAP,dmax=d_max,bmax= b_max,side=1,CVAR=c(),datefield="Week",TRUE)

      BW <- Ri$bopt
      DD <- Ri$dopt
      names(BW) <- Ri$Stimulation
      names(DD) <- Ri$Stimulation
      
      delay_tab <- rbind(delay_tab, data.frame("lob" = rep(res_comb_dim, length(BW)), "channel" = Ri$Stimulation, "bandwidth" = Ri$bopt, "delay" = Ri$dopt))
      
      INPUT1 <- INPUT_r
      for(bwi in names(BW)[!(BW==1 & DD==0)]){
        # TO DO: extract_dimension FUNCTION ONLY WORKS FOR ONE-D
        # IF WE WANT TO GROUP BY MORE THAN ONE-D, NEED TO UPDATE extract_dimension
        print(paste0("for lob " ,res_comb_dim, " and channel ", bwi, " bandwidth is ",  BW[bwi], " and delay is ", DD[bwi]))
        FILCOLS<- c("Week", extract_dimension(setdiff(names(INPUT),"Week"),select_rules=config$bw_rule))
        FILCOLS <- FILCOLS==bwi
        bw<-BW[bwi]
        d<-DD[bwi]
        for(j in which(FILCOLS)){
          INPUT_r[(bw+d):nrow(INPUT_r),j]<-stats:::filter(INPUT1[,j],c(rep(0,d),rep(1,bw))/bw,side=1)[(bw+d):nrow(INPUT_r)]
        }
      }
      INPUT_LIST[[res_comb_dim]] <- INPUT_r
    }
  }
  
  colnames(delay_tab) <- c("lob", "channel", "bandwidth", "delay")
  print("Writing table src/data/bandwidth_delay_table.csv....")
  write_csv(delay_tab, "src/data/bandwidth_delay_table.csv")
  return(INPUT_LIST)
}
