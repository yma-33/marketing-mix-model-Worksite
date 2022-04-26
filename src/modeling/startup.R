list_of_packages <- c("rJava","biglm","stringr","leaps","MASS","stats","linprog","quadprog","xlsx",
                      "data.table","robfilter","lmtest","sandwich","chron","lubridate",
                      "Matrix","futile.logger","plyr","pracma","matrixcalc","gamair","mgcv",
                      "mmlib", "dplyr", "tidyr", "readr", "caret", "minpack.lm")
if (unname(Sys.info()['sysname']=="Linux")){
  list_of_packages <- c(list_of_packages,"doMC")
} else {
  list_of_packages <- c(list_of_packages,"doSNOW")
}
# Commenting out because packages should be installed with renv::restore()
# new_packages <- list_of_packages[! (list_of_packages %in% installed.packages()[,"Package"]) ]
# if (length(new_packages)) install.packages(new_packages)
lapply(list_of_packages, require, character.only=TRUE)