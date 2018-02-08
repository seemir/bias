#Load package for docstring
library(docstring)

make_na <- function(data){
  #' Converts all the NaN in a matrix to NA
  #' 
  #' @description This function returns a matrix in which all the NaN values
  #' are replaced with NA values. Note! NaN ("not a number") is not the R syntax 
  #' for missing values. The correct syntax is NA ("not available").
  #' 
  #' @param data matrix. Matrix containing NaN values
  data[is.nan(data)] <- NA
  return(data)
}

pmv <- function(data){
  #' The percentage of missing values (pmv) in all columns of a dataset
  #' 
  #' @description This function return a vector with the percentage of missing
  #' values for all variables in a dataset
  #' 
  #' @param data matrix. Matrix containing missing values
  
  pmv_vec <- seq(0, length.out = ncol(data))
  
  for (i in 1:length(pmv_vec)){
    pmv_vec[i] <- sum(is.na(data[,i])) / length(data[,i])
  }
  return(pmv_vec)
}