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
  data
}

summary_mv <- function(data){
  #' Summary of the missing values in a dataset
  #' 
  #' @description This function returns a list with the total number of na
  #' values and the total percentage in the entire dataset, including the 
  #' percentage of missing values for all variables (columns) and the 
  #' relative percentage of missing values to the total (both as vectors).
  #' 
  #' @param data matrix. Matrix containing missing values

  num_na <- sum(is.na(data))
  tot_pmv <- num_na/prod(dim(data))
  num_na_vec <- apply(data, 2, function(col) sum(is.na(col)))
  pmv_vec <- num_na_vec / nrow(data) 
  rel_pmv_vec <- num_na_vec / num_na
  
  outp <- list(num_na, tot_pmv, num_na_vec, pmv_vec, rel_pmv_vec)
  names(outp) <- c('num_na', 'tot_pmv', 'num_na_vec', 'pmv_vec', 'rel_pmv_vec') 
  outp
}

summary_zv <- function(data){
  #' Summary of the zero values in a dataset
  #' 
  #' @description The function returns a list with the percentage of zero
  #' values for all variables in a dataset, including the total number of zero
  #' values and the total percentage and the relative percentage of zero values 
  #' to the total.
  #' 
  #' @param data matrix. Matrix containing zero values

  num_zeros <- sum(colSums(data == 0, na.rm = T))
  tot_pzv <- num_zeros / prod(dim(data))
  num_zeros_vec <- colSums(data == 0, na.rm = T)
  pzv_vec <- num_zeros_vec / nrow(data)
  rel_pzv_vec <- num_zeros_vec / num_zeros
  
  outp <- list(num_zeros, tot_pzv, num_zeros_vec, pzv_vec, rel_pzv_vec)
  names(outp) <- c('num_zeros', 'tot_pzv', 'num_zeros_vec', 'pzv_vec', 'rel_pzv_vec')
  outp
}

rm_indicator <- function(data, n_uniq){
  #' Removes indicator variable columns from a dataset based on predefined number of
  #' unique element in that column
  #' 
  #' @description This function return a matrix without indicator variable columns.
  #' A indicator variable column is defined as a column containing less that a 
  #' predefined number of unique elements (n_uniq)
  #' 
  #' @param data matrix. Matrix containing indicator variables
  #' @param n_uniq integer. Number of unique element in a column needed for that
  #' column to be defined as a indicator variable column.
  
  non_indicator <- data[, apply(data, 2, function(col) length(unique(col)) > n_uniq)]
  ind_var_idx <- !(colnames(data) %in% colnames(non_indicator))
  indicator <- data[, ind_var_idx]
  
  outp <- list(non_indicator, indicator)
  names(outp) <- c('non_indicator', 'indicator')
  outp
}

zero_to_na <- function(data, except=NULL){
 #' Convert zero datapoints to na in a dataset. 
 #' 
 #' @description This function converts all the zero datapoints in a dataset into na.
 #' One can also supply a vector of columnames (except) corresponding to variables 
 #' that this function should not be applied on.
 #' 
 #' @param data matrix. Matrix containing zero datapoints
 #' @param except character vector. Names of matrix column not to apply function on. 

 exp_idx <- colnames(data) %in% except
 exp_data <- data[, exp_idx]; not_exp_data <- data[, !exp_idx]
 not_exp_data[not_exp_data == 0] <- NA; 
 data <- cbind(not_exp_data, exp_data); data
}