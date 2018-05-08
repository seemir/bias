# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("docstring", "plotrix")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load package for docstring
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)

# ----------------------------------------------------------- #
# Helper function used in this thesis
# ----------------------------------------------------------- #
make_na <- function(data){
  #' Converts all the NaN in a matrix to NA
  #' 
  #' @description This function returns a matrix in which all 
  #' the NaN values are replaced with NA values. Note! NaN 
  #' ("not a number") is not the R syntax for missing values. 
  #' The correct syntax is NA ("not available").
  #' 
  #' @param data matrix. Matrix containing NaN values

  data[is.nan(data)] <- NA 
  return(data)
}

# ----------------------------------------------------------- #
summary_missing <- function(data){
  #' Summary of the missing values in a dataset
  #' 
  #' @description This function returns a list with the total 
  #' number of na values and the total percentage in the entire 
  #' dataset, including the percentage of missing values for 
  #' all variables (columns) and the relative percentage of 
  #' missing values to the total (both as vectors).
  #' 
  #' @param data matrix. Matrix containing missing values

  num_na <- sum(is.na(data))
  tot_pmv <- num_na/prod(dim(data))
  num_na_vec <- apply(data, 2, function(col) sum(is.na(col)))
  pmv_vec <- num_na_vec / prod(dim(data)) 
  rel_pmv_vec <- num_na_vec / num_na
  rel_pmv_v <- num_na_vec / dim(data)[1]
  
  outp <- list(num_na, tot_pmv, num_na_vec, pmv_vec, 
               rel_pmv_vec, rel_pmv_v)
  names(outp) <- c("num_na", "tot_pmv", "num_na_vec", 
                   "pmv_vec", "rel_pmv_vec", "rel_pmv_v") 
  return(outp)
}

# ----------------------------------------------------------- #
summary_zeros <- function(data){
  #' Summary of the zero values in a dataset
  #' 
  #' @description The function returns a list with the 
  #' percentage of zero values for all variables in a dataset, 
  #' including the total number of zero values and the total 
  #' percentage and the relative percentage of zero values 
  #' to the total.
  #' 
  #' @param data matrix. Matrix containing zero values

  num_zeros <- sum(colSums(data == 0, na.rm = T))
  tot_pzv <- num_zeros / prod(dim(data))
  num_zeros_vec <- colSums(data == 0, na.rm = T)
  pzv_vec <- num_zeros_vec / nrow(data)
  rel_pzv_vec <- num_zeros_vec / num_zeros
  
  outp <- list(num_zeros, tot_pzv, num_zeros_vec, pzv_vec, 
               rel_pzv_vec)
  names(outp) <- c("num_zeros", "tot_pzv", "num_zeros_vec", 
                   "pzv_vec", "rel_pzv_vec")
  return(outp)
}

# ----------------------------------------------------------- #
rm_indicator <- function(data, n_uniq){
  #' Removes indicator variable columns from a dataset based on
  #' predefined number of unique element in that column
  #' 
  #' @description This function return a matrix without 
  #' indicator variable columns. A indicator variable column is 
  #' defined as a column containing less that a predefined 
  #' number of unique elements (n_uniq)
  #' 
  #' @param data matrix. Matrix containing indicator variables
  #' @param n_uniq integer. Number of unique element in a 
  #' column needed for that column to be defined as a indicator 
  #' variable column.
  
  non_indicator <- data[, apply(data, 2, function(col) 
    length(unique(col)) > n_uniq)]
  ind_var_idx <- !(colnames(data) %in% colnames(non_indicator))
  indicator <- data[, ind_var_idx]
  
  outp <- list(non_indicator, indicator)
  names(outp) <- c("non_indicator", "indicator")
  return(outp)
}

# ----------------------------------------------------------- #
zero_to_na <- function(data, except=NULL){
 #' Convert zero datapoints to na in a dataset. 
 #' 
 #' @description This function converts all the zero datapoints 
 #' in a dataset into na. One can also supply a vector of 
 #' columnames (except) corresponding to variables that this 
 #' function should not be applied on.
 #' 
 #' @param data matrix. Matrix containing zero datapoints
 #' @param except character vector. Names of matrix column not 
 #' to apply function on. 

 exp_idx <- colnames(data) %in% except
 exp_data <- data[, exp_idx]; not_exp_data <- data[, !exp_idx]
 not_exp_data[not_exp_data == 0] <- NA; 
 data <- cbind(not_exp_data, exp_data); 
 return(data)
}

# ----------------------------------------------------------- #
move_columns <- function(from_mat, to_mat, column_name){
 #' Move one column from one matric to another. 
 #' 
 #' @description This function moves one column with name 
 #' column_name from matrix called from_mat to matrix called 
 #' to_mat.
 #' 
 #' @param from_mat matrix. Matrix to move column from
 #' @param to_mat matrix. Matrix to move column to 
 #' @param column_name character. Name of column to be moved

  to_mat <- cbind(to_mat,from_mat[, colnames(from_mat) == 
                                    column_name])
  colnames(to_mat)[ncol(to_mat)] <- column_name
  from_mat <- from_mat[, colnames(from_mat) != column_name]
  outp <- list(from_mat, to_mat)
  names(outp) <- c("from_mat","to_mat")
  outp
}

# ----------------------------------------------------------- #
top_n_missing <- function(data, n, decreasing=T){
  #' Summary of top n missing variables in data set.
  #' 
  #' @description This function produces a summary table of the
  #' top n missing variables in an inputed dataset.
  #' 
  #' @param data matrix. Matrix like object
  #' @param n integer. Top n highest missing variables 
  #' @param decreasing logical. Logical argument indicating 
  #' wheater values should be sorted in decreasing order.

  missing <- summary_missing(data)
  count <- missing$num_na_vec
  perc <- missing$pmv_vec
  relp <- missing$rel_pmv_vec
  relv <- missing$rel_pmv_v
  outp <- apply(as.matrix(cbind(count, perc, relp, relv)), 2,
                   sort, decreasing)[1:n,]
  grand_tot <- c(missing$num_na, missing$tot_pmv, sum(relp), 
                 NA)
  outp <- rbind(grand_tot, outp)  
  colnames(outp) <- c("#Na", "%N", "%Na", "%V")
  return(outp)
}

# ----------------------------------------------------------- #
label_summary <- function(labels, label_col, col_names, digits,
                          sort_col, ignore_id_col = T, 
                          decr = T){
  #' Summary of class labels in data set
  #' 
  #' @description The function returns a table with the number
  #' unique labels in a labels matrix and the percentage of
  #' all the labels that occure.
  #' 
  #' @param label matrix. Matrix like object of characters
  #' @param label_col integer. Column number of primary labels
  #' @param col_names charachter vector. Vector of column 
  #' names
  #' @param digits integer. Integer indicating the number of 
  #' decimal places to be used.
  #' @param sort_col integer. Column number to sort
  #' @param ignore_id_col logical. Boolean indicating whether
  #' first column of id numbers should be ignored.
  #' @param decr logical. Boolean indicating if values in
  #' sort_col should be sorted in decreasing order.

  uniq <- unique(if(ignore_id_col){
    labels[order(labels[, label_col]),-1]}else{labels})
  tabl <- table(labels[, label_col])
  perc <- round(tabl/sum(tabl), digits)
  outp <- cbind(uniq, tabl, perc)
  colnames(outp) <- col_names 
  return(outp[order(outp[, sort_col], decreasing = decr),])
}

# ----------------------------------------------------------- #
little_mcar <- function(data){
  #' Little's test to assess for missing completely at 
  #' random.
  #' 
  #' @description This function uses Little's test (from 
  #' BaylorEdPsych package) to assess for missing completely at 
  #' random for multivariate data with missing values. It
  #' return the chi.squared test statistics, df and p.value.
  #'
  #' @param data matrix like object. Matrix or data frame with
  #' values that are missing.
  #'
  #' @note This function cannot accept data with more than 50 
  #' variables, and may in some cases take long time to 
  #' complete.

  l <- LittleMCAR(data[, summary_missing(data)$num_na_vec > 0])
  outp <- c(dim(data)[2],l$missing.patterns, l$chi.square, 
            l$df, l$p.value)
  names(outp) <- c("n var","missing.patterns", "chi.square", "df", 
                   "p.value")
  outp[1:2] <- round(outp[1:2])
  return(outp)
}

# ----------------------------------------------------------- #
pca_var_plot <- function(pca, n_comp=NA, digits=4, title = NA){
  #' Plot the explained and cumulative variance from a
  #' principal component analysis (PCA).
  #' 
  #' @description This function produces a plot of the 
  #' explained and cumulative variance extracted from a 
  #' principal component analysis. 
  #' 
  #' @param pca princomp object. 
  #' @param n_comp integer. Number of components to be plotted
  #' @param digits integer. Integer indicating the number of 
  #' decimal places to be used.
  #' @param title character. Name of title.

  if(class(pca) != "princomp"){
    stop("first argument is not of 'princomp' class!")
  }
  
  sd <- pca$sdev
  n <- 1:ifelse(is.na(n_comp), length(sd), n_comp)
  vr <- (sd^2/sum(sd^2))[n]
  cm <- cumsum(vr)
  colfunc <- colorRampPalette(c("green", "black"))
  twoord.plot(n, vr, n, cm, type = c("bar", "s"),
              lcol = colfunc(length(n)), main = title,
              cex.axis = 0.5)
  grid()
  leg <- c(paste("Number comp:", length(n)),
           paste("Cum.variance:", round(sum(vr),digits)))
  legend("right", legend = leg, bty = "o", box.lty = "dashed")
}

# ----------------------------------------------------------- #