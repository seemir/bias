# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("docstring")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load packages
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)

# ----------------------------------------------------------- #
rm.indicator <- function(data, n.uniq){
  #' Removes indicator variable columns from a dataset based on
  #' predefined number of unique element in that column
  #' 
  #' @description This function return a matrix without 
  #' indicator variable columns. A indicator variable column is 
  #' defined as a column containing less that a predefined 
  #' number of unique elements (n.uniq)
  #' 
  #' @param data matrix. Matrix containing indicator variables
  #' @param n.uniq integer. Number of unique element in a 
  #' column needed for that column to be defined as a indicator 
  #' variable column.
  
  non.indicator <- data[, apply(data, 2, function(col) 
    length(unique(col)) > n.uniq)]
  ind.var.idx <- !(colnames(data) %in% colnames(non.indicator))
  indicator <- data[, ind.var.idx]
  
  outp <- list(non.indicator, indicator)
  names(outp) <- c("non.indicator", "indicator")
  return(outp)
}

# ----------------------------------------------------------- #