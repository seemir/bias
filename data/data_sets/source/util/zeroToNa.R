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
zero.to.na <- function(data, except=NULL){
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
  
  exp.idx <- colnames(data) %in% except
  exp.data <- data[, exp.idx]; not.exp.data <- data[, !exp.idx]
  not.exp.data[not.exp.data == 0] <- NA
  data <- cbind(not.exp.data, exp.data) 
  return(data)
}

# ----------------------------------------------------------- #