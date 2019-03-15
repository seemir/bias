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
data.bounds <- function(data, lower.bound, upper.bound){
  #' Generate an Amelia compatible bound matrix
  #' 
  #' @description This function produces a three column matrix 
  #' to hold logical bounds on the imputations done in Amelia 
  #' II. Each row of the matrix is of the form c(column.number, 
  #' lower.bound,upper.bound).
  #' 
  #' @param data matrix. Matrix like object
  #' @param lower.bound numeric. 
  #' @param upper.bound numeric.
  
  len <- ncol(data); column.number <- seq(1, len)
  lower <- rep(lower.bound, len)
  upper <- rep(upper.bound, len)
  outp <- cbind(column.number, lower, upper)
  return(outp)
}

# ----------------------------------------------------------- #