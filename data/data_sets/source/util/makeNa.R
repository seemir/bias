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
make.na <- function(data){
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