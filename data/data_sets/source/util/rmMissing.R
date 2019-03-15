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
rm.missing <- function(data, cut.off = 0.8, near.zero.var = T){
  #' Remove variables with near zero variance or more missing
  #' values than a percentage threshold.
  #' 
  #' @description This function removes all variables in a 
  #' matrix or dataframe with suspected of having near zero
  #' variance or more missing values than a given percentage
  #' threshold.
  #' 
  #' @param data matrix. Matrix like object
  #' @param cut.off integer. Percentage threshold for missing
  #' values.
  #' @param near.zero.var logical. Boolean indicating if 
  #' criteria for near zero variance is to be used. 
  
  if (near.zero.var){
    near.zero <- nearZeroVar(data)
    if (length(near.zero) != 0){
      data <- data[, -near.zero]
    }    
  }
  miss.col <- summary.missing(data)$rel.pmv.v
  miss.cut <- miss.col < cut.off
  data <- data[, miss.cut]
  return(data)
}
# ----------------------------------------------------------- #