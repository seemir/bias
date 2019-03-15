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
summary.missing <- function(data){
  #' Summary of the missing values in a dataset
  #' 
  #' @description This function returns a list with the total 
  #' number of na values and the total percentage in the entire 
  #' dataset, including the percentage of missing values for 
  #' all variables (columns) and the relative percentage of 
  #' missing values to the total (both as vectors).
  #' 
  #' @param data matrix. Matrix containing missing values
  
  num.na <- sum(is.na(data))
  tot.pmv <- num.na/prod(dim(data))
  num.na.vec <- apply(data, 2, function(col) sum(is.na(col)))
  pmv.vec <- num.na.vec / prod(dim(data)) 
  rel.pmv.vec <- num.na.vec / num.na
  rel.pmv.v <- num.na.vec / dim(data)[1]
  
  outp <- list(num.na, tot.pmv, num.na.vec, pmv.vec, 
               rel.pmv.vec, rel.pmv.v)
  names(outp) <- c("num.na", "tot.pmv", "num.na.vec", 
                   "pmv.vec", "rel.pmv.vec", "rel.pmv.v") 
  return(outp)
}

# ----------------------------------------------------------- #