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
summary.zeros <- function(data){
  #' Summary of the zero values in a dataset
  #' 
  #' @description The function returns a list with the 
  #' percentage of zero values for all variables in a dataset, 
  #' including the total number of zero values and the total 
  #' percentage and the relative percentage of zero values 
  #' to the total.
  #' 
  #' @param data matrix. Matrix containing zero values
  
  num.zeros <- sum(colSums(data == 0, na.rm = T))
  tot.pzv <- num.zeros / prod(dim(data))
  num.zeros.vec <- colSums(data == 0, na.rm = T)
  pzv.vec <- num.zeros.vec / nrow(data)
  rel.pzv.vec <- num.zeros.vec / num.zeros
  
  outp <- list(num.zeros, tot.pzv, num.zeros.vec, pzv.vec, 
               rel.pzv.vec)
  names(outp) <- c("num.zeros", "tot.pzv", "num.zeros.vec", 
                   "pzv.vec", "rel.pzv.vec")
  return(outp)
}
# ----------------------------------------------------------- #