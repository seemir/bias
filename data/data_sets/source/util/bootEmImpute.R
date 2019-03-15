# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("docstring", "Amelia")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load packages
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)

# ----------------------------------------------------------- #
boot.em.impute <- function(data, bounds, n.boot = 30){
  #' Impute data using a mean collapsing bootstrapped EM 
  #' algorithm.
  #' 
  #' @description This function imputes a data matrix using the
  #' bootstrapped EM algorihm from the Amalie II package. The 
  #' algorithm creates n.boot number of bootstrapped datasets 
  #' after which the datasets are collapsed into one dataset
  #' using the mean of all imputted values as final estimate 
  #' of the given missing value.
  #' 
  #' @param data matrix. Matrix like object
  #' @param bounds matrix. Three column matrix of the form 
  #' c(column.number, lower.bound,upper.bound).
  #' @param n.boot numeric. Number of bootstrapped datasets 
  #' to create.
  
  data.em = list()
  for (i in 1:n.boot){
    print(paste("Bootstrap: ", i, " (", i/n.boot*100, " %)",
                sep=""))
    data.em[[i]] <- amelia(data, m = 1, p2s = 0, 
                           bounds = bounds)$imputations$imp1
  }
  return(Reduce("+", data.em) / n.boot)
}

# ----------------------------------------------------------- #