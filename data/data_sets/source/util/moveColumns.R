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
move.columns <- function(from.mat, to.mat, column.name){
  #' Move one column from one matric to another. 
  #' 
  #' @description This function moves one column with name 
  #' column.name from matrix called from.mat to matrix called 
  #' to.mat.
  #' 
  #' @param from.mat matrix. Matrix to move column from
  #' @param to.mat matrix. Matrix to move column to 
  #' @param column.name character. Name of column to be moved
  
  to.mat <- cbind(to.mat,from.mat[, colnames(from.mat) == 
                                    column.name])
  colnames(to.mat)[ncol(to.mat)] <- column.name
  from.mat <- from.mat[, colnames(from.mat) != column.name]
  outp <- list(from.mat, to.mat)
  names(outp) <- c("from.mat","to.mat")
  return(outp)
}

# ----------------------------------------------------------- #