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
sort.column.names <- function(data, id.col = T){
  #' Sorts columns from data
  #' 
  #' @description This function sorts the columns names of an
  #' matrix like object.
  #' 
  #' @param data matrix. Matrix with columns names
  #' @id.col boolean. Logical indicating if data contains 
  #' an id column.
  
  if(id.col){
    id <- data[, 1]
    data <- data[,-1]
    data <- cbind(id, data[,sort(colnames(data))])
  }else{
    data <- data[,sort(colnames(data))]
  }
  return(data)
}
# ----------------------------------------------------------- #