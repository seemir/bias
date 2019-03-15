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
split.matrix <- function(data){
  #' Split matrix in two parts. 
  #' 
  #' @description This function splits a matrix into two parts. 
  #' Both halfs can be accessed by the user as an output.
  #' 
  #' @param data matrix. Matrix like object
  #' 
  #' @note The function assumes that the input matrix has more
  #' than one column.
  
  if (ncol(data)==1){
    stop("data must have more than one column!")
  }
  mid <- trunc(ncol(data)/2); end <- ncol(data)
  first.half <- data[, 1:mid]
  second.half <- data[, (mid+1):end]
  outp <- list(first.half, second.half)
  names(outp) <- c("first.half", "second.half")
  return(outp)
}

# ----------------------------------------------------------- #