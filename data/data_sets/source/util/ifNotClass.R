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
if.not.class <- function(var, class){
  #' Utility function for error messages 
  #' 
  #' @description Utility function for error messages given 
  #' wrong input class as function argument.
  
  if (!any(class(var) %in% class)){
    stop(paste("first argument must be of class(es) ", 
               class, "!", sep = ""))
  }
}

# ----------------------------------------------------------- #