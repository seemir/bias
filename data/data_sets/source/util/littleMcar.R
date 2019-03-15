# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("docstring", "BaylorEdPsych")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load packages
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)

# ----------------------------------------------------------- #
little.mcar <- function(data){
  #' Little's test to assess for missing completely at 
  #' random.
  #' 
  #' @description This function uses Little's test (from 
  #' BaylorEdPsych package) to assess for missing completely at 
  #' random for multivariate data with missing values. It
  #' return the chi.squared test statistics, df and p.value.
  #'
  #' @param data matrix like object. Matrix or data frame with
  #' values that are missing.
  #'
  #' @note This function cannot accept data with more than 50 
  #' variables, and may in some cases take long time to 
  #' complete.
  
  l <- LittleMCAR(data[, summary.missing(data)$num.na.vec > 0])
  outp <- c(dim(data)[2],l$missing.patterns, l$chi.square, 
            l$df, l$p.value)
  names(outp) <- c("n var","missing.patterns", "chi.square", "df", 
                   "p.value")
  outp[1:2] <- round(outp[1:2])
  return(outp)
}

# ----------------------------------------------------------- #