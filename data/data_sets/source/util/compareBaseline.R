# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("docstring", "CBCgrps")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load package for docstring
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)

# ----------------------------------------------------------- #
compare.baseline <- function(data, grp, alpha=0.05){
  #' Compare baseline characteristics between two groups.
  #' 
  #' @description This function compares the baseline charact-
  #' eristics between two sample groups using an automated
  #' process for determining the distribution of continious
  #' variabels and the appropriate tests. The Wilcoxon rank 
  #' sum test is applied for categorical variables.
  #' 
  #'  @param data matrix like object. Matrix or data frame.
  #'  @param grp. group variable
  #' 
  #' @references Zhang Z. Univariate description and bivariate 
  #' statistical inference: the first step delving into data.
  #' Ann Transl Med. 2016 Mar;4(5):91.
  
  if (length(unique(data[, grp]))>2){
    grp.table <- multigrps(data, grp, sim=T)$table
  }else{
    grp.table <- twogrps(data, grp, sim=T)$table
  }
  grp.list <- list(sum(grp.table[,ncol(grp.table)]<alpha),
                   grp.table)
  return(grp.list)
}

# ----------------------------------------------------------- #