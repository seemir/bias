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
top.n.missing <- function(data, n, decreasing=T){
  #' Summary of top n missing variables in data set.
  #' 
  #' @description This function produces a summary table of the
  #' top n missing variables in an inputed dataset.
  #' 
  #' @param data matrix. Matrix like object
  #' @param n integer. Top n highest missing variables 
  #' @param decreasing logical. Logical argument indicating 
  #' wheater values should be sorted in decreasing order.
  
  missing <- summary.missing(data)
  count <- missing$num.na.vec
  if (sum(count) == 0){
    stop("no missing values!")
  }
  perc <- missing$pmv.vec
  relp <- missing$rel.pmv.vec
  relv <- missing$rel.pmv.v
  outp <- apply(as.matrix(cbind(count, perc, relp, relv)), 2,
                sort, decreasing)[1:n,]
  grand.tot <- c(missing$num.na, missing$tot.pmv, sum(relp), 
                 NA)
  outp <- rbind(grand.tot, outp)  
  colnames(outp) <- c("#Na", "%N", "%Na", "%V")
  return(outp)
}
# ----------------------------------------------------------- #