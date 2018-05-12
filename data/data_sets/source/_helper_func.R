# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("docstring", "plotrix", "FactoMineR", 
              "factoextra", "gridExtra", "NbClust",
              "ggpubr")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load package for docstring
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)

# ----------------------------------------------------------- #
# Helper function used in this thesis
# ----------------------------------------------------------- #
if.not.class <- function(var, class){
  #' Utility function for error messages 
  #' 
  #' @description Utility function for error messages given 
  #' wrong input class as function argument.
  
  if (!any(class(var) %in% class)){
    stop(paste("first argument must be a class of ", 
               class, "!", sep = ""))
  }
}
# ----------------------------------------------------------- #

make.na <- function(data){
  #' Converts all the NaN in a matrix to NA
  #' 
  #' @description This function returns a matrix in which all 
  #' the NaN values are replaced with NA values. Note! NaN 
  #' ("not a number") is not the R syntax for missing values. 
  #' The correct syntax is NA ("not available").
  #' 
  #' @param data matrix. Matrix containing NaN values

  if.not.class(data, "matrix")
  data[is.nan(data)] <- NA 
  return(data)
}

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

  if.not.class(data, "matrix")
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

  if.not.class(data, "matrix")
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
rm.indicator <- function(data, n.uniq){
  #' Removes indicator variable columns from a dataset based on
  #' predefined number of unique element in that column
  #' 
  #' @description This function return a matrix without 
  #' indicator variable columns. A indicator variable column is 
  #' defined as a column containing less that a predefined 
  #' number of unique elements (n.uniq)
  #' 
  #' @param data matrix. Matrix containing indicator variables
  #' @param n.uniq integer. Number of unique element in a 
  #' column needed for that column to be defined as a indicator 
  #' variable column.
  
  if.not.class(data, "matrix")
  non.indicator <- data[, apply(data, 2, function(col) 
    length(unique(col)) > n.uniq)]
  ind.var.idx <- !(colnames(data) %in% colnames(non.indicator))
  indicator <- data[, ind.var.idx]
  
  outp <- list(non.indicator, indicator)
  names(outp) <- c("non.indicator", "indicator")
  return(outp)
}

# ----------------------------------------------------------- #
zero.to.na <- function(data, except=NULL){
 #' Convert zero datapoints to na in a dataset. 
 #' 
 #' @description This function converts all the zero datapoints 
 #' in a dataset into na. One can also supply a vector of 
 #' columnames (except) corresponding to variables that this 
 #' function should not be applied on.
 #' 
 #' @param data matrix. Matrix containing zero datapoints
 #' @param except character vector. Names of matrix column not 
 #' to apply function on. 

  if.not.class(data, "matrix")
 exp.idx <- colnames(data) %in% except
 exp.data <- data[, exp.idx]; not.exp.data <- data[, !exp.idx]
 not.exp.data[not.exp.data == 0] <- NA
 data <- cbind(not.exp.data, exp.data) 
 return(data)
}

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

  if.not.class(from.mat, "matrix")
  if.not.class(to.mat, "matrix")
  to.mat <- cbind(to.mat,from.mat[, colnames(from.mat) == 
                                    column.name])
  colnames(to.mat)[ncol(to.mat)] <- column.name
  from.mat <- from.mat[, colnames(from.mat) != column.name]
  outp <- list(from.mat, to.mat)
  names(outp) <- c("from.mat","to.mat")
  outp
}

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

  if.not.class(data, "matrix")
  missing <- summary.missing(data)
  count <- missing$num.na.vec
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
label.summary <- function(labels, label.col, col.names, digits,
                          sort.col, ignore.id.col = T, 
                          decr = T){
  #' Summary of class labels in data set
  #' 
  #' @description The function returns a table with the number
  #' unique labels in a labels matrix and the percentage of
  #' all the labels that occure.
  #' 
  #' @param label matrix. Matrix like object of characters
  #' @param label.col integer. Column number of primary labels
  #' @param col.names charachter vector. Vector of column 
  #' names
  #' @param digits integer. Integer indicating the number of 
  #' decimal places to be used.
  #' @param sort.col integer. Column number to sort
  #' @param ignore.id.col logical. Boolean indicating whether
  #' first column of id numbers should be ignored.
  #' @param decr logical. Boolean indicating if values in
  #' sort.col should be sorted in decreasing order.

  if.not.class(labels, "matrix")
  uniq <- unique(if(ignore.id.col){
    labels[order(labels[, label.col]),-1]}else{labels})
  tabl <- table(labels[, label.col])
  perc <- round(tabl/sum(tabl), digits)
  outp <- cbind(uniq, tabl, perc)
  colnames(outp) <- col.names 
  return(outp[order(outp[, sort.col], decreasing = decr),])
}

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

  if.not.class(data, "matrix")
  l <- LittleMCAR(data[, summary.missing(data)$num.na.vec > 0])
  outp <- c(dim(data)[2],l$missing.patterns, l$chi.square, 
            l$df, l$p.value)
  names(outp) <- c("n var","missing.patterns", "chi.square", "df", 
                   "p.value")
  outp[1:2] <- round(outp[1:2])
  return(outp)
}

# ----------------------------------------------------------- #
pca.var.plot <- function(pca, n.comp=NA, digits=4, title = NA){
  #' Plot the explained and cumulative variance from a
  #' principal component analysis (PCA).
  #' 
  #' @description This function produces a plot of the 
  #' explained and cumulative variance extracted from a 
  #' principal component analysis. 
  #' 
  #' @param pca princomp object. 
  #' @param n.comp integer. Number of components to be plotted
  #' @param digits integer. Integer indicating the number of 
  #' decimal places to be used.
  #' @param title character. Name of title.

  if.not.class(pca, "princomp")
  sd <- pca$sdev
  n <- 1:ifelse(is.na(n.comp), length(sd), n.comp)
  vr <- (sd^2/sum(sd^2))[n]
  cm <- cumsum(vr)
  colfunc <- colorRampPalette(c("lightblue","blue"))
  twoord.plot(n, vr, n, cm, type = c("bar", "s"),
              lcol = colfunc(length(n)), main = title,
              cex.axis = 0.5); grid()
  lines(vr); points(vr, pch = 20)
  leg <- c(paste("Number comp:", length(n)),
           paste("Cum.variance:", round(sum(vr),digits)))
  legend("top", legend = leg, bty = "n")
}

# ----------------------------------------------------------- #
pca.cluster.plot <- function(pca, ncp, km.clust = 2, 
                             hc.clust = -1, digits = 5,
                             ellipse = T, 
                             ellipse.type = "convex",
                             ggtheme = theme_gray()){
  #' Two side-by-side cluster plots with Hierarchical 
  #' Clustering and kMeans clustering on principal components.
  #' 
  #' @description This function runs Hierarchical and kMeans 
  #' clustering on a predefined number of principal components.
  #' The results are two scatterplots with the results from
  #' the clustering.
  #' 
  #' @param pca princomp object.
  #' @param ncp numeric. Number of principal components
  #' @param km.clust numeric. Number of clusters to be used
  #' in the kMeans algorithm.
  #' @param hc.clust numeric. Number of clusters to be used 
  #' in the Hierarchical clustering.
  #' @param digits numeric. Number of decimal places for 
  #' cumulative variance in plot title.
  #' @param ellipse logical value. Boolean indicating if 
  #' ellipse around clusters should be drawn.
  #' @param ellipse.type. Type of ellipse to be drawn. 
  #' See ggscatter for more information.
  #' @param ggtheme. function, ggplot2 theme name.

  if.not.class(pca, "princomp")
  data <- as.data.frame(pca$scores[,1:ncp])
  sdev <- pca$sdev 
  cdev <- cumsum(sdev^2 / sum(sdev^2))
  
  subt <- paste("Cumulative variance: ", 
                round(cdev[ncp], digits))
  hc.title <- labs(title = paste("Hierarchical Clustering on",
                                 ncp, "Principle Components"), 
                   subtitle = subt) 
  km.title <- labs(title = paste("kMeans (k = ", km.clust, 
                                 ") Clustering on ", ncp, 
                                 " Principle Components", 
                                 sep = ""), subtitle = subt)
  hc <- fviz_cluster(HCPC(data, nb.clust = hc.clust, 
                          graph = F), main = hc.title, 
                     ellipse = ellipse, 
                     ellipse.type = ellipse.type, 
                     ggtheme = ggtheme)
  cluster <- as.factor(kmeans(data, km.clust)$cluster)
  data <- cbind(data[, 1:2], cluster)
  km <- ggscatter(data, "Comp.1", "Comp.2", 
                  color = rev("cluster"), 
                  shape = "cluster", ellipse = ellipse, 
                  ellipse.type = ellipse.type, 
                  ggtheme = ggtheme, mean.point = T, 
                  label = seq(nrow(data)),ylab = hc$labels$y,
                  xlab = hc$labels$x) + km.title
  grid.arrange(hc, km, nrow=1)
}

# ----------------------------------------------------------- #