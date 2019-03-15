# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("docstring", "plotrix", "FactoMineR", 
              "factoextra", "gridExtra", "ggpubr", "mclust")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load package for docstring
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)

# ----------------------------------------------------------- #
pca.cluster.plot <- function(pca, ncp, km.clust = 2,
                             hc.clust = -1, em.clust = 2,
                             digits = 5, ellipse = T, 
                             actual = NA, fcp=1, scp = 2,
                             ellipse.type = "convex",
                             ggtheme = theme_gray(),
                             return.clust=F){
  #' Side-by-side cluster plots with Hierarchical Clustering, 
  #' kMeans and EM clustering on principal components.
  #' 
  #' @description This function runs Hierarchical, kMeans and 
  #' EM clustering on a predefined number of principal 
  #' components. The results are scatterplots with the 
  #' results from the clustering.
  #' 
  #' @param pca princomp object.
  #' @param ncp numeric. Number of principal components
  #' @param km.clust numeric. Number of clusters to be used
  #' in the kMeans algorithm.
  #' @param hc.clust numeric. Number of clusters to be used 
  #' in the Hierarchical clustering.
  #' @param em.clust numeric. Number of clusters to be used
  #' in the expectation maximization algorithm.
  #' @param digits numeric. Number of decimal places for 
  #' cumulative variance in plot title.
  #' @param ellipse logical value. Boolean indicating if 
  #' ellipse around clusters should be drawn.
  #' @param ellipse.type. Type of ellipse to be drawn. 
  #' See ggscatter for more information.
  #' @param ggtheme. function, ggplot2 theme name.
  #' @param return.clust. logical. Boolean indicating wheather
  #' one want to return the cluster partioning.
  
  if.not.class(pca, "princomp")
  data <- as.data.frame(pca$scores[,1:ncp])
  sdev <- pca$sdev 
  rdev <- sdev^2 / sum(sdev^2)
  cdev <- cumsum(rdev)
  subt <- paste("Cum.variance: ",round(cdev[ncp], digits))
  hc.title <- labs(title=paste("Hierarchical Clustering"),
                   subtitle= subt) 
  km.title <- labs(title = paste("kMeans (k = ", km.clust, 
                                 ") Clustering", sep = ""),subtitle = subt)
  em.title <- labs(title = paste("EM Clustering"),
                   subtitle = subt)
  xlab <- paste("Dim", fcp, "(", 
                round((rdev[fcp])*100, 2), 
                "%)", sep = "")
  ylab <- paste("Dim", scp," (", 
                round((rdev[scp])*100, 2),"%)", 
                sep = "")
  hc.cluster <- HCPC(data, nb.clust = hc.clust, 
                     graph = F)$data.clust$clust
  km.cluster <- as.factor(kmeans(data, km.clust)$cluster)
  em.cluster <- as.factor(Mclust(data[,1:ncp], 
                                 em.clust)$classification)
  if (all(is.na(actual))){
    data <- cbind(data[, fcp:scp], hc.cluster, km.cluster, 
                  em.cluster)
  }else{
    actual <- as.factor(actual)
    data <- cbind(data[, fcp:scp], hc.cluster, km.cluster, 
                  em.cluster,
                  actual)
  }
  hc <- ggscatter(data, paste("Comp.", fcp, sep=""), 
                  paste("Comp.", scp, sep=""),
                  color = "hc.cluster",ylab=ylab, xlab=xlab,
                  shape = "hc.cluster", ellipse = ellipse,
                  ellipse.type = ellipse.type,
                  ggtheme = ggtheme, mean.point = T,
                  label = seq(nrow(data))) + hc.title
  km <- ggscatter(data, paste("Comp.", fcp, sep=""), 
                  paste("Comp.", scp, sep=""),
                  color = "km.cluster", ylab=ylab, xlab=xlab,
                  shape = "km.cluster", ellipse = ellipse,
                  ellipse.type = ellipse.type,
                  ggtheme = ggtheme, mean.point = T,
                  label = seq(nrow(data))) + km.title
  em <- ggscatter(data, paste("Comp.", fcp, sep=""), 
                  paste("Comp.", scp, sep=""),
                  color = "em.cluster",ylab=ylab, xlab=xlab,
                  shape = "em.cluster", ellipse = ellipse,
                  ellipse.type = ellipse.type,
                  ggtheme = ggtheme, mean.point = T,
                  label = seq(nrow(data))) + em.title
  if (all(is.na(actual))){
    grid.arrange(hc, km, em, nrow = 2)
  }else{
    act <- ggscatter(data, paste("Comp.", fcp, sep=""), 
                     paste("Comp.", scp, sep=""),
                     color = "actual", shape = "actual",
                     ellipse = ellipse, 
                     ellipse.type = ellipse.type,
                     ggtheme = ggtheme,
                     label = seq(nrow(data)),ylab=hc$labels$y,
                     xlab = hc$labels$x) +
      labs(title = "Actual Clustering", subtitle = "")
    grid.arrange(act, hc, km, em, nrow = 2)
  }
  if (return.clust){
    clust.list <- list(as.numeric(actual),
                       as.numeric(hc.cluster),
                       as.numeric(km.cluster),
                       as.numeric(em.cluster))
    names(clust.list) <- c("ACT", "HC", "KMC", "EMC")
    return(clust.list)
  }
}

# ----------------------------------------------------------- #