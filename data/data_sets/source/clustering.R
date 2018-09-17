# ----------------------------------------------------------- #
# Install relevant packages (if not already done)
# ----------------------------------------------------------- #
Packages <- c("NbClust")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load relevant packages
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)
source("utilities.R")

# ----------------------------------------------------------- #
# Load pca objects and source utility functions
# ----------------------------------------------------------- #
allDataFiles <- c("HFfullpca", "HFpEFpca", "HFmrEFpca",
                  "HFfullImp","HFpEFimp", "HFmrEFimp", 
                  "SyndClass")
lapply(gsub(" ", "", paste("data_files/", allDataFiles, 
                           ".Rdat")), load,.GlobalEnv)

# ----------------------------------------------------------- #
# Determine optimal number of clusters
# ----------------------------------------------------------- #
NbClust(HFfullpca$scores[,1:31], min.nc = 2, max.nc = 4,
        method = "kmeans")
NbClust(HFpEFpca$scores[,1:31], min.nc = 2, max.nc = 4,
        method = "kmeans")
NbClust(HFmrEFpca$scores[,1:31], min.nc = 2, max.nc = 4, 
        method = "kmeans")

# ----------------------------------------------------------- #
# PCA cluster plot for all data sets
# ----------------------------------------------------------- #
clustFull <- pca.cluster.plot(HFfullpca, 2, km.clust = 2, 
                              hc.clust = 2, em.clust = 2, 
                              actual = SyndClass[,2], 
                              return.clust = T, ellipse = F)

# ----------------------------------------------------------- #
# Extract cluster configuration and add to data frame
# ----------------------------------------------------------- #
ACT <- clustFull$ACT
HC <- clustFull$HC
KM <- clustFull$KM
EM <- clustFull$EM

# ----------------------------------------------------------- #
# Compare baseline characteristics
# ----------------------------------------------------------- #
compare.baseline(cbind(HFfullImp, ACT), "ACT")
compare.baseline(cbind(HFfullImp, HC), "HC")
compare.baseline(cbind(HFfullImp, KM), "KM")
compare.baseline(cbind(HFfullImp, EM), "EM")

# ----------------------------------------------------------- #
# Assuming clustering by physicians is correct
# ----------------------------------------------------------- #
clustPef <- pca.cluster.plot(HFpEFpca, 2, km.clust = 3, 
                             hc.clust = 3, em.clust = 3, 
                             return.clust = T, ellipse = F)

clustMr <- pca.cluster.plot(HFmrEFpca, 2, km.clust = 3, 
                            hc.clust = 3, em.clust = 3, 
                            return.clust = T, ellipse = F)

# ----------------------------------------------------------- #
# Compare baseline characteristics HFpEF
# ----------------------------------------------------------- #
HC <- clustPef$HC
KM <- clustPef$KM
EM <- clustPef$EM

compare.baseline(cbind(HFpEFimp, HC), "HC")
compare.baseline(cbind(HFpEFimp, KM), "KM")
compare.baseline(cbind(HFpEFimp, EM), "EM")

# ----------------------------------------------------------- #
# Compare baseline characteristics HFmrEF
# ----------------------------------------------------------- #
HC <- clustMr$HC
KM <- clustMr$KM
EM <- clustMr$EM

compare.baseline(cbind(HFmrEFimp, HC), "HC")
compare.baseline(cbind(HFmrEFimp, KM), "KM")
compare.baseline(cbind(HFmrEFimp, EM), "EM")

# ----------------------------------------------------------- #
# Assumin clustering by physicians is incorrect
# ----------------------------------------------------------- #


