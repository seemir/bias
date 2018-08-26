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
clustFull <- pca.cluster.plot(HFfullpca, 31, km.clust = 2, 
                              hc.clust = 2, em.clust = 2, 
                              actual = SyndClass[,2], 
                              return.clust = T, ellipse = F)

# ----------------------------------------------------------- #
# Extract cluster configuration
# ----------------------------------------------------------- #


# ----------------------------------------------------------- #
# Compare baseline characteristics
# ----------------------------------------------------------- #
compare.baseline(HFfullImp, clustFull$ACT)
compare.baseline(HFfullImp, clustFull$HC)
compare.baseline(HFfullImp, clustFull$KMC)
compare.baseline(HFfullImp, clustFull$EMC)

# ----------------------------------------------------------- #
# Assuming clustering by physicians is correct
# ----------------------------------------------------------- #
clustPef <- pca.cluster.plot(HFpEFpca, 31, km.clust = 2, 
                             hc.clust = 2, em.clust = 2, 
                             return.clust = T, ellipse = F)

clustPef <- pca.cluster.plot(HFmrEFpca, 31, km.clust = 2, 
                             hc.clust = 2, em.clust = 2, 
                             return.clust = T, ellipse = F)

# ----------------------------------------------------------- #