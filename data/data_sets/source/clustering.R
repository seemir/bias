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
# Load pca objects and data files
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
pdf(file="../../../doc/thesis/images/clustFull.pdf")
clustFull <- pca.cluster.plot(HFfullpca, 31, km.clust = 2, 
                              hc.clust = 2, em.clust = 2, 
                              actual = SyndClass[,2], 
                              return.clust = T, ellipse = F)
dev.off()

# ----------------------------------------------------------- #
# Extract cluster configuration and add to data frame
# ----------------------------------------------------------- #
ACTfull <- clustFull$ACT
HCfull <- clustFull$HC
KMfull <- clustFull$KM
EMfull <- clustFull$EM

# ----------------------------------------------------------- #
# Compare baseline characteristics
# ----------------------------------------------------------- #
compare.baseline(cbind(HFfullImp, ACTfull), "ACTfull")
compare.baseline(cbind(HFfullImp, HCfull), "HCfull")
compare.baseline(cbind(HFfullImp, KMfull), "KMfull")
compare.baseline(cbind(HFfullImp, EMfull), "EMfull")

# ----------------------------------------------------------- #
# Assuming clustering by physicians is correct
# ----------------------------------------------------------- #
clustPefFull <- pca.cluster.plot(HFpEFpca, 2, km.clust = 3, 
                                 hc.clust = 3, em.clust = 3, 
                                 return.clust = T, ellipse = F)

clustMrFull <- pca.cluster.plot(HFmrEFpca, 2, km.clust = 3, 
                                hc.clust = 3, em.clust = 3, 
                                return.clust = T, ellipse = F)

# ----------------------------------------------------------- #
# Compare baseline characteristics HFpEF
# ----------------------------------------------------------- #
HCpEFphy <- clustPefFull$HC
KMpEFphy <- clustPefFull$KM
EMpEFphy <- clustPefFull$EM

compare.baseline(cbind(HFpEFimp, HCpEFphy), "HCpEFphy")
compare.baseline(cbind(HFpEFimp, KMpEFphy), "KMpEFphy")
compare.baseline(cbind(HFpEFimp, EMpEFphy), "EMpEFphy")

# ----------------------------------------------------------- #
# Compare baseline characteristics HFmrEF
# ----------------------------------------------------------- #
HCmrEFphy <- clustMrFull$HC
KMmrEFphy <- clustMrFull$KM
EMmrEFphy <- clustMrFull$EM

compare.baseline(cbind(HFmrEFimp, HCmrEFphy), "HCmrEFphy")
compare.baseline(cbind(HFmrEFimp, KMmrEFphy), "KMmrEFphy")
compare.baseline(cbind(HFmrEFimp, EMmrEFphy), "EMmrEFphy")

# ----------------------------------------------------------- #
# Assumin clustering by physicians is incorrect
# ----------------------------------------------------------- #
hiKmeansClust <- clustFull$HC
HFpEFhiKmeans <- HFfullImp[hiKmeansClust==1,] 
HFmrEFhiKmeans <- HFfullImp[hiKmeansClust==2,] 

# ----------------------------------------------------------- #
# Re-calculate principal components 
# ----------------------------------------------------------- #
HFpEFNewpca <- princomp(HFpEFhiKmeans, cor = T)
HFmrEFNewpca <- princomp(HFmrEFhiKmeans, cor = T)

# ----------------------------------------------------------- #
# Plot clusters 
# ----------------------------------------------------------- #
clustNewPef <- pca.cluster.plot(HFpEFNewpca, 2, km.clust = 3, 
                             hc.clust = 3, em.clust = 3, 
                             return.clust = T, ellipse = F)

clustNewMr <- pca.cluster.plot(HFmrEFNewpca, 2, km.clust = 3, 
                            hc.clust = 3, em.clust = 3, 
                            return.clust = T, ellipse = F)

# ----------------------------------------------------------- #
# Compare baseline characteristics HFpEF
# ----------------------------------------------------------- #
HCpEFnoPhy <- clustNewPef$HC
KMpEFnoPhy <- clustNewPef$KM
EMpEFnoPhy <- clustNewPef$EM

compare.baseline(cbind(HFpEFhiKmeans,HCpEFnoPhy),"HCpEFnoPhy")
compare.baseline(cbind(HFpEFhiKmeans,KMpEFnoPhy),"KMpEFnoPhy")
compare.baseline(cbind(HFpEFhiKmeans,EMpEFnoPhy),"EMpEFnoPhy")

# ----------------------------------------------------------- #
# Compare baseline characteristics HFmrEF
# ----------------------------------------------------------- #
HCmrEFnoPhy <- clustNewMr$HC
KMmrEFnoPhy <- clustNewMr$KM
EMmrEFnoPhy <- clustNewMr$EM

compare.baseline(cbind(HFmrEFhiKmeans,HCmrEFnoPhy),
                 "HCmrEFnoPhy")
compare.baseline(cbind(HFmrEFhiKmeans,KMmrEFnoPhy),
                 "KMmrEFnoPhy")
compare.baseline(cbind(HFmrEFhiKmeans,EMmrEFnoPhy),
                 "EMmrEFnoPhy")

# ----------------------------------------------------------- #