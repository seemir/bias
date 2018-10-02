# ----------------------------------------------------------- #
# Install relevant packages (if not already done)
# ----------------------------------------------------------- #
Packages <- c("NbClust", "xtable")
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
path_to_images <- "../../../doc/thesis/images/"
pdf(file = paste(path_to_images, "ClustFull.pdf"))
clustFull <- pca.cluster.plot(HFfullpca, 4, km.clust = 2, 
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
xtable(compare.baseline(cbind(HFfullImp, ACTfull),
                        "ACTfull")[[2]][1:14,])
xtable(compare.baseline(cbind(HFfullImp, HCfull), 
                        "HCfull")[[2]][1:14,])
xtable(compare.baseline(cbind(HFfullImp, KMfull), 
                        "KMfull")[[2]][1:14,])
xtable(compare.baseline(cbind(HFfullImp, EMfull), 
                        "EMfull")[[2]][1:14,])

# ----------------------------------------------------------- #
# Assuming clustering by physicians is correct
# ----------------------------------------------------------- #
pdf(file = paste(path_to_images, "ClustpBiPhy.pdf"))
clustPefFull <- pca.cluster.plot(HFpEFpca, 2, km.clust = 3, 
                                 hc.clust = 3, em.clust = 3, 
                                 return.clust = T, ellipse = F)
dev.off()

pdf(file = paste(path_to_images, "ClustmrBiPhy.pdf"))
clustMrFull <- pca.cluster.plot(HFmrEFpca, 2, km.clust = 3, 
                                hc.clust = 3, em.clust = 3, 
                                return.clust = T, ellipse = F)
dev.off()

# ----------------------------------------------------------- #
# Compare baseline characteristics HFpEF
# ----------------------------------------------------------- #
HCpEFphy <- clustPefFull$HC
KMpEFphy <- clustPefFull$KM
EMpEFphy <- clustPefFull$EM

xtable(compare.baseline(cbind(HFpEFimp, HCpEFphy), 
                        "HCpEFphy")[[2]][1:14,])
xtable(compare.baseline(cbind(HFpEFimp, KMpEFphy), 
                        "KMpEFphy")[[2]][1:14,])
xtable(compare.baseline(cbind(HFpEFimp, EMpEFphy), 
                        "EMpEFphy")[[2]][1:14,])

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