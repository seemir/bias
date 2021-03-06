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
NbClust(HFpEFpca$scores[,1:2], min.nc = 2, max.nc = 4,
        method = "kmeans")
NbClust(HFmrEFpca$scores[,1:2], min.nc = 2, max.nc = 4, 
        method = "kmeans")

# ----------------------------------------------------------- #
# PCA cluster plot for all data sets
# ----------------------------------------------------------- #
path_to_images <- "../../../doc/thesis/images/"
pdf(file = paste(path_to_images, "ClustFull.pdf"), width = 8, 
    height = 8)
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
act_full <- compare.baseline(cbind(HFfullImp, ACTfull),
                             "ACTfull")
act_hc <- compare.baseline(cbind(HFfullImp, HCfull), 
                           "HCfull")
act_km <- compare.baseline(cbind(HFfullImp, KMfull), 
                           "KMfull")
act_em <- compare.baseline(cbind(HFfullImp, EMfull), 
                           "EMfull")

xtable(act_full[[2]][1:15,])
xtable(act_hc[[2]][1:15,])
xtable(act_km[[2]][1:15,])
xtable(act_em[[2]][1:15,])

# ----------------------------------------------------------- #
# Assuming clustering by physicians is correct
# ----------------------------------------------------------- #
pdf(file = paste(path_to_images, "ClustpPhy.pdf"), width = 9,
    height = 8)
clustPefFull <- pca.cluster.plot(HFpEFpca, 2, km.clust = 3, 
                                 hc.clust = 3, em.clust = 3, 
                                 return.clust = T, ellipse = F)
dev.off()

pdf(file = paste(path_to_images, "ClustmrPhy.pdf"), width = 9,
    height = 8)
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

post_HC_p <- compare.baseline(cbind(HFpEFimp, HCpEFphy), 
                              "HCpEFphy")
post_KM_p <- compare.baseline(cbind(HFpEFimp, KMpEFphy), 
                              "KMpEFphy")
post_EM_p <- compare.baseline(cbind(HFpEFimp, EMpEFphy), 
                              "EMpEFphy")

xtable(post_HC_p[[2]][1:15,-1])
xtable(post_KM_p[[2]][1:15,-1])
xtable(post_EM_p[[2]][1:15,-1])

# ----------------------------------------------------------- #
# Compare baseline characteristics HFmrEF
# ----------------------------------------------------------- #
HCmrEFphy <- clustMrFull$HC
KMmrEFphy <- clustMrFull$KM
EMmrEFphy <- clustMrFull$EM

post_HC_mr <- compare.baseline(cbind(HFmrEFimp, HCmrEFphy), 
                               "HCmrEFphy")
post_KM_mr <- compare.baseline(cbind(HFmrEFimp, KMmrEFphy), 
                               "KMmrEFphy")
post_EM_mr <- compare.baseline(cbind(HFmrEFimp, EMmrEFphy), 
                               "EMmrEFphy")

xtable(post_HC_mr[[2]][1:15,-1])
xtable(post_KM_mr[[2]][1:15,-1])
xtable(post_EM_mr[[2]][1:15,-1])

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
pdf(file = paste(path_to_images, "ClustpNoPhy.pdf"), width = 9,
    height = 8)
clustNewPef <- pca.cluster.plot(HFpEFNewpca, 2, km.clust = 3, 
                             hc.clust = 3, em.clust = 3, 
                             return.clust = T, ellipse = F)
dev.off()

pdf(file = paste(path_to_images, "ClustmrNoPhy.pdf"), width=9,
    height = 8)
clustNewMr <- pca.cluster.plot(HFmrEFNewpca, 2, km.clust = 3, 
                            hc.clust = 3, em.clust = 3, 
                            return.clust = T, ellipse = F)
dev.off()

# ----------------------------------------------------------- #
# Compare baseline characteristics HFpEF
# ----------------------------------------------------------- #
HCpEFnoPhy <- clustNewPef$HC
KMpEFnoPhy <- clustNewPef$KM
EMpEFnoPhy <- clustNewPef$EM

noPost_HC_p <-compare.baseline(cbind(HFpEFhiKmeans,
                                     HCpEFnoPhy),"HCpEFnoPhy")
noPost_KM_p <-compare.baseline(cbind(HFpEFhiKmeans,
                                     KMpEFnoPhy),"KMpEFnoPhy")
noPost_EM_p <-compare.baseline(cbind(HFpEFhiKmeans,
                                     EMpEFnoPhy),"EMpEFnoPhy")

xtable(noPost_HC_p[[2]][1:15,-1])
xtable(noPost_KM_p[[2]][1:15,-1])
xtable(noPost_EM_p[[2]][1:15,-1])

# ----------------------------------------------------------- #
# Compare baseline characteristics HFmrEF
# ----------------------------------------------------------- #
HCmrEFnoPhy <- clustNewMr$HC
KMmrEFnoPhy <- clustNewMr$KM
EMmrEFnoPhy <- clustNewMr$EM

noPost_HC_mr<- compare.baseline(cbind(HFmrEFhiKmeans,
                                      HCmrEFnoPhy), 
                                "HCmrEFnoPhy")
noPost_KM_mr<- compare.baseline(cbind(HFmrEFhiKmeans,
                                      KMmrEFnoPhy), 
                                "KMmrEFnoPhy")
noPost_EM_mr<- compare.baseline(cbind(HFmrEFhiKmeans,
                                      EMmrEFnoPhy), 
                                "EMmrEFnoPhy")

xtable(noPost_HC_mr[[2]][1:15,-1])
xtable(noPost_KM_mr[[2]][1:15,-1])
xtable(noPost_EM_mr[[2]][1:15,-1])

# ----------------------------------------------------------- #
# Result of all the significant baseline characteristics
# ----------------------------------------------------------- #
results_post <- c(post_HC_p[[1]], post_KM_p[[1]], 
                  post_EM_p[[1]], post_HC_mr[[1]],
                  post_KM_mr[[1]], post_EM_mr[[1]])
results_no_post <- c(noPost_HC_p[[1]], noPost_KM_p[[1]],
                     noPost_EM_p[[1]], noPost_HC_mr[[1]],
                     noPost_KM_mr[[1]], noPost_EM_mr[[1]])

results <- cbind(matrix(results_post, 3), 
                 matrix(results_no_post, 3))

colnames(results) <- rep(c("HFpEF", "HFmrEF"), 2)
rownames(results) <- c("Hierarchical", "K-Means", "EM")

xtable(results)

# ----------------------------------------------------------- #