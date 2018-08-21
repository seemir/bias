# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("tikzDevice")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load package for docstring
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)
source("utilities.R")

# ----------------------------------------------------------- #
# Load imputed data
# ----------------------------------------------------------- #
allDataFiles <- c("HFpEFimp", "HFmrEFimp")
lapply(gsub(" ", "", paste("data_files/", allDataFiles, 
                           ".Rdat")), load,.GlobalEnv)

# ----------------------------------------------------------- #
# Principal component analysis
# ----------------------------------------------------------- #
HFpEFpca <- princomp(as.matrix(HFpEFimp), cor = T)
HFmrEFpca <- princomp(as.matrix(HFmrEFimp), cor = T)

# ----------------------------------------------------------- #
# Explained variance plot
# ----------------------------------------------------------- #
ncp <- 5; ncmr <- 5
tikz(file="../../../doc/thesis/images/pca_var_plot_HFpEF.tex",
    width = 10, height = 9)
pca.var.plot(HFpEFpca, ncp, title = "HFpEF")
dev.off()
tikz(file="../../../doc/thesis/images/pca_var_plot_HFmrEF.tex",
    width = 10, height = 9)
pca.var.plot(HFmrEFpca, ncmr, title = "HFmrEF")
dev.off()

# ----------------------------------------------------------- #
# Optimal number of clusters
# ----------------------------------------------------------- #
# In HFpEF
# ----------------------------------------------------------- #
NbClust(as.data.frame(HFpEFpca$scores[, 1:ncp]), 
        distance = "euclidean", min.nc = 2,
        max.nc = 10, method = "kmeans")

# ----------------------------------------------------------- #
# In HFmrEF
# ----------------------------------------------------------- #
NbClust(as.data.frame(HFmrEFpca$scores[, 1:ncmr]), 
        distance = "euclidean", min.nc = 2,
        max.nc = 10, method = "kmeans")

# ----------------------------------------------------------- #
# Plot pca clusters
# ----------------------------------------------------------- #
pca.cluster.plot(HFpEFpca, ncp = 2, km.clust = 3,hc.clust=3, 
                 em.clust = 3)
pca.cluster.plot(HFmrEFpca, ncp = 2, km.clust=3, hc.clust=3,
                 em.clust = 3)

# ----------------------------------------------------------- #