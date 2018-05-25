# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("tikzDevice")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load package for docstring
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)
source("_helper_func.R")

# ----------------------------------------------------------- #
# Load imputed data
# ----------------------------------------------------------- #
allDataFiles <- c("HFpEF", "HFmrEF")
lapply(gsub(" ", "", paste("data_files/", allDataFiles, 
                           ".Rdat")), load,.GlobalEnv)

# ----------------------------------------------------------- #
# Principal component analysis
# ----------------------------------------------------------- #
HFpEFpca <- princomp(as.matrix(HFpEF), cor = T)
HFmrEFpca <- princomp(as.matrix(HFmrEF), cor = T)

# ----------------------------------------------------------- #
# Explained variance plot
# ----------------------------------------------------------- #
tikz(file="../../../doc/thesis/images/pca_var_plot_HFpEF.tex",
    width = 10, height = 9)
pca.var.plot(HFpEFpca, 58, title = "HFpEF")
dev.off()
tikz(file="../../../doc/thesis/images/pca_var_plot_HFmrEF.tex",
    width = 10, height = 9)
pca.var.plot(HFmrEFpca, 52, title = "HFmrEF")
dev.off()

# ----------------------------------------------------------- #
pca.cluster.plot(HFmrEFpca, ncp = 52)
pca.cluster.plot(HFpEFpca, ncp = 58, km.clust = 3)

NbClust(as.data.frame(HFmrEFpca$scores[, 1:52]), 
        distance = "euclidean", min.nc = 2,
        max.nc = 10, method = "kmeans")

NbClust(as.data.frame(HFpEFpca$scores[, 1:58]), 
        distance = "euclidean", min.nc = 2,
        max.nc = 10, method = "kmeans")

# ----------------------------------------------------------- #