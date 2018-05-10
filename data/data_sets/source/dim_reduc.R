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
HFpEFpca <- princomp(scale(as.matrix(HFpEF)), cor = T)
HFmrEFpca <- princomp(scale(as.matrix(HFmrEF)), cor = T)

# ----------------------------------------------------------- #
# Explained variance plot
# ----------------------------------------------------------- #
tikz(file="../../../doc/thesis/images/pca_var_plot_HFpEF.tex",
    width = 10, height = 9)
pca.var.plot(HFpEFpca, 59, title = "HFpEF")
dev.off()
tikz(file="../../../doc/thesis/images/pca_var_plot_HFmrEF.tex",
    width = 10, height = 9)
pca.var.plot(HFmrEFpca, 51,title = "HFmrEF")
dev.off()

# ----------------------------------------------------------- #
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(cluster)

df <- HFmrEF
n_comp <- 51
K <- 2
k <- 2

pca <- PCA(df, ncp = ifelse(is.na(n_comp), length(df), 
           n_comp), scale.unit = T, graph = F)
sca <- scale(df)

hclpca <- fviz_cluster(HCPC(pca, graph = F, 
                       nb.clust = ifelse(is.na(k), -1, k)), 
                       ellipse.type = "t", 
main = "Hierarchical Clustering on Principle Components")
op_n_k <- 

hkm <- fviz_cluster(kmeans(sca, centers = K), sca, 
                    ellipse.type = "t",
main = "kmeans (k = 2) Clustering")

grid.arrange(hclpca, hkm, nrow = 2)

