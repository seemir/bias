# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("Amelia", "mice")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load package for docstring
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)

# ----------------------------------------------------------- #
# Load data set with same variables and source helper functions
# ----------------------------------------------------------- #
load("data_files/HFfullDataSet.Rdat")
load("data_files/SyndClass.Rdat")
source("_helper_func.R")

# ----------------------------------------------------------- #
# Summary of missing variables
# ----------------------------------------------------------- #
top.n.missing(HFfullDataSet, 10)

# ----------------------------------------------------------- #
# Split variables into indicator and categorical variables
# ----------------------------------------------------------- #
HFfullRmInd <- rm.indicator(HFfullDataSet, 8)
HFfullInd <- HFfullRmInd$indicator
HFfullNoInd <- HFfullRmInd$non.indicator

# ----------------------------------------------------------- #
# Impute data using Bootstrap EM and CART
# ----------------------------------------------------------- #
bnd <- data.bounds(HFfullNoInd, 0, Inf)
HFfullEm <- boot.em.impute(HFfullNoInd, bnd, n.boot = 10)
HFfullCart <- complete(mice(HFfullInd, method = "cart"))

# ----------------------------------------------------------- #
# Combine imputed data sets into one 
# ----------------------------------------------------------- #
HFfullImpDataSet <- cbind(HFfullEm, HFfullCart)

# ----------------------------------------------------------- #
# Sort columns
# ----------------------------------------------------------- #
HFfullImpDataSet <- sort.column.names(HFfullImpDataSet)

# ----------------------------------------------------------- #
# Principal component analysis
# ----------------------------------------------------------- #
HFfullpca <- princomp(HFfullImpDataSet, cor = T)

# ----------------------------------------------------------- #
# Explained variance 
# ----------------------------------------------------------- #
pca.var.plot(HFfullpca, 41, title = "HF same variables")

# ----------------------------------------------------------- #
# Cluster plot
# ----------------------------------------------------------- #
pca.cluster.plot(HFfullpca, 41, hc.clust = 2, ellipse = T)

# ----------------------------------------------------------- #
# Actual clustering configuration
# ----------------------------------------------------------- #
pca.cluster.plot(HFfullpca, 41, hc.clust = 2, 
                 actual = SyndClass[, 2])

# ----------------------------------------------------------- #