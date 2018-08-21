# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("Amelia", "mice", "NbClust")
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
source("utilities.R")

# ----------------------------------------------------------- #
# Summary of missing variables
# ----------------------------------------------------------- #
if(ncol(HFfullDataSet) == 55){
  HFfullDataSet <- HFfullDataSet[,-1]  
}

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
HFfullEm <- boot.em.impute(HFfullNoInd, bnd, n.boot = 30)
HFfullCart <- complete(mice(HFfullInd, method = "cart"))

# ----------------------------------------------------------- #
# Combine imputed data sets into one 
# ----------------------------------------------------------- #
HFfullImpDataSet <- cbind(HFfullEm, HFfullCart)

# ----------------------------------------------------------- #
# Sort columns and remove 328 outlier
# ----------------------------------------------------------- #
if(nrow(HFfullImpDataSet) == 375){
  HFfullImpDataSet<-sort.column.names(HFfullImpDataSet[-328,], 
                                      id.col = F)  
}

# ----------------------------------------------------------- #
# Save full data set
# ----------------------------------------------------------- #
save(HFfullImpDataSet, file="data_files/HFfullImpDataSet.Rdat")
load(file="data_files/HFfullImpDataSet.Rdat")

# ----------------------------------------------------------- #
# Principal component analysis
# ----------------------------------------------------------- #
HFfullpca <- princomp(HFfullImpDataSet, cor = T)

# ----------------------------------------------------------- #
# Explained variance 
# ----------------------------------------------------------- #
pca.var.plot(HFfullpca, 35, title = "HF same variables")

# ----------------------------------------------------------- #
# Actual clustering configuration
# ----------------------------------------------------------- #
act <- SyndClass[-328, 2]
clust <- pca.cluster.plot(HFfullpca, 35, hc.clust = 2, 
                          km.clust = 2, em.clust = 2, 
                          ellipse = T, actual = act, 
                          return.clust = T)

# ----------------------------------------------------------- #
# Compare cluster groups
# ----------------------------------------------------------- #
# Actual clustering
# ----------------------------------------------------------- #
acGroup <- clust$ACT
dataSet <- cbind(acGroup, if(ncol(HFfullEm==27)){
  HFfullEm[-328,-4]})
compare.baseline(dataSet, "acGroup")

# ----------------------------------------------------------- #
# Hierarcical clustering
# ----------------------------------------------------------- #
hCgroup <- clust$HC
dataSet <- cbind(hCgroup, if(ncol(HFfullEm==27)){
  HFfullEm[-328,-4]})
compare.baseline(dataSet, "hCgroup")

# ----------------------------------------------------------- #
# kmeans ckustering
# ----------------------------------------------------------- #
kMgroup <- clust$KM
dataSet <- cbind(kMgroup, if(ncol(HFfullEm==27)){
  HFfullEm[-328,-4]})
compare.baseline(dataSet, "kMgroup")

# ----------------------------------------------------------- #