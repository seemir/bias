# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("BaylorEdPsych", "Amelia", "mice", "NbClust",
              "caret", "rlist", "xtable")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load package for docstring
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)

# ----------------------------------------------------------- #
# Load data set with same variables and source helper functions
# ----------------------------------------------------------- #
allDataFiles <- c("HFpEFind", "HFmrEFind",
                  "HFpEFnoInd", "HFmrEFnoInd",
                  "HFfullDataSet", "SyndClass")
lapply(gsub(" ", "", paste("data_files/", allDataFiles, 
                           ".Rdat")), load,.GlobalEnv)
source("utilities.R")

# ----------------------------------------------------------- #
# Summary of missing variables
# ----------------------------------------------------------- #
top.n.missing(HFfullDataSet, 10)
top.n.missing(cbind(HFmrEFnoInd, HFmrEFind), 10)
top.n.missing(cbind(HFpEFnoInd, HFpEFind), 10)

# ----------------------------------------------------------- #
# Split variables into indicator and categorical variables
# ----------------------------------------------------------- #
HFfullRmInd <- rm.indicator(HFfullDataSet, 8)
HFfullInd <- HFfullRmInd$indicator
HFfullNoInd <- HFfullRmInd$non.indicator

# ----------------------------------------------------------- #
# Little's test to assess for missing completely at random.
# Remove variables with more than a given cut.off missing 
# values and that have near zero variance (not for indicator 
# variables).
# ----------------------------------------------------------- #
# In Full data set
# ----------------------------------------------------------- #
CutOff <- 0.20 # cut.off percentage
HFfullInd <- rm.missing(HFfullInd, cut.off = CutOff,
                        near.zero.var = F)
HFfullNoInd <- rm.missing(HFfullNoInd, cut.off = CutOff)
HFfullList <- list(HFfullInd, HFfullNoInd)
HFfullMcar <- do.call(rbind, lapply(HFfullList, little.mcar))
HFfullCarNames <- c("indicator","continuous")
rownames(HFfullMcar) <- HFfullCarNames

# ----------------------------------------------------------- #
# In HFpEF 
# ----------------------------------------------------------- #
CutOff <- 0.15 # cut.off percentage
HFpEFind <- rm.missing(HFpEFind, cut.off = CutOff, 
                       near.zero.var = F)
HFpEFnoInd <- rm.missing(HFpEFnoInd, cut.off = CutOff)
HFpEFlist <- list(HFpEFind, HFpEFnoInd)
HFpEFmcar <- do.call(rbind, lapply(HFpEFlist, little.mcar))
HFpEFmcarNames <- c("indicator","continuous")
rownames(HFpEFmcar) <- HFpEFmcarNames

# ----------------------------------------------------------- #
# In HFmrEF 
# ----------------------------------------------------------- #
CutOff <- 0.25 # cut.off percentage
HFmrEFind <- rm.missing(HFmrEFind, cut.off = CutOff,
                        near.zero.var = F)
HFmrEFnoInd <- rm.missing(HFmrEFnoInd, cut.off = CutOff)
HFmrEFlist <- list(HFmrEFind, HFmrEFnoInd)
HFmrEFmcar <- do.call(rbind, lapply(HFmrEFlist, little.mcar))
HFmrEFmcarNames <- c("indicator","continuous")
rownames(HFmrEFmcar) <- HFmrEFmcarNames
xtable(rbind(HFfullMcar, HFpEFmcar, HFmrEFmcar), 
       digits = c(0,0,0,4,0,5))

# ----------------------------------------------------------- #
# Report missing data after removing variables
# ----------------------------------------------------------- #
top.n.missing(cbind(HFfullNoInd, HFfullInd), n = 10)
top.n.missing(cbind(HFpEFnoInd, HFpEFind), n = 10)
top.n.missing(cbind(HFmrEFnoInd, HFmrEFind), n = 10)

# ----------------------------------------------------------- #
# Impute data using Bootstrap EM and CART
# ----------------------------------------------------------- #
# In Full data set
# ----------------------------------------------------------- #
m <- 100 # number of bootstrap samples
bnd <- data.bounds(HFfullNoInd, 0, Inf)
HFfullEm <- boot.em.impute(HFfullNoInd, bnd, n.boot = m)
HFfullCart <- complete(mice(HFfullInd, method = "cart"))

# ----------------------------------------------------------- #
# In HFpEF
# ----------------------------------------------------------- #
HFpEFconImpEmList <- HFmrEFconImpEmList <- list()
HFpEFbound   <- data.bounds(HFpEFnoInd, 0, Inf)
HFpEFem <- boot.em.impute(HFpEFnoInd, bounds = HFpEFbound, 
                          n.boot = m)
HFpEFcart <- complete(mice(HFpEFind, method ="cart"))

# ----------------------------------------------------------- #
# In HFmrEF
# ----------------------------------------------------------- #
HFmrEFbound <- data.bounds(HFmrEFnoInd, 0, Inf)
HFmrEFem <- boot.em.impute(HFmrEFnoInd, 
                           bounds = HFmrEFbound, 
                           n.boot = m)
HFmrEFcart <- complete(mice(HFmrEFind, method ="cart"))

# ----------------------------------------------------------- #
# Combine imputed data sets into one 
# ----------------------------------------------------------- #
HFfullImp <- cbind(HFfullEm, HFfullCart)
HFpEFimp <- cbind(HFpEFem, HFpEFcart)
HFmrEFimp <- cbind(HFmrEFem, HFmrEFcart) 

# ----------------------------------------------------------- #
# Sort columns and remove 328 outlier
# ----------------------------------------------------------- #
HFfullImp <- sort.column.names(HFfullImp, id.col = T)  
HFpEFimp <- sort.column.names(HFpEFimp, id.col = T)
HFmrEFimp <- sort.column.names(HFmrEFimp, id.col = T)

# ----------------------------------------------------------- #
# Save full data set
# ----------------------------------------------------------- #
path <- "data_files/"; r <- ".Rdat"
fileNames <- c("HFfullImp", "HFpEFimp", "HFmrEFimp")

for (name in fileNames){
  save(list = (name), file = paste(path, name, r, sep = ""))
}

# ----------------------------------------------------------- #
# Principal component analysis
# ----------------------------------------------------------- #
HFfullpca <- princomp(HFfullImp, cor = T)
HFpEFpca <- princomp(HFpEFimp, cor = T)
HFmrEFpca <- princomp(HFmrEFimp, cor = T)

# ----------------------------------------------------------- #
# Explained variance 
# ----------------------------------------------------------- #
pca.var.plot(HFfullpca, 31, title = "HF same variables")
pca.var.plot(HFpEFpca, 34, title = "HFpEF")
pca.var.plot(HFmrEFpca, 31, title = "HFmrEF")

# ----------------------------------------------------------- #
# Save pca objects
# ----------------------------------------------------------- #
path <- "data_files/"; r <- ".Rdat"
objects <- c("HFfullpca", "HFpEFpca", "HFmrEFpca")

for (object in objects){
  save(list = (object), file = paste(path, object, r, sep = ""))
}

# ----------------------------------------------------------- #