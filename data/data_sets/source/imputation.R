# ----------------------------------------------------------- #
# Install relevant packages (if not already done)
# ----------------------------------------------------------- #
Packages <- c("BaylorEdPsych", "mvnmle", "xtable", "Amelia",
              "rlist", "mice", "caret")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load relevant packages
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)
source("utilities.R")

# ----------------------------------------------------------- #
# Load indicator and non indicator variables
# ----------------------------------------------------------- #
allDataFiles <- c("HFpEFind", "HFmrEFind",
                  "HFpEFnoInd", "HFmrEFnoInd")
lapply(gsub(" ", "", paste("data_files/", allDataFiles, 
                           ".Rdat")), load,.GlobalEnv)

# ----------------------------------------------------------- #
# Little's test to assess for missing completely at random.
# Remove variables with more than a given cut.off missing 
# values and that have near zero variance (not for indicator 
# variables).
# ----------------------------------------------------------- #
# In HFpEF 
# ----------------------------------------------------------- #
CutOff <- 0.15 # cut.off percentage
HFpEFind <- rm.missing(HFpEFind, cut.off = CutOff, 
                       near.zero.var = F)
HFpEFcon <- rm.missing(HFpEFnoInd, cut.off = CutOff)
HFpEFlist <- list(HFpEFind, HFpEFcon)
HFpEFmcar <- do.call(rbind, lapply(HFpEFlist, little.mcar))
HFpEFmcarNames <- c("indicator","continuous")
rownames(HFpEFmcar) <- HFpEFmcarNames

# ----------------------------------------------------------- #
# In HFmrEF 
# ----------------------------------------------------------- #
CutOff <- 0.25 # cut.off percentage
HFmrEFind <- rm.missing(HFmrEFind, cut.off = CutOff,
                        near.zero.var = F)
HFmrEFcon <- rm.missing(HFmrEFnoInd, cut.off = CutOff)
HFmrEFlist <- list(HFmrEFind, HFmrEFcon)
HFmrEFmcar <- do.call(rbind, lapply(HFmrEFlist, little.mcar))
HFmrEFmcarNames <- c("indicator","continuous")
rownames(HFmrEFmcar) <- HFmrEFmcarNames
xtable(rbind(HFpEFmcar, HFmrEFmcar), digits = c(0,0,0,4,0,5))

# ----------------------------------------------------------- #
# Report missing data after removing variables
# ----------------------------------------------------------- #
top.n.missing(cbind(HFpEFcon, HFpEFind), n = 10)
top.n.missing(cbind(HFmrEFcon, HFmrEFind), n = 10)

# ----------------------------------------------------------- #
# Impute missing values
# ----------------------------------------------------------- #
# Impute the non-indicator variables with the Bootstrap
# EM algorithm. 
# ----------------------------------------------------------- #
m <- 100 # number of bootstrap samples
HFpEFconImpEmList <- HFmrEFconImpEmList <- list()
HFpEFbound   <- data.bounds(HFpEFcon, 0, Inf)
HFmrEFbound   <- data.bounds(HFmrEFcon, 0, Inf)
HFpEFconImpEm <- boot.em.impute(HFpEFcon, bounds = HFpEFbound, 
                                n.boot = m)
HFmrEFconImpEm <- boot.em.impute(HFmrEFcon, 
                                 bounds = HFmrEFbound, 
                                 n.boot = m)

# ----------------------------------------------------------- #
# Impute the indicator variables with classification and 
# regression trees algorithm 
# ----------------------------------------------------------- #
HFpEFindImpCart <- complete(mice(HFpEFind, method ="cart"))
HFmrEFindImpCart <- complete(mice(HFmrEFind, method ="cart"))

# ----------------------------------------------------------- #
# Merge imputed data into one data file
# ----------------------------------------------------------- #
HFpEFimp <- cbind(HFpEFconImpEm, HFpEFindImpCart)
HFmrEFimp <- cbind(HFmrEFconImpEm, HFmrEFindImpCart)

# ----------------------------------------------------------- #
# Sort column names
# ----------------------------------------------------------- #
HFpEFimp <- sort.column.names(HFpEFimp, id.col = T)
HFmrEFimp <- sort.column.names(HFmrEFimp, id.col = T)

# ----------------------------------------------------------- #
# Save the data files
# ----------------------------------------------------------- #
save(HFpEFimp, file = "data_files/HFpEFimp.Rdat")
save(HFmrEFimp, file = "data_files/HFmrEFimp.Rdat")

# ----------------------------------------------------------- #