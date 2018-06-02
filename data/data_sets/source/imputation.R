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
source("_helper_func.R")

# ----------------------------------------------------------- #
# Load indicator and non indicator variables
# ----------------------------------------------------------- #
allDataFiles <- c("HFpEF_ind_var", "HFmrEF_ind_var",
                  "HFpEF_not_ind", "HFmrEF_not_ind")
lapply(gsub(" ", "", paste("data_files/", allDataFiles, 
                           ".Rdat")), load,.GlobalEnv)

# ----------------------------------------------------------- #
# Little's test to assess for missing completely at random.
# Remove variables with more than 60% missing values and 
# that have near zero variance (not for indicator variables).
# ----------------------------------------------------------- #
# In HFpEF 
# ----------------------------------------------------------- #
CutOff <- 0.52 # cut.off percentage
HFpEFind <- rm.missing(HFpEFmatInd, cut.off = CutOff, 
                       near.zero.var = F)
HFpEFcon <- rm.missing(HFpEFmatNoInd, cut.off = CutOff)
FirstHFpEFcon <- split.matrix(HFpEFcon)$first.half
SecondHFpEFcon <- split.matrix(HFpEFcon)$second.half
HFpEFlist <- list(HFpEFind, FirstHFpEFcon, SecondHFpEFcon)
HFpEFmcar <- do.call(rbind, lapply(HFpEFlist, little.mcar))
HFpEFmcarNames <- c("indicator","continuous_1", "continuous_2")
rownames(HFpEFmcar) <- HFpEFmcarNames

# ----------------------------------------------------------- #
# In HFmrEF 
# ----------------------------------------------------------- #
HFmrEFind <- rm.missing(HFmrEFmatInd, cut.off = CutOff,
                        near.zero.var = F)
HFmrEFcon <- rm.missing(HFmrEFmatNoInd, cut.off = CutOff)
FirstHFmrEFcon <- split.matrix(HFmrEFcon)$first.half
SecondHFmrEFcon <- split.matrix(HFmrEFcon)$second.half
HFmrEFlist <- list(HFmrEFind, FirstHFmrEFcon, SecondHFmrEFcon)
HFmrEFmcar <- do.call(rbind, lapply(HFmrEFlist, little.mcar))
HFmrEFmcarNames <- c("indicator","continuous_1","continuous_2")
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
m <- 50 # number of bootstrap samples
HFpEFconImpEmList <- HFmrEFconImpEmList <- list()
FirstHFpEFBound   <- data.bounds(FirstHFpEFcon, 0, Inf)
SecondHFpEFBound  <- data.bounds(SecondHFpEFcon, 0, Inf)
FirstHFmrEFBound  <- data.bounds(FirstHFmrEFcon, 0, Inf)
SecondHFmrEFBound <- data.bounds(SecondHFmrEFcon, 0, Inf)
HFpEFfirstEM <- boot.em.impute(FirstHFpEFcon, 
                bounds = FirstHFpEFBound, n.boot = m)
HFpEFsecondEM <- boot.em.impute(SecondHFpEFcon, 
                 bounds = SecondHFpEFBound, n.boot = m)
HFmrEFfirstEM <- boot.em.impute(FirstHFmrEFcon, 
                 bounds = FirstHFmrEFBound, n.boot = m)
HFmrEFsecondEM <- boot.em.impute(SecondHFmrEFcon, 
                  bounds = SecondHFmrEFBound, n.boot = m)
HFpEFconImpEm <- cbind(HFpEFfirstEM, HFpEFsecondEM)
HFmrEFconImpEm <- cbind(HFmrEFfirstEM, HFmrEFsecondEM)

# ----------------------------------------------------------- #
# Impute the indicator variables with classification and 
# regression trees algorithm 
# ----------------------------------------------------------- #
HFpEFindImpCart <- complete(mice(HFpEFind, method ="cart"))
HFmrEFindImpCart <- complete(mice(HFmrEFind,method ="cart"))

# ----------------------------------------------------------- #
# Merge imputed data into one data file
# ----------------------------------------------------------- #
HFpEF <- cbind(HFpEFconImpEm, HFpEFindImpCart)
HFmrEF <- cbind(HFmrEFconImpEm, HFmrEFindImpCart)

# ----------------------------------------------------------- #
# Save the data files
# ----------------------------------------------------------- #
save(HFpEF, file = "data_files/HFpEF.Rdat")
save(HFmrEF, file = "data_files/HFmrEF.Rdat")

# ----------------------------------------------------------- #
