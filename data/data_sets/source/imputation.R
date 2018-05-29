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
cf <- 0.6 # cut.off percentage
HFpEFind <- rm.missing(HFpEFmatInd, cut.off = cf, 
                       near.zero.var = F)
HFpEFcon <- rm.missing(HFpEFmatNoInd, cut.off = cf)
HFpEFconFirstHalf <- split.matrix(HFpEFcon)$first.half
HFpEFconSecondHalf <- split.matrix(HFpEFcon)$second.half
HFpEFlis <- list(HFpEFind, HFpEFconFirstHalf, 
                 HFpEFconSecondHalf)
HFpEFmcar <- do.call(rbind, lapply(HFpEFlis, little.mcar))
HFpEFmcarNames <- c("indicator","continuous_1", "continuous_2")
rownames(HFpEFmcar) <- HFpEFmcarNames

# ----------------------------------------------------------- #
# In HFmrEF 
# ----------------------------------------------------------- #
HFmrEFind <- rm.missing(HFmrEFmatInd, cut.off = cf,
                        near.zero.var = F)
HFmrEFcon <- rm.missing(HFmrEFmatNoInd, cut.off = cf)
HFmrEFconFirstHalf <- split.matrix(HFmrEFcon)$first.half
HFmrEFconSecondHalf <- split.matrix(HFmrEFcon)$second.half
HFmrEFlis <- list(HFmrEFind, HFmrEFconFirstHalf, 
                  HFmrEFconSecondHalf)
HFmrEFmcar <- do.call(rbind, lapply(HFmrEFlis, little.mcar))
HFmrEFmcarNames <-c("indicator","continuous_1","continuous_2")
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
m <- 30 # number of bootstrap samples
HFpEFconImpEmlis <- list() 
HFmrEFconImpEmlis <- list()
HFpEFconFirstBound <- data.bounds(HFpEFconFirstHalf, 0, Inf)
HFpEFconSecondBound <- data.bounds(HFpEFconSecondHalf, 0, Inf)
HFmrEFconFirstBound <- data.bounds(HFmrEFconFirstHalf, 0, Inf)
HFmrEFconSecondBound<-data.bounds(HFmrEFconSecondHalf, 0, Inf)

for (i in 1:m){
  print(paste("Bootstrap: ", i, " (", i/m*100," %)",sep =""))
  HFpEFfirstEM <- amelia(HFpEFconFirstHalf, m = 1, p2s = 0, 
                bounds = HFpEFconFirstBound)$imputations$imp1
  HFpEFsecondEM <-amelia(HFpEFconSecondHalf, m = 1, p2s = 0,
                bounds = HFpEFconSecondBound)$imputations$imp1
  HFmrEFfirstEM <-amelia(HFmrEFconFirstHalf, m = 1, p2s = 0, 
                bounds = HFmrEFconFirstBound)$imputations$imp1
  HFmrEFsecondEM<-amelia(HFmrEFconSecondHalf, m = 1, p2s = 0,
                bounds = HFmrEFconSecondBound)$imputations$imp1  
  HFpEFconImpEmlis[[i]] <- cbind(HFpEFfirstEM, HFpEFsecondEM)  
  HFmrEFconImpEmlis[[i]]<-cbind(HFmrEFfirstEM, HFmrEFsecondEM)  
}
HFpEFconImpEm <- Reduce("+", HFpEFconImpEmlis) / m
HFmrEFconImpEm <- Reduce("+", HFmrEFconImpEmlis) / m

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
