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
# Little's test to assess for missing completely at random
# Remove variables with more than 70% missing values
# ----------------------------------------------------------- #
# In HFpEF 
# ----------------------------------------------------------- #
HFpEFind <- HFpEFmatInd
HFpEFind <- HFpEFind[, !colnames(HFpEFind) %in% 
                       c("osa", "obesitybmi30")]
HFpEFcon <- HFpEFmatNoInd
HFpEFlis <- list(HFpEFind, HFpEFcon[,2:15], HFpEFcon[,16:33], 
                 HFpEFcon[,34:47])
HFpEFmcar <- do.call(rbind, lapply(HFpEFlis, little.mcar))
HFpEFmcarNames <- c("indicator", "continuous_1", 
                    "continuous_2", "continuous_3")
rownames(HFpEFmcar) <- HFpEFmcarNames

# ----------------------------------------------------------- #
# In HFmrEF 
# ----------------------------------------------------------- #
HFmrEFind <- HFmrEFmatInd
HFmrEFind <- HFmrEFind[, !colnames(HFmrEFind) %in% 
                         c("cva", "e9cms")]
HFmrEFcon <- HFmrEFmatNoInd
HFmrEFcon <- HFmrEFcon[, !colnames(HFmrEFcon) %in%
                         c("bmiadmission",
                           "dischargeweight",
                           "admissionwgt",
                           "procedures", "troponin", 
                           "timetofirstcardiachospitalisation",
                           "ferritin")]
HFmrEFlis <- list(HFmrEFind, HFmrEFcon[,2:15],
                  HFmrEFcon[,16:30])
HFmrEFmcar <- do.call(rbind, lapply(HFmrEFlis, little.mcar))
HFmrEFmcarNames <- c("indicator", "continuous_1", 
                     "continuous_2")
rownames(HFmrEFmcar) <- HFmrEFmcarNames
xtable(rbind(HFpEFmcar, HFmrEFmcar), digits = c(0,0,0,4,0,5))

# ----------------------------------------------------------- #
# Impute missing values
# ----------------------------------------------------------- #
# Impute the non-indicator variables with the Bootstrap
# EM algorithm 
# ----------------------------------------------------------- #
m <- 30 # number of bootstrap samples
HFpEFconImpEmlis <- list() 
HFmrEFconImpEmlis <- list()
for (i in 1:m){
  print(paste("Bootstrap: ", i, " ", i/m*100,"% ",sep =""))
  HFpEFconImpEmlis[[i]] <- list.cbind(lapply(lapply(lapply(
    HFpEFlis[2:4], amelia, m = 1, boot.type="none", p2s = 0)
    ,"[[", 1), "[[", 1))
  HFmrEFconImpEmlis[[i]] <- list.cbind(lapply(lapply(lapply(
    HFmrEFlis[2:3], amelia, m = 1, boot.type="none", p2s = 0),
    "[[", 1), "[[", 1))
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
