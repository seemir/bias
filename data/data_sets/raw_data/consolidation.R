# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("R.matlab", "data.table","stringr")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load relevant packages
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)
source("../source/utilities.R")

# ----------------------------------------------------------- #
# Read matlab files into R
# ----------------------------------------------------------- #
dataSetHFpEF <- readMat('data_use_HFpEF.mat')
dataSetHFmrEF <- readMat('data_use_HFmrEF.mat')

# ----------------------------------------------------------- #
# Extract the data matrix from matlab files
# ----------------------------------------------------------- #
HFpEFmat <- dataSetHFpEF$All.data
HFmrEFmat <- dataSetHFmrEF$All.data

# ----------------------------------------------------------- #
# Add all column names
# ----------------------------------------------------------- #
colnames(HFpEFmat) <- c(as.vector(unlist(
                        dataSetHFpEF$Varnames)))
colnames(HFmrEFmat) <- c(as.vector(unlist(
                         dataSetHFmrEF$Varnames)))

# ----------------------------------------------------------- #
# Consolidate naming conventions for some variables
# ----------------------------------------------------------- #
# In the HFpEF matrix
# ----------------------------------------------------------- #
setnames(as.data.frame(HFpEFmat), 
         old = c("E_e","LVfunction", "ECGRhythm_other", 
                 "ECGQRS_other", "Other_ethnicity", "Plt",
                 "COPD"), 
         new = c("Ee", "LVEF", "ECGRhythmother", "ECGQRSother", 
                 "Otherethnicity", "Plts", "COPDasthma"))

# ----------------------------------------------------------- #
# In the HFmrEF matrix
# ----------------------------------------------------------- #
setnames(as.data.frame(HFmrEFmat), 
         old=c("Admissionweight","BMI","Numberofcomorbidities",
               "Afrocaribbean", "Caucasian","Pulse","NtproBNP",
               "E", "ECGRhythm_other", "LVHand_orLAE", 
               "ECGQRS_other", "iron", "Timetoadmission"), 
         new = c("admissionwgt","BmIadmission","comorbidities",
                 "Black","White","pulse","NTproBNP", "Ewave",
                 "ECGRhythmother", "LVHandorLAE", 
                 "ECGQRSother", "Ironlevels", "TimetoHFadm"))

# ----------------------------------------------------------- #
# Lowercase letters for all the colnames
# ----------------------------------------------------------- #
colnames(HFpEFmat) <- tolower(colnames(HFpEFmat))
colnames(HFmrEFmat) <- tolower(colnames(HFmrEFmat))

# ----------------------------------------------------------- #
# Rename dupblicate names in variables af and ar
# ----------------------------------------------------------- #
if(all(colnames(HFmrEFmat)[c(2,4)] == c("af", "ar"))){
  colnames(HFmrEFmat)[c(2,4)] <- c("afib", "ai")  
}
# ----------------------------------------------------------- #
if(all(colnames(HFpEFmat)[c(3,7)] == c("af", "ar"))){
  colnames(HFpEFmat)[c(3,7)] <- c("afib", "ai")
}

# ----------------------------------------------------------- #
# Address error in HFmrEF - lvef data point nr. 1
# ----------------------------------------------------------- #
HFmrEFmat[1, "lvef"] <- 40.45

# ----------------------------------------------------------- #
# Replace NaN values with NA using the make_na function
# ----------------------------------------------------------- #
HFpEFmat <- make.na(HFpEFmat)
HFmrEFmat <- make.na(HFmrEFmat)

# ----------------------------------------------------------- #
# Create one file with all the common variables in both 
# HFpEF and HFmrEF data sets.
# ----------------------------------------------------------- #
# Find common columns in both data sets
# ----------------------------------------------------------- #
HFpEFcol <- colnames(HFpEFmat) %in% colnames(HFmrEFmat)
HFmrEFcol <- colnames(HFmrEFmat) %in% colnames(HFpEFmat)

# ----------------------------------------------------------- #
# Test that all columns are equal
# ----------------------------------------------------------- #
all(sort(colnames(HFpEFmat)[HFpEFcol]) == 
      sort(colnames(HFmrEFmat)[HFmrEFcol]))

# ----------------------------------------------------------- #
# Get and sort the column names
# ----------------------------------------------------------- #
HFpEFcol <- sort(colnames(HFpEFmat)[HFpEFcol])
HFmrEFcol <- sort(colnames(HFmrEFmat)[HFmrEFcol])
HFpEFsame <- HFpEFmat[, HFpEFcol]
HFmrEFsame  <- HFmrEFmat[, HFmrEFcol]

# ----------------------------------------------------------- #
# Create syndrome class matrix
# ----------------------------------------------------------- #
syndrome <- rep(c(1, 2),
                times = c(nrow(HFpEFmat), nrow(HFmrEFmat)))
SyndName <- rep(c("HFpEF", "HFmrEF"),
                times = c(nrow(HFpEFmat), nrow(HFmrEFmat)))

# ----------------------------------------------------------- #
# Add patient id, create full data set and syndrome classes
# ----------------------------------------------------------- #
HFfullDataSet <- rbind(HFpEFsame, HFmrEFsame)
id <- seq(1, nrow(HFfullDataSet))
HFfullDataSet <- as.data.frame(cbind(id, HFfullDataSet))
SyndClass <- as.data.frame(cbind(id, syndrome, SyndName))

# ----------------------------------------------------------- #
# Store indicator and non-indicator variables using the 
# rm_indicator function
# ----------------------------------------------------------- #
HFfullrmInd <- rm.indicator(HFfullDataSet, n.uniq = 8)

# ----------------------------------------------------------- #
# Store the non-indicator and in variables for later
# ----------------------------------------------------------- #
HFfullInd <- HFfullrmInd$indicator
HFfullNoInd <- HFfullrmInd$non.indicator

# ----------------------------------------------------------- #
# Convert zeros to missings, the following variables are not to 
# be converted.
# ----------------------------------------------------------- #
notZeros <- c("comorbidities", "timetohfadm")
HFfullNoInd <- zero.to.na(HFfullNoInd, notZeros)

# ----------------------------------------------------------- #
# Concatinate indicator and non-indicator variables to one 
# data set and sort column names.
# ----------------------------------------------------------- #
HFfullDataSet <- cbind(HFfullNoInd[, -1], HFfullInd)
HFfullDataSet <- HFfullDataSet[, sort(colnames(HFfullDataSet))]
HFfullDataSet <- cbind(id, HFfullDataSet)

# ----------------------------------------------------------- #
# Split data according to syndroms
# ----------------------------------------------------------- #
# Full data set
# ----------------------------------------------------------- #
HFpEFrow <- SyndClass[,3] == "HFpEF"
HFmrEFrow <- SyndClass[,3] == "HFmrEF"
# ----------------------------------------------------------- #
HFpEFdataSet <- HFfullDataSet[HFpEFrow,]
HFmrEFdataSet <- HFfullDataSet[HFmrEFrow,]

# ----------------------------------------------------------- #
# Non-indicator variables
# ----------------------------------------------------------- #
HFpEFnoInd <- HFfullNoInd[HFpEFrow, ]
HFmrEFnoInd <- HFfullNoInd[HFmrEFrow, ]

# ----------------------------------------------------------- #
# Indicator variables
# ----------------------------------------------------------- #
HFpEFind <- HFfullInd[HFpEFrow, ]
HFmrEFind <- HFfullInd[HFmrEFrow, ]

# ----------------------------------------------------------- #
# Re-code patient group labels
# ----------------------------------------------------------- #
# Get patient groups
# ----------------------------------------------------------- #
patientGroupsHFpEF <- as.matrix(unlist(
                                dataSetHFpEF$Patient.group))
patientGroupsHFmrEF <- as.matrix(unlist(
                                 dataSetHFmrEF$Patient.group))

# ----------------------------------------------------------- #
# Labels of clinical outcomes
# ----------------------------------------------------------- #
deceased <- c("IN", "Z", "Y", "X")
reAdmission <- c("V", "U")

# ----------------------------------------------------------- #
# Split labels 
# ----------------------------------------------------------- #
HFpEFsplit <- str_split_fixed(patientGroupsHFpEF,", ", n = 2)
HFmrEFsplit <- str_split_fixed(patientGroupsHFmrEF,", ",n = 2)

# ----------------------------------------------------------- #
# Re-coding mortality labels
# ----------------------------------------------------------- #
isDeceasedHFpEF <- HFpEFsplit[,1] %in% deceased
isDeceasedHFmrEF <- HFmrEFsplit[,1] %in% deceased
deceasedHFpEF <- ifelse(isDeceasedHFpEF, "yes", "no")
deceasedHFmrEF <- ifelse(isDeceasedHFmrEF, "yes", "no")

# ----------------------------------------------------------- #
# Re-coding re-admission labels
# ----------------------------------------------------------- #
isReAdmittedHFpEF <- HFpEFsplit[,1] %in% reAdmission | 
                     HFpEFsplit[,2] %in% reAdmission
isReAdmittedHFmrEF <- HFmrEFsplit[,1] %in% reAdmission | 
                      HFmrEFsplit[,2] %in% reAdmission
reAdmissionHFpEF <- ifelse(isReAdmittedHFpEF,"yes","no")
reAdmissionHFmrEF <- ifelse(isReAdmittedHFmrEF,"yes","no")

# ----------------------------------------------------------- #
# Add outcomes to matrix
# ----------------------------------------------------------- #
HFpEFoutcomes <- cbind(id[HFpEFrow], patientGroupsHFpEF,
                       deceasedHFpEF, reAdmissionHFpEF)
HFmrEFoutcomes <- cbind(id[HFmrEFrow], patientGroupsHFmrEF,
                        deceasedHFmrEF, reAdmissionHFmrEF)

# ----------------------------------------------------------- #
# Add colnames to matrices
# ----------------------------------------------------------- #
colnames(HFpEFoutcomes) <- colnames(HFmrEFoutcomes) <- 
  c("id", "patientgroup", "deceased", "readmitted")

# ----------------------------------------------------------- #
# Create outcomes data frames
# ----------------------------------------------------------- #
HFfullOutcomes <- as.data.frame(rbind(HFpEFoutcomes, 
                                      HFmrEFoutcomes))
rownames(HFfullOutcomes) <- HFfullOutcomes[,1]
# ----------------------------------------------------------- #
HFpEFoutcomes <- HFfullOutcomes[HFpEFrow,]
HFmrEFoutcomes <- HFfullOutcomes[HFmrEFrow,]

# ----------------------------------------------------------- #
# Save all data frames (13 df in all)
# ----------------------------------------------------------- #
path <- "../source/data_files/"; r <- ".Rdat"
fileNames <- c("HFfullDataSet", "HFfullNoInd", "HFfullInd", 
               "HFpEFdataSet", "HFpEFnoInd", "HFpEFind", 
               "HFmrEFdataSet", "HFmrEFnoInd", "HFmrEFind",
               "HFfullOutcomes", "HFpEFoutcomes", 
               "HFmrEFoutcomes", "SyndClass")
for (name in fileNames){
  save(list = (name), file = paste(path, name, r, sep = ""))
}

# ----------------------------------------------------------- #