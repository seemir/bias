# Load relevant packages
library(R.matlab)
library(data.table)

# Read matlab file into R
data_set_HFpEF <- readMat('data_use_HFpEF.mat')
data_set_HFmrEF <- readMat('data_use_HFmrEF.mat')

# Convert data into a R matrix and add header names
HFpEF_matrix <- data_set_HFpEF$All.data
HFmrEF_matrix <- data_set_HFmrEF$All.data

# Get patient ids
patient_id_HFpEF <- data_set_HFpEF$patientID
patient_id_HFmrEF <- data_set_HFmrEF$patientID

# Add patient ids to HF_matrices and all column names
HFpEF_matrix <- cbind(patient_id_HFpEF, HFpEF_matrix)
HFmrEF_matrix <- cbind(patient_id_HFmrEF, HFmrEF_matrix)
colnames(HFpEF_matrix) <- c('patientId', 
                            as.vector(unlist(
                              data_set_HFpEF$Varnames)))
colnames(HFmrEF_matrix) <- c('patientId', 
                             as.vector(unlist(
                               data_set_HFmrEF$Varnames)))

# Remove patient_id not to clog the analysis
rm(patient_id_HFpEF, patient_id_HFmrEF)

# Consolidate naming convensions for some variables
# In the HFmrEF matrix
setnames(as.data.frame(HFmrEF_matrix), 
         old=c("Admissionweight","BMI","Numberofcomorbidities",
               "Afrocaribbean","Caucasian","Pulse","NtproBNP","E",
               "ECGRhythm_other", "LVHand_orLAE", "ECGQRS_other"), 
         new = c("admissionwgt","BmIadmission","comorbidities",
                 "Black","White","pulse","NTproBNP", "Ewave",
                 "ECGRhythmother", "LVHandorLAE", "ECGQRSother"))

# In the HFpEF matrix
setnames(as.data.frame(HFpEF_matrix), 
         old = c("E_e","LVfunction", "ECGRhythm_other", "ECGQRS_other",
                 "Other_ethnicity"), 
         new = c("Ee", "LVEF", "ECGRhythmother", "ECGQRSother", 
                 "Otherethnicity"))

# Save the data frame as .Rdat
save(HFpEF_matrix, file='data_use_HFpEF_matrix.Rdat')
save(HFmrEF_matrix, file='data_use_HFmrEF_matrix.Rdat')