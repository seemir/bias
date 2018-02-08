# Load module R.matlab inorder to read matlab datafile into R
library(R.matlab)

# Read matlab file into R
data_use_HFpEF <- readMat('data_use_HFpEF.mat')

# Convert data into a R matrix and add header names
HEpEF_matrix <- data_use_HFpEF$All.data

# Get patient_id
patient_id <- data_use_HFpEF$patientID

# Add patient_id to HEpEF_matrix and all column names
HEpEF_matrix <- cbind(patient_id, HEpEF_matrix)
colnames(HEpEF_matrix) <- c('patient_id', as.vector(unlist(data_use_HFpEF$Varnames)))

# Remove patient_id not to clog the analysis
rm(patient_id)

# Save the data frame as .Rdat
save(HEpEF_matrix, file='data_use_HFpEF_matrix.Rdat')