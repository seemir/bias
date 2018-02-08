# Load module R.matlab inorder to read matlab datafile into R
library(R.matlab)

# Read matlab file into R
data_use_HFmrEF <- readMat('data_use_HFmrEF.mat')

# Convert data into a R matrix and add header names
HEmrEF_matrix <- data_use_HFmrEF$All.data

# Get patient_id 
patient_id <- data_use_HFmrEF$patientID

# Add patient_id to HEmrEF_matrix and all column names
HEmrEF_matrix <- cbind(patient_id, HEmrEF_matrix)
colnames(HEmrEF_matrix) <- c('patient_id', as.vector(unlist(data_use_HFmrEF$Varnames)))

# Remove patient_id not to clog the analysis
rm(patient_id)

# Save the data frame as .Rdat
save(HEmrEF_matrix, file='data_use_HFmrEF_matrix.Rdat')