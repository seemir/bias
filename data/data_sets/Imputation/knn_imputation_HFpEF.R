#Load package for knn imputation
library(impute)
source("_helper_func.R")

# Load HFpEF datafile
load("../raw_data/data_use_HFpEF_matrix.Rdat") 

# Replace NaN values with NA using the make_na function
HEpEF_matrix <- make_na(HEpEF_matrix)

# Impute using knn algorithm with k=10 neightbours
HEpEF_matrix_imputed <- impute.knn(HEpEF_matrix, colmax = 1)$data

# Test that all missing values are imputed
sum(pmv(HEpEF_matrix_imputed)) == 0

# Save the imputed data
save(HEpEF_matrix_imputed, file = "HEpEF_matrix_imputed_k_10.Rdat")
