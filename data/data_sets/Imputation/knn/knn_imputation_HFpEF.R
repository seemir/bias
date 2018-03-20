#Load package for knn imputation
library(impute)
source("../_helper_func.R")

# Load HFpEF datafile
load("../../raw_data/data_use_HFpEF_matrix.Rdat") 

# Replace NaN values with NA using the make_na function
HEpEF_matrix <- make_na(HEpEF_matrix)

# Remove the indicator variables as first step of analysis
HEpEF_matrix_no_ind <- rm_indicator(HEpEF_matrix, n_uniq = 5)$non_indicator

# Store the indicator variables for later
HEpEF_matrix_ind_var <- rm_indicator(HEpEF_matrix, n_uniq = 5)$indicator

# Convert zeros to na, the following variables are not to be convert.
not_zeros <- c("comorbidities", "weightchange", "daysfollowupdischarge", "Timetonextadm")
HEpEF_matrix_no_ind <- zero_to_na(HEpEF_matrix_no_ind, not_zeros)

# Inpute indicator variables and non-indicator variables (k = 10)
K = 10
HEpEF_matrix_ind_var <- round(impute.knn(HEpEF_matrix_ind_var, k = K, colmax = 1)$data)
HEpEF_matrix_no_ind <- impute.knn(HEpEF_matrix_no_ind, k = K, colmax = 1)$data

# Merge the imputed indicator variables with non-indicator variables
HEpEF_matrix <- cbind(HEpEF_matrix_no_ind, HEpEF_matrix_ind_var)

# Save the data as .Rdat
filename <- paste(c(deparse(substitute(HEpEF_matrix)), '_k_', 
                    as.character(K), '.Rdat'), collapse = "")

save(HEpEF_matrix, file=filename) 
rm(HEpEF_matrix_ind_var, HEpEF_matrix_no_ind, not_zeros, filename)
