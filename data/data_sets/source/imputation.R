# ----------------------------------------------------------- #
# Install relevant packages (if not already done)
# ----------------------------------------------------------- #
bio_conductor <- "https://bioconductor.org/biocLite.R"
source(bio_conductor); biocLite("impute")
# ----------------------------------------------------------- #
# Load indicator and non indicator variables
# ----------------------------------------------------------- #
all_data_files <- c("HFpEF_matrix_ind_var", 
                    "HFmrEF_matrix_ind_var",
                    "HFpEF_matrix_not_ind",
                    "HFmrEF_matrix_not_ind")
lapply(gsub(" ", "", paste("data_files/", all_data_files, 
                           ".Rdat")), load,.GlobalEnv)

# ----------------------------------------------------------- #
# Inpute missing indicator variables and non-indicator variables (k = 10)
# ----------------------------------------------------------- #
K = 10
HFpEF_matrix_ind_var <- round(impute.knn(HFpEF_matrix_ind_var, k = K, colmax = 1)$data)
HFpEF_matrix_no_ind <- impute.knn(HFpEF_matrix_no_ind, k = K, colmax = 1)$data

# Merge the imputed indicator variables with non-indicator variables
HFpEF_matrix <- cbind(HFpEF_matrix_no_ind, HFpEF_matrix_ind_var)

# Save the data as .Rdat
filename <- paste(c(deparse(substitute(HFpEF_matrix)), '_k_', 
                    as.character(K), '.Rdat'), collapse = "")

save(HFpEF_matrix, file=filename) 
rm(HFpEF_matrix_ind_var, HFpEF_matrix_no_ind, not_zeros, filename, K, cap_desc_HFpEF,
   lab_desc_HFpEF)