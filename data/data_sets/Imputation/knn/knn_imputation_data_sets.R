# Install relevant packages
# source("https://bioconductor.org/biocLite.R")
# biocLite("impute")
# install.packages("reporttools")
# install.packages("VIM")

# Load relevant packages and source helper functions
library(Hmisc)
library(impute)
library(reporttools)
library(VIM)
library(tikzDevice)

source("../_helper_func.R")

# Load HFpEF and HFmrEF datafiles
load("../../raw_data/data_use_HFpEF_matrix.Rdat") 
load("../../raw_data/data_use_HFmrEF_matrix.Rdat") 

# Replace NaN values with NA using the make_na function
HFpEF_matrix <- make_na(HFpEF_matrix)
HFmrEF_matrix <- make_na(HFmrEF_matrix)

# Remove the indicator variables as first step of analysis
HFpEF_rm_ind <- rm_indicator(HFpEF_matrix, n_uniq = 5)
HFmrEF_rm_ind <- rm_indicator(HFmrEF_matrix, n_uniq = 5)

HFpEF_matrix_not_ind <- HFpEF_rm_ind$non_indicator
HFmrEF_matrix_not_ind <- HFmrEF_rm_ind$non_indicator

# Store the indicator variables for later
HFpEF_matrix_ind_var <- HFpEF_rm_ind$indicator
HFmrEF_matrix_ind_var <- HFmrEF_rm_ind$indicator

# Move some variables between matrices in HFmrEF dataset
# Change RVfunction from non-indicator to indicator variable
HFmrEF_RV <- move_columns(HFmrEF_matrix_not_ind, HFmrEF_matrix_ind_var, 
                          "rvfunction")
HFmrEF_matrix_ind_var<- HFmrEF_RV$to_mat
HFmrEF_matrix_not_ind <- HFmrEF_RV$from_mat

# Change BmIadmission from indicator variable to non_indicator variables
HFmrEF_BMI <- move_columns(HFmrEF_matrix_ind_var, HFmrEF_matrix_not_ind, 
                           "bmiadmission")
HFmrEF_matrix_ind_var<- HFmrEF_BMI$from_mat
HFmrEF_matrix_not_ind <- HFmrEF_BMI$to_mat

# Convert zeros to missings, the following variables are not to be converted.
not_zeros_HFpEF <- c("comorbidities", "weightchange", "daysfollowupdischarge", 
                     "timetonextadm")
not_zeros_HFmrEF <- c("numbercomorditiesnoida", "comorbidities", "timetoadmission",
                      "timetoecho", "timetofollowupfrombnp", 
                      "timetofollowupfromdischarge",
                      "timetofirstcardiachospitalisation")
HFpEF_matrix_not_ind <- zero_to_na(HFpEF_matrix_not_ind, not_zeros_HFpEF)
HFmrEF_matrix_not_ind <- zero_to_na(HFmrEF_matrix_not_ind, not_zeros_HFmrEF)

# Plot of missing valus distribution
aggr(cbind(HFpEF_matrix_not_ind, HFpEF_matrix_ind_var), plot = T, 
     sortVars = T, bars = F, combined = T, ylabs = "", cex.axis = 0.7)
aggr(cbind(HFmrEF_matrix_not_ind, HFmrEF_matrix_ind_var), plot = T, 
     sortVars = T, bars = F, combined = T, ylabs = "", cex.axis = 0.7)
  
# Summary of variables
cap_desc_HFpEF <- "Patient characteristics: HFpEF variables"
lab_desc_HFpEF <- "tab:desc_stat_HFpEF_variables"
tableContinuous(as.data.frame(HFpEF_matrix), 
                stats = c("n", "na", "min", "max", "mean", "median", "s", 
                          "q1", "q3"),
                cap = cap_desc_HFpEF, lab = lab_desc_HFpEF)

# Inpute missing indicator variables and non-indicator variables (k = 10)
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