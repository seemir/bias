# ----------------------------------------------------------- #
# Install relevant packages (if not already done)
# ----------------------------------------------------------- #
Packages <- c("BaylorEdPsych", "mvnmle", "xtable", "Amelia",
              "rlist", "mice")
install.packages(Packages)

# ----------------------------------------------------------- #
# Load relevant packages
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)
source("_helper_func.R")

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
# Little's test to assess for missing completely at random
# ----------------------------------------------------------- #
# In HFpEF 
# ----------------------------------------------------------- #
HFpEF_ind <- HFpEF_matrix_ind_var
HFpEF_ind <- HFpEF_ind[, !colnames(HFpEF_ind) %in% 
                         c("obesitybmi30","osa")]
HFpEF_con <- HFpEF_matrix_not_ind
HFpEF_lis <- list(HFpEF_ind, HFpEF_con[,1:15], 
                  HFpEF_con[,16:33], HFpEF_con[,34:47])
HFpEF_mcar_res <- do.call(rbind, lapply(HFpEF_lis, 
                                        little_mcar))
HFpEF_mcar_names <- c("indicator", "continuous_1", 
                      "continuous_2", "continuous_3")
rownames(HFpEF_mcar_res) <- HFpEF_mcar_names

# ----------------------------------------------------------- #
# In HFmrEF 
# ----------------------------------------------------------- #
HFmrEF_ind <- HFmrEF_matrix_ind_var
HFmrEF_ind <- HFmrEF_ind[, !colnames(HFmrEF_ind) %in% 
                           c("cva", "e9cms")]
HFmrEF_con <- HFmrEF_matrix_not_ind
HFmrEF_con <- HFmrEF_con[, !colnames(HFmrEF_con) %in%
                     c("bmiadmission",
                       "dischargeweight",
                       "admissionwgt",
                       "procedures", "troponin", 
                       "timetofirstcardiachospitalisation",
                       "ferritin")]
HFmrEF_lis <- list(HFmrEF_ind, HFmrEF_con[,1:15], 
                   HFmrEF_con[,16:30])
HFmrEF_mcar_res <- do.call(rbind, lapply(HFmrEF_lis,
                                         little_mcar))
HFmrEF_mcar_names <- c("indicator", "continuous_1", 
                       "continuous_2")
rownames(HFmrEF_mcar_res) <- HFmrEF_mcar_names
xtable(rbind(HFpEF_mcar_res, HFmrEF_mcar_res), 
       digits = c(0,0,0,4,0,5))

# ----------------------------------------------------------- #
# Impute missing values
# ----------------------------------------------------------- #
# Impute the non-indicator variables with the EM algorithm 
# ----------------------------------------------------------- #
HFpEF_con_imp_em <- list.cbind(lapply(lapply(lapply(
  HFpEF_lis[2:4], amelia, m = 1, boot.type="none"),"[[", 1), 
  "[[", 1))
HFmrEF_con_imp_em <- list.cbind(lapply(lapply(lapply(
  HFmrEF_lis[2:3], amelia, m = 1, boot.type="none"),"[[", 1), 
  "[[", 1))
# ----------------------------------------------------------- #
# Impute the indicator variables with the classification and 
# regression trees algorithm 
# ----------------------------------------------------------- #
HFpEF_ind_imp_cart <- complete(mice(HFpEF_ind, method ="cart"))
HFmrEF_ind_imp_cart <-complete(mice(HFmrEF_ind,method ="cart"))

# ----------------------------------------------------------- #
# Merge imputed data into one data file
# ----------------------------------------------------------- #
HFpEF <- cbind(HFpEF_con_imp_em, HFpEF_ind_imp_cart)
HFmrEF <- cbind(HFmrEF_con_imp_em, HFmrEF_ind_imp_cart)

# ----------------------------------------------------------- #
# Save the data files
# ----------------------------------------------------------- #
save(HFpEF, file = "data_files/HFpEF")
save(HFmrEF, file = "data_files/HFmrEF")

# ----------------------------------------------------------- #