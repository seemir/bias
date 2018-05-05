# ----------------------------------------------------------- #
# Install relevant packages (if not already done)
# ----------------------------------------------------------- #
Packages <- c("BaylorEdPsych", "mvnmle", "xtable")
# install.packages(Packages)

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
HFpEF_lis <- list(HFpEF_ind, HFpEF_con[,1:16], 
                  HFpEF_con[,17:32], HFpEF_con[,32:48])
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
# Inpute missing values
# ----------------------------------------------------------- #
