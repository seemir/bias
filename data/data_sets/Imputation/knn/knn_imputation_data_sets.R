# ----------------------------------------------------------- #
# Install relevant packages (if not already done)
# ----------------------------------------------------------- #
bio_conductor <- "https://bioconductor.org/biocLite.R"
source(bio_conductor)
Packages <- c("reporttools", "VIM", "tikzDevice", "Hmisc",
              "impute")
# install.packages(Packages)
# ----------------------------------------------------------- #
# Load relevant packages and source helper functions
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = T)
source("../_helper_func.R")

# ----------------------------------------------------------- #
# Load HFpEF and HFmrEF datafiles
# ----------------------------------------------------------- #
load("../../raw_data/data_use_HFpEF_matrix.Rdat") 
load("../../raw_data/data_use_HFmrEF_matrix.Rdat") 

# ----------------------------------------------------------- #
# Rename dupblicate names in variables af and ar
# ----------------------------------------------------------- #
colnames(HFmrEF_matrix)[c(3,5)] <- c("a-fib", "ai")
colnames(HFpEF_matrix)[c(4,8)] <- c("a-fib", "ai")

# ----------------------------------------------------------- #
# Replace NaN values with NA using the make_na function
# ----------------------------------------------------------- #
HFpEF_matrix <- make_na(HFpEF_matrix)
HFmrEF_matrix <- make_na(HFmrEF_matrix)

# ----------------------------------------------------------- #
# Store indicator and non-indicator variables in seperate
# variables using the rm_indicator function
# ----------------------------------------------------------- #
HFpEF_rm_ind <- rm_indicator(HFpEF_matrix, n_uniq = 5)
HFmrEF_rm_ind <- rm_indicator(HFmrEF_matrix, n_uniq = 5)

# ----------------------------------------------------------- #
# Store the non-indicator variables for later
# ----------------------------------------------------------- #
HFpEF_matrix_not_ind <- HFpEF_rm_ind$non_indicator
HFmrEF_matrix_not_ind <- HFmrEF_rm_ind$non_indicator

# ----------------------------------------------------------- #
# Store the indicator variables for later
# ----------------------------------------------------------- #
HFpEF_matrix_ind_var <- HFpEF_rm_ind$indicator
HFmrEF_matrix_ind_var <- HFmrEF_rm_ind$indicator

# ----------------------------------------------------------- #
# Move some variables between matrices in HFmrEF dataset
# ----------------------------------------------------------- #
# Change RVfunction from non-indicator to indicator variable
# ----------------------------------------------------------- #
HFmrEF_RV <- move_columns(HFmrEF_matrix_not_ind, 
                          HFmrEF_matrix_ind_var, 
                          "rvfunction")
HFmrEF_matrix_ind_var<- HFmrEF_RV$to_mat
HFmrEF_matrix_not_ind <- HFmrEF_RV$from_mat

# ----------------------------------------------------------- #
# Change BmIadmission from indicator variable to non_indicator 
# variable
# ----------------------------------------------------------- #
HFmrEF_BMI <- move_columns(HFmrEF_matrix_ind_var, 
                           HFmrEF_matrix_not_ind, 
                           "bmiadmission")
HFmrEF_matrix_ind_var<- HFmrEF_BMI$from_mat
HFmrEF_matrix_not_ind <- HFmrEF_BMI$to_mat

# ----------------------------------------------------------- #
# Convert zeros to missings, the following variables are not to 
# be converted.
# ----------------------------------------------------------- #
not_zeros_HFpEF <- c("comorbidities", "weightchange", 
                     "daysfollowupdischarge", "timetonextadm")
not_zeros_HFmrEF <- c("numbercomorditiesnoida","comorbidities", 
                      "timetoadmission", "timetoecho", 
                      "timetofollowupfrombnp", 
                      "timetofollowupfromdischarge",
                      "timetofirstcardiachospitalisation")
HFpEF_matrix_not_ind <- zero_to_na(HFpEF_matrix_not_ind, 
                                   not_zeros_HFpEF)
HFmrEF_matrix_not_ind <- zero_to_na(HFmrEF_matrix_not_ind, 
                                    not_zeros_HFmrEF)

# ----------------------------------------------------------- #
# Plot of missing values distribution
# ----------------------------------------------------------- #
path_to_images <- "../../../../doc/thesis/images/"

pdf(file=paste(c(path_to_images,"HFpEF_miss_dist.pdf"), 
               collapse = ""))
aggr(cbind(HFpEF_matrix_not_ind, HFpEF_matrix_ind_var), 
     plot = T, sortVars = T, bars = F, combined = T, 
     ylabs = "", cex.axis = 0.7)
dev.off()

pdf(file = paste(c(path_to_images, "HFmrEF_miss_dist.pdf"),
                 collapse = ""))
aggr(cbind(HFmrEF_matrix_not_ind, HFmrEF_matrix_ind_var), 
     plot = T, sortVars = T, bars = F, combined = T, 
     ylabs = "", cex.axis = 0.7)
dev.off()

# ----------------------------------------------------------- #
# Summary of variables
# ----------------------------------------------------------- #
# Reorder data matrix by phenotype domains
# ----------------------------------------------------------- #
# In HFpEF matrix
# ----------------------------------------------------------- #
id_HFpEF <- c("patientid")
demo_HFpEF <- c("age", "gender", "white", "asian", "black", 
                "otherethnicity")
adm_symp_HFpEF <- c("breathless", "chestpain", "orthopnoea",
                    "peripheraloedema", "palpdizzyfalls", 
                    "pnd")
adm_sign_HFpEF <- c("sbp", "dbp", "map", "admissionwgt", 
                    "height", "bmiadmission", "weightchange",
                    "admissionsbnp", "pulse", "bp",
                    "asympthf", "devicetherapy")
risk_fact_HFpEF <- c("a-fib", "copdasthma", "irondef", "obesity", 
                     "obesitybmi30", "nyhaclass", "dm",
                     "copdasthma", "ihd", "osa")
comor_HFpEF <- c("comorbidities")
ecg_HFpEF <- c( "ecgblock", "ecgblockcomment", 
                "ecgqrsduration", "ecgqrsother", "ecgrate",
                "ecgrhythmother", "twi", "lvh", 
                "normalecgqrs", "lbbb", "rbbb", "lvhlev",
                "sr")
lab_test_HFpEF <- c("albumin", "hb", "hba1c", "wbc", "tsat",
                    "glucose", "plts", "pcv", "ferritin",
                    "k", "ironlevels", "chol", "ntprobnp",
                    "gfr", "mcv", "na")
echo_HFpEF <- c("lvef","ewave", "pasp", "tapse", "ea", "ee", 
                "laterals", "mr", "tr", "as", "awave", 
                "dilatedlv", "ladiameter", "ai", "laarea",
                "raarea", "rwma", "calculatede", "rvfunction",
                "edeceltime")
outcomes_HFpEF <- c("alive", "timefromprevadm", "timetohfadm",
                    "timetonextadm", "daysfollowupdischarge",
                    "hfhospitalisation", "daysfollowupbnp",
                    "los")
df_HFpEF_names <- c(id_HFpEF, demo_HFpEF, adm_symp_HFpEF,
                    adm_sign_HFpEF, risk_fact_HFpEF,
                    comor_HFpEF, ecg_HFpEF, lab_test_HFpEF,
                    echo_HFpEF, outcomes_HFpEF)
df_HFpEF <- as.data.frame(HFpEF_matrix[,df_HFpEF_names])
cap_desc_HFpEF <- "Patient characteristics: HFpEF variables"
lab_desc_HFpEF <- "tab:desc_stat_HFpEF_variables"
tableContinuous(df_HFpEF, 
                stats = c("n", "na", "min", "max", "mean", 
                          "median", "s", "q1", "q3"),
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