# ----------------------------------------------------------- #
# Install relevant packages (if not already done)
# ----------------------------------------------------------- #
Packages <- c("reporttools", "VIM", "tikzDevice", "Hmisc")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load relevant packages and source helper functions
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = T)
source("_helper_func.R")

# ----------------------------------------------------------- #
# Load HFpEF and HFmrEF datafiles
# ----------------------------------------------------------- #
load("../raw_data/data_use_HFpEF_matrix.Rdat") 
load("../raw_data/data_use_HFmrEF_matrix.Rdat") 

# ----------------------------------------------------------- #
# Rename dupblicate names in variables af and ar
# ----------------------------------------------------------- #
if(all(colnames(HFmrEF_matrix)[c(3,5)] == c("af", "ar"))){
  colnames(HFmrEF_matrix)[c(3,5)] <- c("a-fib", "ai")  
}
if(all(colnames(HFpEF_matrix)[c(4,8)] == c("af", "ar"))){
  colnames(HFpEF_matrix)[c(4,8)] <- c("a-fib", "ai")
}

# ----------------------------------------------------------- #
# Address error in HFmrEF - lvef data point nr. 1
# ----------------------------------------------------------- #
HFmrEF_matrix[1, "lvef"] <- 40.45

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
path_to_images <- "../../../doc/thesis/images/"

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
risk_fact_HFpEF <- c("a-fib", "copdasthma", "irondef", 
                     "obesity", "obesitybmi30", "nyhaclass", 
                     "dm","ihd", "osa")
comor_HFpEF <- c("comorbidities")
ecg_HFpEF <- c( "ecgblock","ecgblockcomment","ecgqrsduration", 
                "ecgqrsother","ecgrate","ecgrhythmother","twi", 
                "lvh","normalecgqrs", "lbbb", "rbbb", "lvhlev",
                "sr")
lab_test_HFpEF <- c("albumin", "hb", "hba1c", "wbc", "tsat",
                    "glucose", "plts", "pcv", "ferritin",
                    "k", "ironlevels", "chol", "ntprobnp",
                    "gfr", "mcv", "na")
echo_HFpEF <- c("lvef","ewave", "pasp", "tapse", "ea", "ee", 
                "laterals", "mr", "tr", "as", "awave", 
                "dilatedlv", "ladiameter", "ai", "laarea",
                "raarea", "rwma", "calculatede", "rvfunction",
                "edeceltime", "af")
outcomes_HFpEF <- c("alive", "timefromprevadm", "timetohfadm",
                    "timetonextadm", "daysfollowupdischarge",
                    "hfhospitalisation", "daysfollowupbnp",
                    "los")

# ----------------------------------------------------------- #
# In HFmrEF matrix
# ----------------------------------------------------------- #
id_HFmrEF <- c("patientid")
demo_HFmrEF <- c("age", "gender", "white", "asian", "black", 
                 "other", "mixed")
adm_symp_HFmrEF <- c("breathless", "st")
adm_sign_HFmrEF <- c("sbp", "dbp", "admissionwgt", "bp", 
                    "bmiadmission", "pulse", "sympthf",
                    "symptlvhf", "symptlvunder", 
                    "symptomatichfmref", "pathway")
risk_fact_HFmrEF <- c("a-fib", "copdasthma", "irondef", "dm", 
                      "obesity", "copdasthma", "ihd", 
                      "ar", "both", "cva", "sb", "procedures")
comor_HFmrEF <- c("comorbidities", "numbercomorditiesnoida")
ecg_HFmrEF <- c("ecgqrsduration", "ecgqrsother", "ecgrate",
                "ecgrhythmother", "lvh", "normalecgqrs","nsr", 
                "lbbb", "rbbb", "sr")
lab_test_HFmrEF <- c("hb", "wbc", "tsat", "plts", "pcv","crp", 
                     "ferritin", "k", "ironlevels", "chol", 
                     "ntprobnp", "gfr","mcv","na","troponin")
echo_HFmrEF <- c("lvef","ewave", "pasp", "ee", "mr", "tr", 
                 "as", "ai", "rvfunction", "checkedee", 
                 "timetoecho", "ee13diastolic", "af", 
                 "eagroup", "edecelgroup", "finalla",
                 "lvhgroup", "lvhandorlae", "e9cms")
outcomes_HFmrEF <- c("timetohfadm", "hfhospitalisation", 
                     "los", "dischargeweight", "cardiachosp",
                     "truehf", "timetofollowupfromdischarge",
                     "timetofirstcardiachospitalisation",
                     "timetofollowupfrombnp", "newhf")

# ----------------------------------------------------------- #
# Long Descriptive statistics
# ----------------------------------------------------------- #
df_HFpEF_names <- c(id_HFpEF, demo_HFpEF, adm_symp_HFpEF,
                    adm_sign_HFpEF, risk_fact_HFpEF,
                    comor_HFpEF, ecg_HFpEF, lab_test_HFpEF,
                    echo_HFpEF, outcomes_HFpEF)
df_HFmrEF_names <- c(id_HFmrEF, demo_HFmrEF, adm_symp_HFmrEF,
                    adm_sign_HFmrEF, risk_fact_HFmrEF,
                    comor_HFmrEF, ecg_HFmrEF, lab_test_HFmrEF,
                    echo_HFmrEF, outcomes_HFmrEF)

# ----------------------------------------------------------- #
df_HFpEF <- as.data.frame(HFpEF_matrix[,df_HFpEF_names])
cap_desc_HFpEF <- "Patient characteristics: HFpEF variables"
lab_desc_HFpEF <- "tab:desc_stat_HFpEF_variables"
tableContinuous(df_HFpEF, 
                stats = c("n", "na", "min", "max", "mean", 
                          "median", "s", "q1", "q3"),
                cap = cap_desc_HFpEF, lab = lab_desc_HFpEF)

# ----------------------------------------------------------- #
df_HFmrEF <- as.data.frame(HFmrEF_matrix[,df_HFmrEF_names])
cap_desc_HFmrEF <- "Patient characteristics: HFmrEF variables"
lab_desc_HFmrEF <- "tab:desc_stat_HFmrEF_variables"
tableContinuous(df_HFmrEF, 
                stats = c("n", "na", "min", "max", "mean", 
                          "median", "s", "q1", "q3"),
                cap = cap_desc_HFmrEF, lab = lab_desc_HFmrEF)

# ----------------------------------------------------------- #
# Outcomes table
# ----------------------------------------------------------- #
load("../raw_data/outcomes_HFpEF_matrix.Rdat") 
load("../raw_data/outcomes_HFmrEF_matrix.Rdat") 
r <- rep("", 5)
tab_out_HFpEF <- rbind(label_summary(HFpEF_outcomes_matrix, 2, 
                               c("Group", "Dead?", "Readm?", 
                                 "n", "% Tot"), 3, 5), r, r) 
tab_out_HFmrEF <- label_summary(HFmrEF_outcomes_matrix, 2,
                                c("Group", "Dead?", "Readm?", 
                                  "n", "% Tot"), 3, 5)
print(xtable(cbind(tab_out_HFpEF, tab_out_HFmrEF)), 
                   include.rownames = F)

# ----------------------------------------------------------- #
# Tables of top 10 missing values variables in both data sets
# ----------------------------------------------------------- #
# In HFpEF 
# ----------------------------------------------------------- #
HFpEF_miss <- top_n_missing(cbind(HFpEF_matrix_ind_var, 
                              HFpEF_matrix_not_ind), 10)

# ----------------------------------------------------------- #
# In HFmrEF 
# ----------------------------------------------------------- #
HFmrEF_miss <- top_n_missing(cbind(HFmrEF_matrix_ind_var,
                                   HFmrEF_matrix_not_ind),10)

# ----------------------------------------------------------- #
# Combine missing values table and convert to Latex code
# ----------------------------------------------------------- #
xtable(cbind(round(HFpEF_miss,3), rownames(HFmrEF_miss), 
       round(HFmrEF_miss,3)))

# ----------------------------------------------------------- #
# Save indicator variables and non indicator variables
# for later imputation
# ----------------------------------------------------------- #
save(HFpEF_matrix_ind_var, 
     file="data_files/HFpEF_matrix_ind_var.Rdat")
save(HFmrEF_matrix_ind_var, 
     file="data_files/HFmrEF_matrix_ind_var.Rdat")
save(HFpEF_matrix_not_ind, 
     file="data_files/HFpEF_matrix_not_ind.Rdat")
save(HFmrEF_matrix_not_ind, 
     file="data_files/HFmrEF_matrix_not_ind.Rdat")

# ----------------------------------------------------------- #