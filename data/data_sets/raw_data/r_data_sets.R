# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("R.matlab", "data.table","stringr")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load relevant packages
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)

# ----------------------------------------------------------- #
# Read matlab files into R
# ----------------------------------------------------------- #
data_set_HFpEF <- readMat('data_use_HFpEF.mat')
data_set_HFmrEF <- readMat('data_use_HFmrEF.mat')

# ----------------------------------------------------------- #
# Extract the data matrix from matlab files
# ----------------------------------------------------------- #
HFpEF_matrix <- data_set_HFpEF$All.data
HFmrEF_matrix <- data_set_HFmrEF$All.data

# ----------------------------------------------------------- #
# Get patient ids 
# ----------------------------------------------------------- #
patient_id_HFpEF <- data_set_HFpEF$patientID
patient_id_HFmrEF <- data_set_HFmrEF$patientID

# ----------------------------------------------------------- #
# Add patient ids to HF_matrices and all column names
# ----------------------------------------------------------- #
HFpEF_matrix <- cbind(patient_id_HFpEF, HFpEF_matrix)
HFmrEF_matrix <- cbind(patient_id_HFmrEF, HFmrEF_matrix)

colnames(HFpEF_matrix) <- c('patientid', 
                            as.vector(unlist(
                              data_set_HFpEF$Varnames)))
colnames(HFmrEF_matrix) <- c('patientid', 
                             as.vector(unlist(
                               data_set_HFmrEF$Varnames)))

# ----------------------------------------------------------- #
# Consolidate naming conventions for some variables
# ----------------------------------------------------------- #
# In the HFmrEF matrix
# ----------------------------------------------------------- #
setnames(as.data.frame(HFmrEF_matrix), 
         old=c("Admissionweight","BMI","Numberofcomorbidities",
               "Afrocaribbean", "Caucasian","Pulse","NtproBNP",
               "E", "ECGRhythm_other", "LVHand_orLAE", 
               "ECGQRS_other"), 
         new = c("admissionwgt","BmIadmission","comorbidities",
                 "Black","White","pulse","NTproBNP", "Ewave",
                 "ECGRhythmother", "LVHandorLAE", 
                 "ECGQRSother"))

# ----------------------------------------------------------- #
# In the HFpEF matrix
# ----------------------------------------------------------- #
setnames(as.data.frame(HFpEF_matrix), 
         old = c("E_e","LVfunction", "ECGRhythm_other", 
                 "ECGQRS_other", "Other_ethnicity"), 
         new = c("Ee", "LVEF", "ECGRhythmother", "ECGQRSother", 
                 "Otherethnicity"))

# ----------------------------------------------------------- #
# Lowercase letters for all the colnames
# ----------------------------------------------------------- #
colnames(HFmrEF_matrix) <- tolower(colnames(HFmrEF_matrix))
colnames(HFpEF_matrix) <- tolower(colnames(HFpEF_matrix))

# ----------------------------------------------------------- #
# Save the matrices as .Rdat
# ----------------------------------------------------------- #
save(HFpEF_matrix, file='data_use_HFpEF_matrix.Rdat')
save(HFmrEF_matrix, file='data_use_HFmrEF_matrix.Rdat')

# ----------------------------------------------------------- #
# Re-code patient group labels
# ----------------------------------------------------------- #
# Get patient groups
# ----------------------------------------------------------- #
patient_groups_HFpEF <- as.matrix(unlist(
                        data_set_HFpEF$Patient.group))
patient_groups_HFmrEF <- as.matrix(unlist(
                        data_set_HFmrEF$Patient.group))

# ----------------------------------------------------------- #
# Labels of clinical outcomes
# ----------------------------------------------------------- #
deceased <- c("IN", "Z", "Y", "X")
re_admission <- c("V", "U")

# ----------------------------------------------------------- #
# Split labels 
# ----------------------------------------------------------- #
HFmrEF_split <- str_split_fixed(patient_groups_HFmrEF,", ",n=2)
HFpEF_split <- str_split_fixed(patient_groups_HFpEF,", ", n=2)

# ----------------------------------------------------------- #
# Re-coding mortality labels
# ----------------------------------------------------------- #
is_deceased_HFmrEF <- HFmrEF_split[,1] %in% deceased
is_deceased_HFpEF <- HFpEF_split[,1] %in% deceased
deceased_HFmrEF <- ifelse(is_deceased_HFmrEF, "yes", "no")
deceased_HFpEF <- ifelse(is_deceased_HFpEF, "yes", "no")

# ----------------------------------------------------------- #
# Re-coding re-admission labels
# ----------------------------------------------------------- #
is_re_admitted_HFmrEF <- HFmrEF_split[,1] %in% re_admission | 
                         HFmrEF_split[,2] %in% re_admission
is_re_admitted_HFpEF <- HFpEF_split[,1] %in% re_admission | 
                        HFpEF_split[,2] %in% re_admission
re_admission_HFmrEF <- ifelse(is_re_admitted_HFmrEF,"yes","no")
re_admission_HFpEF <- ifelse(is_re_admitted_HFpEF,"yes","no")

# ----------------------------------------------------------- #
# Add outcomes to matrix
# ----------------------------------------------------------- #
HFmrEF_outcomes_matrix <- cbind(patient_id_HFmrEF,
                                patient_groups_HFmrEF,
                                deceased_HFmrEF,
                                re_admission_HFmrEF)
HFpEF_outcomes_matrix <- cbind(patient_id_HFpEF,
                               patient_groups_HFpEF,
                               deceased_HFpEF,
                               re_admission_HFpEF)

# ----------------------------------------------------------- #
# Add colnames to matrices
# ----------------------------------------------------------- #
colnames(HFmrEF_outcomes_matrix) <- 
colnames(HFpEF_outcomes_matrix) <- 
c("patientid", "patientgroup", "deceased", "readmitted")

# ----------------------------------------------------------- #
# Save the matrices as .Rdat file
# ----------------------------------------------------------- #
save(HFpEF_outcomes_matrix, file='outcomes_HFpEF_matrix.Rdat')
save(HFmrEF_outcomes_matrix,file='outcomes_HFmrEF_matrix.Rdat')

# ----------------------------------------------------------- #