# ----------------------------------------------------------- #
# Install relevant packages (if not already done)
# ----------------------------------------------------------- #
Packages <- c("reporttools", "VIM", "Hmisc", "xtable")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load relevant packages and source helper functions
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = T)
source("_helper_func.R")

# ----------------------------------------------------------- #
# Load HFpEF and HFmrEF datafiles
# ----------------------------------------------------------- #
load("data_files/data_use_HFpEF.Rdat") 
load("data_files/data_use_HFmrEF.Rdat") 

# ----------------------------------------------------------- #
# Plot of missing values distribution
# ----------------------------------------------------------- #
pathToImages <- "../../../doc/thesis/images/"

pdf(file=paste(c(pathToImages,"HFpEF_miss_dist.pdf"), 
               collapse = ""))
aggr(HFpEFmat, plot = T, sortVars = T, 
     bars = F, combined = T, ylabs = "", cex.axis = 0.7)
dev.off()

pdf(file = paste(c(pathToImages, "HFmrEF_miss_dist.pdf"),
                 collapse = ""))
aggr(HFmrEFmat, plot = T, 
     sortVars = T, bars = F, combined = T, ylabs = "", 
     cex.axis = 0.7)
dev.off()

# ----------------------------------------------------------- #
# Summary of variables
# ----------------------------------------------------------- #
# Reorder data matrix by phenotype domains
# ----------------------------------------------------------- #
# In HFpEF matrix
# ----------------------------------------------------------- #
idHFpEF <- c("patientid")
demoHFpEF <- c("age", "gender", "white", "asian", "black", 
              "otherethnicity")
admSympHFpEF <- c("breathless", "chestpain", "orthopnoea",
                  "peripheraloedema", "palpdizzyfalls", 
                  "pnd")
admSignHFpEF <- c("sbp", "dbp", "map", "admissionwgt", 
                  "height", "bmiadmission", "weightchange",
                  "admissionsbnp", "pulse", "bp",
                  "asympthf", "devicetherapy")
riskFactHFpEF <- c("a-fib", "copdasthma", "irondef", 
                   "obesity", "obesitybmi30", "nyhaclass", 
                   "dm","ihd", "osa")
comorHFpEF <- c("comorbidities")
ecgHFpEF <- c("ecgblock","ecgblockcomment","ecgqrsduration", 
              "ecgqrsother","ecgrate","ecgrhythmother","twi", 
              "lvh","normalecgqrs", "lbbb", "rbbb", "lvhlev",
              "sr")
labTestHFpEF <- c("albumin", "hb", "hba1c", "wbc", "tsat",
                  "glucose", "plts", "pcv", "ferritin",
                  "k", "ironlevels", "chol", "ntprobnp",
                  "gfr", "mcv", "na")
echoHFpEF <- c("lvef","ewave", "pasp", "tapse", "ea", "ee", 
                "laterals", "mr", "tr", "as", "awave", 
                "dilatedlv", "ladiameter", "ai", "laarea",
                "raarea", "rwma", "calculatede", "rvfunction",
                "edeceltime", "af")
outcomesHFpEF <- c("alive", "timefromprevadm", "timetohfadm",
                   "timetonextadm", "daysfollowupdischarge",
                   "hfhospitalisation", "daysfollowupbnp",
                   "los")

# ----------------------------------------------------------- #
# In HFmrEF matrix
# ----------------------------------------------------------- #
idHFmrEF <- c("patientid")
demoHFmrEF <- c("age", "gender", "white", "asian", "black", 
                "other", "mixed")
admSympHFmrEF <- c("breathless", "st")
admSignHFmrEF <- c("sbp", "dbp", "admissionwgt", "bp", 
                   "bmiadmission", "pulse", "sympthf",
                   "symptlvhf", "symptlvunder", 
                   "symptomatichfmref", "pathway")
riskFactHFmrEF <- c("a-fib", "copdasthma", "irondef", "dm", 
                    "obesity", "copdasthma", "ihd", 
                    "ar", "both", "cva", "sb", "procedures")
comorHFmrEF <- c("comorbidities", "numbercomorditiesnoida")
ecgHFmrEF <- c("ecgqrsduration", "ecgqrsother", "ecgrate",
               "ecgrhythmother", "lvh", "normalecgqrs","nsr", 
               "lbbb", "rbbb", "sr")
labTestHFmrEF <- c("hb", "wbc", "tsat", "plts", "pcv","crp", 
                   "ferritin", "k", "ironlevels", "chol", 
                   "ntprobnp", "gfr","mcv","na","troponin")
echoHFmrEF <- c("lvef","ewave", "pasp", "ee", "mr", "tr", 
                 "as", "ai", "rvfunction", "checkedee", 
                 "timetoecho", "ee13diastolic", "af", 
                 "eagroup", "edecelgroup", "finalla",
                 "lvhgroup", "lvhandorlae", "e9cms")
outcomesHFmrEF <- c("timetohfadm", "hfhospitalisation", 
                    "los", "dischargeweight", "cardiachosp",
                    "truehf", "timetofollowupfromdischarge",
                    "timetofirstcardiachospitalisation",
                    "timetofollowupfrombnp", "newhf")

# ----------------------------------------------------------- #
# Long descriptive statistics
# ----------------------------------------------------------- #
dsHFpEFnames <- c(idHFpEF, demoHFpEF, admSympHFpEF,
                    admSignHFpEF, riskFactHFpEF,
                    comorHFpEF, ecgHFpEF, labTestHFpEF,
                    echoHFpEF, outcomesHFpEF)
dsHFmrEFnames <- c(idHFmrEF, demoHFmrEF, admSympHFmrEF,
                    admSignHFmrEF, riskFactHFmrEF,
                    comorHFmrEF, ecgHFmrEF, labTestHFmrEF,
                    echoHFmrEF, outcomesHFmrEF)

# ----------------------------------------------------------- #
dfHFpEF <- as.data.frame(HFpEFmat[,dsHFpEFnames])
capDescHFpEF <- "Patient characteristics: HFpEF variables"
labDescHFpEF <- "tab:desc_stat_HFpEF_variables"
tableContinuous(dfHFpEF, stats = c("n", "na", "min", "max", 
                                   "mean", "median", "s", "q1",
                                   "q3"), cap = capDescHFpEF, 
                lab = labDescHFpEF)

# ----------------------------------------------------------- #
dfHFmrEF <- as.data.frame(HFmrEFmat[,dsHFmrEFnames])
capDescHFmrEF <- "Patient characteristics: HFmrEF variables"
labDescHFmrEF <- "tab:desc_stat_HFmrEF_variables"
tableContinuous(dfHFmrEF, stats = c("n", "na", "min", "max", 
                                    "mean", "median", "s", 
                                    "q1", "q3"), 
                cap = capDescHFmrEF, lab = labDescHFmrEF)

# ----------------------------------------------------------- #
# Outcomes table
# ----------------------------------------------------------- #
load("data_files/outcomes_HFpEF.Rdat") 
load("data_files/outcomes_HFmrEF.Rdat") 
r <- rep("", 5)
tabOutHFpEF <- rbind(label.summary(HFpEFoutcomes, 2, 
                                   c("Group", "Dead?", 
                                     "Readm?", "n", "% Tot"), 
                                   3, 5), r, r) 
tabOutHFmrEF <- label.summary(HFmrEFoutcomes, 
                              2, c("Group", "Dead?", "Readm?", 
                                   "n", "% Tot"), 3, 5)
print(xtable(cbind(tabOutHFpEF, tabOutHFmrEF)), 
                   include.rownames = F)

# ----------------------------------------------------------- #
# Tables of top 10 missing values variables in both data sets
# ----------------------------------------------------------- #
HFpEFmiss <- top.n.missing(HFpEFmat, 11)
HFmrEFmiss <- top.n.missing(HFmrEFmat, 11)

# ----------------------------------------------------------- #
# Combine missing values table and convert to Latex code
# ----------------------------------------------------------- #
xtable(cbind(round(HFpEFmiss,3), rownames(HFmrEFmiss), 
       round(HFmrEFmiss,3)))

# ----------------------------------------------------------- #