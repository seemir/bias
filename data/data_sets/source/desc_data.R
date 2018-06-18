# ----------------------------------------------------------- #
# Install relevant packages (if not already done)
# ----------------------------------------------------------- #
Packages <- c("reporttools", "VIM", "Hmisc", "xtable", 
              "tikzDevice")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load relevant packages and source helper functions
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = T)
source("_helper_func.R")

# ----------------------------------------------------------- #
# Load HFpEF and HFmrEF datafiles
# ----------------------------------------------------------- #
path <- "data_files/"
fileNames <- c("HFpEFfullDataSet", "HFmrEFfullDataSet",
               "HFpEFoutcomes", "HFmrEFoutcomes")
r <- ".Rdat"
for (file in fileNames){
  load(paste(path, file, r, sep = ""))
}

# ----------------------------------------------------------- #
# Plot of missing values distribution
# ----------------------------------------------------------- #
pathToImages <- "../../../doc/thesis/images/"

tikz(file=paste(c(pathToImages,"HFpEF_miss_dist.tex"), 
               collapse = ""))
aggr(HFpEFfullDataSet, plot = T, sortVars = T, 
     bars = F, combined = T, ylabs = "", cex.axis = 0.7)
dev.off()

tikz(file = paste(c(pathToImages, "HFmrEF_miss_dist.tex"),
                 collapse = ""))
aggr(HFmrEFfullDataSet, plot = T, 
     sortVars = T, bars = F, combined = T, ylabs = "", 
     cex.axis = 0.7)
dev.off()

# ----------------------------------------------------------- #
# Summary of variables
# ----------------------------------------------------------- #
# Reorder data matrix by phenotype domains
# ----------------------------------------------------------- #
nameOrder <- c("age", "gender", "white", "asian", "black", 
               "breathless", "sbp", "dbp", "admissionwgt", 
               "bp", "bmiadmission", "pulse", "a-fib", 
               "copdasthma", "irondef", "dm", "obesity", 
               "copdasthma", "ihd", "comorbidities", 
               "ecgqrsduration", "ecgqrsother", "ecgrate", 
               "ecgrhythmother", "lvh", "normalecgqrs", "lbbb", 
               "rbbb", "sr", "hb", "wbc", "tsat", "plts", "pcv", 
               "ferritin", "k", "ironlevels", "chol", 
               "ntprobnp", "gfr", "mcv", "na", "lvef", "ewave", 
               "pasp", "ee", "mr", "tr", "as", "ai", 
               "rvfunction", "af", "timetohfadm", 
               "hfhospitalisation", "los")

# ----------------------------------------------------------- #
# Descriptive statistics
# ----------------------------------------------------------- #
capHFpEF <- "Patient characteristics: HFpEF"
labHFpEF <- "tab:desc_stat_HFpEF"
tableContinuous(HFpEFfullDataSet[, nameOrder],  
                stats = c("n", "na", "min", "max", "mean", 
                          "median", "s", "q1", "q3"),  
                cap = capHFpEF, lab = labHFpEF)

# ----------------------------------------------------------- #
capHFmrEF <- "Patient characteristics: HFmrEF"
labHFmrEF <- "tab:desc_stat_HFmrEF"
tableContinuous(HFmrEFfullDataSet[, nameOrder], 
                stats = c("n", "na", "min", "max", "mean", 
                          "median", "s", "q1", "q3"),  
                cap = capHFmrEF, lab = labHFmrEF)

# ----------------------------------------------------------- #
# Outcomes table
# ----------------------------------------------------------- #
r <- rep("", 5)
tabOutHFpEF <- rbind(label.summary(as.matrix(HFpEFoutcomes), 
                     2, c("Group", "Dead?", "Readm?", "n", 
                          "% Tot"), 3, 5), r, r)

tabOutHFmrEF <- label.summary(as.matrix(HFmrEFoutcomes), 
                2, c("Group", "Dead?", "Readm?", 
                     "n", "% Tot"), 3, 5)
print(xtable(cbind(tabOutHFpEF, tabOutHFmrEF)), 
                   include.rownames = F)

# ----------------------------------------------------------- #
# Tables of top 10 missing values variables in both data sets
# ----------------------------------------------------------- #
HFpEFmiss <- top.n.missing(HFpEFfullDataSet, 10)
HFmrEFmiss <- top.n.missing(HFmrEFfullDataSet, 10)

# ----------------------------------------------------------- #
# Combine missing values table and convert to Latex code
# ----------------------------------------------------------- #
xtable(cbind(round(HFpEFmiss,3), rownames(HFmrEFmiss), 
       round(HFmrEFmiss,3)))

# ----------------------------------------------------------- #