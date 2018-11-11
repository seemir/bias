# ----------------------------------------------------------- #
# Function for sourcing package info
# ----------------------------------------------------------- #
source_lines <- function(file, lines){
  source(textConnection(readLines(file, warn = F)[lines]))
}

# ----------------------------------------------------------- #
# Extract all package installed in all files
# ----------------------------------------------------------- #
packages <- c()
files <- c("utilities.R", "desc_stat.R", "pre_process.R", 
           "clustering.R", "classification.R", 
           "../raw_data/consolidation.R")

for (file in files){
  source_lines(file, 1:10)
  packages <- c(packages, Packages)
}

# ----------------------------------------------------------- #
# Extract title, version and author information
# ----------------------------------------------------------- #
title <- c(); version <- c()
for (package in packages){
  title <- c(title, packageDescription(package)$Title)
  version <- c(version, packageDescription(package)$Version)
}

# ----------------------------------------------------------- #
# Build LaTex table with all the package info
# ----------------------------------------------------------- #
packagesUsed <- as.data.frame(matrix(c(packages, title, 
                                       version),ncol = 3))
colnames(packagesUsed) <- c("Package", "Title", "Version")
packagesUsed <- unique(packagesUsed[packagesUsed$Package,])
packagesUsed <- packagesUsed[order(packagesUsed$Package),]
rownames(packagesUsed) <- 1:nrow(packagesUsed)
print(xtable(packagesUsed), include.rownames=FALSE)

# ----------------------------------------------------------- #