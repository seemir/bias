# ----------------------------------------------------------- #
# Install relevant packages (if not already done)
# ----------------------------------------------------------- #
Packages <- c("mlbench", "caret")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load relevant packages
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)
source("utilities.R")

# ----------------------------------------------------------- #
# Load data files
# ----------------------------------------------------------- #
allDataFiles <- c("HFfullImp", "HFfullOutcomes")
lapply(gsub(" ", "", paste("data_files/", allDataFiles, 
                           ".Rdat")), load,.GlobalEnv)

# ----------------------------------------------------------- #
# Add cross validation configuration
# ----------------------------------------------------------- #
kfold <- trainControl(method = "cv", number = 10)
loocv <- trainControl(method = "LOOCV")
seed <- 90210; metric <- "Accuracy"

# ----------------------------------------------------------- #
# Train and evaluate the classification algorithms with kfold
# ----------------------------------------------------------- #
dataset <- HFfullImp[,-1]
mortality <- HFfullOutcomes[,3]
readmission <- HFfullOutcomes[,4]

# ----------------------------------------------------------- #
# kfold CV evaluation of classifiers
# ----------------------------------------------------------- #
fit.knn.kfold <- train(dataset, mortality, method="knn", 
                       metric=metric, preProc="pca", 
                       trControl=kfold)

fit.svm.kfold <- train(dataset, mortality, method="svmRadial", 
                       metric=metric, preProc="pca", 
                       trControl=kfold, fit = F)

fit.rf.kfold <- train(dataset, mortality, method="rf", 
                      metric = metric, preProcess = "pca", 
                      trControl = kfold)

# ----------------------------------------------------------- #
