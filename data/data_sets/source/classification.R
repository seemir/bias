# ----------------------------------------------------------- #
# Install relevant packages (if not already done)
# ----------------------------------------------------------- #
Packages <- c("mlbench", "caret", "elasticnet")
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
kfold <- trainControl(method = "cv", number = 10, 
                      returnResamp = "all")
loocv <- trainControl(method = "LOOCV")
seed <- 90210; metric <- "Accuracy"

# ----------------------------------------------------------- #
# Train and evaluate the classification algorithms with kfold
# ----------------------------------------------------------- #
dataset <- HFfullImp[,-1]
mortality <- HFfullOutcomes[,3]
readmission <- HFfullOutcomes[,4]

# ----------------------------------------------------------- #
# Mortality
# ----------------------------------------------------------- #
# kfold CV evaluation of classifiers
# ----------------------------------------------------------- #
fitKnnKfoldMort <- train(dataset, mortality, method="knn", 
                         metric=metric, trControl=kfold)

fitSvmKfoldMort <- train(dataset, mortality,method="svmRadial", 
                         metric=metric, trControl=kfold)

fitRfKfoldMort <- train(dataset, mortality, method="rf", 
                        metric = metric, trControl = kfold)

# ----------------------------------------------------------- #
# loocv CV evaluation of classifiers
# ----------------------------------------------------------- #
fitKnnLoocvMort <- train(dataset, mortality, method="knn", 
                         metric=metric, trControl=loocv)

fitSvmLoocvMort <- train(dataset, mortality,method="svmRadial", 
                         metric=metric, trControl=loocv)

fitRfLoocvMort <- train(dataset, mortality, method="rf", 
                        metric = metric, trControl = loocv)

# ----------------------------------------------------------- #
# Produce summary statistics and plots
# ----------------------------------------------------------- #
# Kfold CV
# ----------------------------------------------------------- #
resultsMortalityKfold <- resamples(list(knn = fitKnnKfoldMort, 
                                        svm = fitSvmKfoldMort,
                                        rf = fitRfKfoldMort))

summary(resultsMortalityKfold); dotplot(resultsMortalityKfold)

# ----------------------------------------------------------- #
# Loocv CV
# ----------------------------------------------------------- #
resultsMortalityLoocv <- resamples(list(knn = fitKnnLoocvMort,
                                        svm = fitSvmLoocvMort,
                                        rf = fitRfLoocvMort))

summary(resultsMortalityLoocv); dotplot(resultsMortalityLoocv)

# ----------------------------------------------------------- #
# Readmission
# ----------------------------------------------------------- #
# kfold CV evaluation of classifiers
# ----------------------------------------------------------- #
fitKnnKfoldReadm <- train(dataset, readmission, method="knn", 
                          metric=metric, trControl=kfold)

fitSvmKfoldReadm <- train(dataset, readmission, 
                          method="svmRadial", metric=metric,
                          trControl=kfold)

fitRfKfoldReadm <- train(dataset, readmission, method="rf", 
                         metric = metric, trControl = kfold)

# ----------------------------------------------------------- #
# Loocv evaluation of classifiers
# ----------------------------------------------------------- #
fitKnnLoocvReadm <- train(dataset, readmission, method="knn", 
                          metric=metric, trControl=loocv)

fitSvmLoocvReadm <- train(dataset, readmission, 
                          method="svmRadial", metric=metric,
                          trControl=loocv)

fitRfLoocvReadm <- train(dataset, readmission, method="rf", 
                         metric = metric, trControl = loocv)

# ----------------------------------------------------------- #
# Produce summary statistics and plots
# ----------------------------------------------------------- #
# Kfold CV
# ----------------------------------------------------------- #
resultsReadmKfold <- resamples(list(knn = fitKnnKfoldReadm, 
                                   svm = fitSvmKfoldReadm,
                                   rf = fitRfKfoldReadm))

summary(resultsReadmKfold); dotplot(resultsReadmKfold)

# ----------------------------------------------------------- #
# LOOCV CV
# ----------------------------------------------------------- #
resultsReadmLoocv <- resamples(list(knn = fitKnnLoocvReadm, 
                                    svm = fitSvmLoocvReadm,
                                    rf = fitRfLoocvReadm))

summary(resultsReadmLoocv); dotplot(resultsReadmLoocv)

# ----------------------------------------------------------- #