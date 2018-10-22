# ----------------------------------------------------------- #
# Install relevant packages (if not already done)
# ----------------------------------------------------------- #
Packages <- c("mlbench", "caret", "elasticnet", "klarR")
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
kfold <- trainControl(method = "cv", number = 5)
seed <- 902109
metric <- "Accuracy"

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
set.seed(seed)
fitKnnKfoldMort <- train(dataset, mortality, method="knn", 
                         metric=metric, trControl=kfold)

set.seed(seed)
fitLLKfoldMort <- train(dataset, mortality, method = "glm",
                        metric=metric, trControl = kfold)

set.seed(seed)
fitLDAKfoldMort <- train(dataset, mortality, method = "lda",
                         metric = metric, trControl = kfold)

set.seed(seed)
fitNbKfoldMort <- train(dataset, mortality, method = "nb",
                        metric = metric, trControl = kfold)

set.seed(seed)
fitSvmKfoldMort <- train(dataset, mortality,method="svmRadial", 
                         metric=metric, trControl=kfold)

set.seed(seed)
fitRfKfoldMort <- train(dataset, mortality, method="rf", 
                        metric = metric, trControl = kfold)

# ----------------------------------------------------------- #
# Produce summary statistics and plots
# ----------------------------------------------------------- #
# Kfold CV
# ----------------------------------------------------------- #
resultsMortalityKfold <- resamples(list(knn = fitKnnKfoldMort,
                                        logr = fitLLKfoldMort,
                                        lda = fitLDAKfoldMort,
                                        nb = fitNbKfoldMort,
                                        svm = fitSvmKfoldMort,
                                        rf = fitRfKfoldMort))

summary(resultsMortalityKfold)

tikzDevice::tikz("classification_mortality", width = 6, 
                 height = 5)
dotplot(resultsMortalityKfold, main = "Mortality")
dev.off()

# ----------------------------------------------------------- #
# Readmission
# ----------------------------------------------------------- #
# kfold CV evaluation of classifiers
# ----------------------------------------------------------- #
set.seed(seed)
fitKnnKfoldReadm <- train(dataset, readmission, method="knn", 
                          metric=metric, trControl=kfold)

set.seed(seed)
fitLLKfoldReadm <- train(dataset, readmission, method = "glm",
                        metric=metric, trControl = kfold)

set.seed(seed)
fitLDAKfoldReadm <- train(dataset[,-19], readmission, 
                          method = "lda", metric = metric, 
                          trControl = kfold)

set.seed(seed)
fitNbKfoldReadm <- train(dataset, readmission, method = "nb",
                        metric = metric, trControl = kfold)

set.seed(seed)
fitSvmKfoldReadm <- train(dataset, readmission, 
                          method="svmRadial", metric=metric,
                          trControl=kfold)

set.seed(seed)
fitRfKfoldReadm <- train(dataset, readmission, method="rf", 
                         metric = metric, trControl = kfold)

# ----------------------------------------------------------- #
# Produce summary statistics and plots
# ----------------------------------------------------------- #
# Kfold CV
# ----------------------------------------------------------- #
resultsReadmKfold <- resamples(list(knn = fitKnnKfoldReadm, 
                                    lda = fitLDAKfoldReadm,
                                    nb = fitNbKfoldReadm,
                                    logr = fitLLKfoldReadm,
                                    svm = fitSvmKfoldReadm,
                                    rf = fitRfKfoldReadm))

summary(resultsReadmKfold); 
tikzDevice::tikz("classification_readmission", width = 6, 
                 height = 5)
dotplot(resultsReadmKfold, main = "Re-admission")
dev.off()

# ----------------------------------------------------------- #