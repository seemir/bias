# ----------------------------------------------------------- #
# Install packages (if not already installed)
# ----------------------------------------------------------- #
Packages <- c("Amelia", "mice")
# install.packages(Packages)

# ----------------------------------------------------------- #
# Load package for docstring
# ----------------------------------------------------------- #
lapply(Packages, library, character.only = TRUE)

# ----------------------------------------------------------- #
# Load data set with same variables and source helper functions
# ----------------------------------------------------------- #
load("data_files/HF_full_data_set.Rdat")
load("data_files/syndromes_HF_full.Rdat")
source("_helper_func.R")

# ----------------------------------------------------------- #
# Summary of missing variables
# ----------------------------------------------------------- #
top.n.missing(HFfullDataSet, 10)

# ----------------------------------------------------------- #
# Split variables into indicator and categorical variables
# ----------------------------------------------------------- #
HFfullRmInd <- rm.indicator(HFfullDataSet, 8)
HFfullInd <- HFfullRmInd$indicator
HFfullNoInd <- HFfullRmInd$non.indicator

# ----------------------------------------------------------- #
# Impute data using Bootstrap EM and CART
# ----------------------------------------------------------- #
bnd <- data.bounds(HFfullNoInd[, -1], 0, Inf)
HFfullEm <- boot.em.impute(HFfullNoInd[, -1], bnd, n.boot = 10)
HFfullCart <- complete(mice(HFfullInd, method = "cart"))

# ----------------------------------------------------------- #
# Combine imputed data sets into one 
# ----------------------------------------------------------- #
HFfullImpDataSet <- cbind(HFfullEm, HFfullCart)

# ----------------------------------------------------------- #
# Principal component analysis
# ----------------------------------------------------------- #
HFfullpca <- princomp(HFfullImpDataSet, cor = T)

# ----------------------------------------------------------- #
# Explained variance 
# ----------------------------------------------------------- #
pca.var.plot(HFfullpca, 41, title = "HF same variables")

# ----------------------------------------------------------- #
# Cluster plot
# ----------------------------------------------------------- #
pca.cluster.plot(HFfullpca, 41, hc.clust = 2, ellipse = F)

# ----------------------------------------------------------- #
# Remove possible outlier obsnr. 328
# ----------------------------------------------------------- #
HFfullpca <- princomp(HFfullImpDataSet[-328, ], cor = T)
ClustPlot <- pca.cluster.plot(HFfullpca, 41, ellipse = F, 
                              hc.clust = 2)

# ----------------------------------------------------------- #
# Actual clustering configuration
# ----------------------------------------------------------- #
pca.cluster.plot(HFfullpca, 41, hc.clust = 2, 
                 actual = SyndClass[-328, 2])

# ----------------------------------------------------------- #
# Logistic regression
# ----------------------------------------------------------- #
library(caret)
library(plyr)

syndrome <- ifelse(SyndClass[-328, 2]==2, 1, 0)
dat <- as.data.frame(cbind(syndrome, HFfullpca$scores)) 

fpr <- NULL # False positive rate
fnr <- NULL # False negative rate
k <- 500 # Number of iterations

# Accuracy
acc <- NULL

for(i in 1:k)
{
  # Train-test splitting
  # 95% of samples -> fitting
  # 5% of samples -> testing
  smp_size <- floor(0.95 * nrow(dat))
  index <- sample(seq_len(nrow(dat)), size=smp_size)
  train <- dat[index, ]
  test <- dat[-index, ]

  # Fitting
  model <- glm(syndrome~.,family=binomial,data=train)
  
  # Predict results
  results_prob <- predict(model, subset(test, select=c(2:55)), 
                          type='response')
  
  # If prob > 0.5 then 1, else 0
  results <- as.factor(ifelse(results_prob > 0.5, 1, 0))
  
  # Actual answers
  answers <- as.factor(test$syndrome)
  
  # Accuracy calculation
  misClasificError <- mean(answers != results)
  
  # Collecting results
  acc[i] <- 1-misClasificError
  
  # Confusion matrix
  cm <- confusionMatrix(data=results, reference=answers)
  fpr[i] <- cm$table[2]/(nrow(dat)-smp_size)
  fnr[i] <- cm$table[3]/(nrow(dat)-smp_size)
}

# Average accuracy of the model
acc <- acc[acc != 1]
mean(acc)

# ----------------------------------------------------------- #