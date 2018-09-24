# Reproducible Research - Roger G. Peng
# Example 1 - Email/Spam Classification

# Install kernlab package
install.packages("kernlab")
library(kernlab)
data(spam)

# Perform subsampling - random assignment of observations
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)

# Split dataset into train and test sets
trainSpam = spam[trainIndicator==1, ]
testSpam = spam[trainIndicator==0, ]

# Explore data set
# Obtain data summary, plots, check for missing values
names(trainSpam)
head(trainSpam)

# Obtain table count of email types
table(trainSpam$type)

# Plot Average Capital Letters by email type 
plot(trainSpam$capitalAve ~ trainSpam$type)

# Plot based 10 log transformed Average Capital Letters
# Add 1 when taking log of zeros
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)

# Explore Relationships between Predictors
plot(log10(trainSpam[,1:4] + 1))

# Cluster Analysis - Exploratory
hCluster = hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)

# Run cluster analysis on log transform of predictor space
hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClusterUpdated)

# STATISTICAL PREDICTION / MODELING
# Iterate through variables and fit logistic regression model for each
trainSpam$numType = as.numeric(trainSpam$type) - 1
costFunction = function(x,y) sum(x != (y > 0.5))
cvError = rep(NA, 55)
library(boot)
for (i in 1:55) {
    lmFormula = reformulate(names(trainSpam)[i], response = "numType")
    glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
    cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

# Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]

## Refit the model to make prediction about test set data
# Use best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)

# Get predictions on test set
precictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])

# Classify as 'spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

# Construct Classification Table
table(predictedSpam, testSpam$type)

# Obtain Error rate
(61 + 458)/(1146 + 458 + 61 + 549)





