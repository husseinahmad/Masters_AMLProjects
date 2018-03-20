

library(glmnet)
library(MASS)
library(caret)

m_data <- read.table("defaultCreditCard.txt", sep = '\t', header=TRUE)
m_features <- m_data[,-c(24)]
v_labels <- m_data[,24]
m_trainingData <- createDataPartition(y = v_labels, p = 0.8, list = FALSE)

m_trainingFeatures <- m_features[m_trainingData,]
m_trainingLabels <- v_labels[m_trainingData]

m_testingFeatures <- m_features[-m_trainingData,]
m_testingLabels <- v_labels[-m_trainingData]

##### Ridge regularization
fit <- cv.glmnet(x = as.matrix(m_trainingFeatures),y = m_trainingLabels,  family = "binomial",  keep = TRUE, alpha = 0)
s_lambdaPos <- match(fit$lambda.1se, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_trainingLabels - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fit.preval[,s_lambdaPos])/var(m_trainingLabels))

lmpredn <- predict(fit, as.matrix(m_testingFeatures), type="class", s = "lambda.min")
nmright <- sum( m_testingLabels==lmpredn) 
errratem <- (1 - nmright/length(m_testingLabels))
print(errratem)


##### Lasso regularization
fit <- cv.glmnet(x = as.matrix(m_trainingFeatures),y = m_trainingLabels,  family = "binomial",  keep = TRUE, alpha = 1)
s_lambdaPos <- match(fit$lambda.1se, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_trainingLabels - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fit.preval[,s_lambdaPos])/var(m_trainingLabels))

lmpredn <- predict(fit, as.matrix(m_testingFeatures), type="class", s = "lambda.min")
nmright <- sum( m_testingLabels==lmpredn) 
errratem <- (1 - nmright/length(m_testingLabels))
print(errratem)


##### Ridge regularization
fit <- cv.glmnet(x = as.matrix(m_trainingFeatures),y = m_trainingLabels,  family = "binomial",  keep = TRUE, alpha = 0.5)
s_lambdaPos <- match(fit$lambda.1se, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_trainingLabels - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fit.preval[,s_lambdaPos])/var(m_trainingLabels))

lmpredn <- predict(fit, as.matrix(m_testingFeatures), type="class", s = "lambda.min")
nmright <- sum( m_testingLabels==lmpredn) 
errratem <- (1 - nmright/length(m_testingLabels))
print(errratem)
