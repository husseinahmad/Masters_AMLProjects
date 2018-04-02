

library(glmnet)
library(MASS)
library(caret)

m_data <- read.table("defaultCreditCard.txt", sep = '\t', header=TRUE)
m_features <- m_data[,-c(24)]
v_labels <- m_data[,24]


##### no regularization, can't use CV.glmnet as it expects lambda values, used manual CV with 5 folds
foldSize <- nrow(m_features) / 10
current <- 1
errratem <- 0
for(i in 1:10)
{
  m_trainingData <- as.matrix(seq(current,current+foldSize-1))
  current <- current + foldSize
  m_trainingFeatures <- m_features[-m_trainingData,]
  m_trainingLabels <- v_labels[-m_trainingData]
  m_testingFeatures <- m_features[m_trainingData,]
  m_testingLabels <- v_labels[m_trainingData]
  fit <- glmnet(x = as.matrix(m_trainingFeatures), y = m_trainingLabels,  family = "binomial", lambda = 0)
  lmpredn <- predict(fit, as.matrix(m_testingFeatures), type="class")
  nmright <- sum( m_testingLabels==lmpredn) 
  errratem <- errratem + (1 - nmright/length(m_testingLabels))
}
print(errratem/10)


##### Ridge regularization
fit <- cv.glmnet(x = as.matrix(m_features),y = v_labels,  family = "binomial",  keep = TRUE, alpha = 0, type.measure = "class")
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
print(fit$lambda.min)
print(fit$cvm[s_lambdaPos])
print(fit$nzero[s_lambdaPos])


##### Lasso regularization
fit <- cv.glmnet(x = as.matrix(m_features),y = v_labels,  family = "binomial",  keep = TRUE, alpha = 1, type.measure = "class")
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
print(fit$lambda.min)
print(fit$cvm[s_lambdaPos])
print(fit$nzero[s_lambdaPos])

##### Elastic net regularization
fit <- cv.glmnet(x = as.matrix(m_features),y = v_labels,  family = "binomial",  keep = TRUE, alpha = 0.25, type.measure = "class")
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
print(fit$lambda.min)
print(fit$cvm[s_lambdaPos])
print(fit$nzero[s_lambdaPos])
#####################################
fit <- cv.glmnet(x = as.matrix(m_features),y = v_labels,  family = "binomial",  keep = TRUE, alpha = 0.5, type.measure = "class")
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
print(fit$lambda.min)
print(fit$cvm[s_lambdaPos])
print(fit$nzero[s_lambdaPos])
#####################################
fit <- cv.glmnet(x = as.matrix(m_features),y = v_labels,  family = "binomial",  keep = TRUE, alpha = 0.75, type.measure = "class")
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
print(fit$lambda.min)
print(fit$cvm[s_lambdaPos])
print(fit$nzero[s_lambdaPos])


