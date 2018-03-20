library(glmnet)
library(MASS)
library(caret)

m_data <- read.table("default_plus_chromatic_features_1059_tracks.txt", sep = ',', header=FALSE)
 
#shift lat and long

m_data[,117] <- m_data[,117] + 90
m_data[,118] <- m_data[,118] + 90

m_trainingData <- createDataPartition(y = m_data[,1], p = 0.8, list = FALSE)

m_data <- m_data[m_trainingData,]
m_testing_data <- m_data[-m_trainingData,]

########################### Section A ###################################
print("Question 1 R-Squared")

# latitude regression
fit_lat <- lm(formula = V117 ~ ., data = m_data[, c(-118)] )
v_predicted_lat <- predict(fit_lat, m_testing_data[,-118])
# RSquared in training and testing
print(var(fit_lat$fitted.values)/var(m_data[,117] ))
print(var(v_predicted_lat)/var(m_testing_data[,117] ))
# residuals against fitted in training and testing
plot(fit_lat$fitted.values, fit_lat$residuals, ylab = "Residuals", xlab = "Fitted Values")
plot(v_predicted_lat, v_predicted_lat - m_testing_data[,117], ylab = "Residuals", xlab = "Fitted Values")
# MSE
print(sum((v_predicted_lat - m_testing_data[,117]) ^ 2) / length(v_predicted_lat))

# longitude regression
fit_long <- lm(formula = V118 ~ ., data = m_data[, c(-117)] )
v_predicted_long <- predict(fit_long, m_testing_data[,-117])
# RSquared in training and testing
print(var(fit_long$fitted.values)/var(m_data[,118] ))
print(var(v_predicted_long)/var(m_testing_data[,118] ))
# residuals against fitted in training and testing
plot(fit_long$fitted.values, fit_long$residuals, ylab = "Residuals", xlab = "Fitted Values")
plot(v_predicted_long, v_predicted_long - m_testing_data[,118], ylab = "Residuals", xlab = "Fitted Values")
# MSE
print(sum((v_predicted_long - m_testing_data[,118]) ^ 2) / length(v_predicted_long))

########################### Section B ###################################
print("Boxcox transformation R-Squared")

# latitude transformation
t_boxcox_lat <- boxcox(fit_lat) #after analysis, lambda equal 2 is best
transformed_lat <- ((m_data[,117] ^ 2) - 1) / 2
m_boxcox_lat_data <- cbind(m_data[,c(-117,-118)], transformed_lat)
fit_lat_boxCox <- lm(formula = transformed_lat ~ ., data = m_boxcox_lat_data )
reconstructed_lat <- sqrt((fit_lat_boxCox$fitted.values * 2) + 1)
# R-Squared over training, transformed and original coordinates
print(var(fit_lat_boxCox$fitted.values)/var(transformed_lat))
print(var(reconstructed_lat)/var(m_data[,117]))
plot(reconstructed_lat, m_data[,117] - reconstructed_lat, ylab = "Residuals", xlab = "Fitted Values")
transformed_lat <- ((m_testing_data[,117] ^ 2) - 1) / 2
v_predicted_lat_boxcox <- predict(fit_lat_boxCox, m_testing_data[,-117])
reconstructed_lat_test <- sqrt((v_predicted_lat_boxcox * 2) + 1)
# R-Squared over training, transformed and original coordinates
print(var(v_predicted_lat_boxcox)/var(transformed_lat))
print(var(reconstructed_lat_test)/var(m_data[,117]))
# MSE
print(sum((reconstructed_lat_test - m_testing_data[,117]) ^ 2) / length(v_predicted_lat_boxcox))


# longitude transformation
# after analysis, lambda equal 1 is best, which means it doesn't improve 
# as it will result in the same longitude shifted 1
t_boxcox_long <- boxcox(fit_long) 


########################### Section C/ Ridge regularization ###################################
print("Ridge R-Squared")

# latitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,117], keep = TRUE, alpha = 0)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_data[,117] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
v_predicted <- predict(fit, as.matrix(m_testing_data[,c(-117,-118)]))
# R Squared trianing 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))
print(var(v_predicted)/var(m_data[,117]))
# MSE
print(sum((v_predicted - m_testing_data[,117]) ^ 2) / length(v_predicted))


# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 0)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
v_predicted <- predict(fit, as.matrix(m_testing_data[,c(-117,-118)]))
# R Squared trianing 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))
print(var(v_predicted)/var(m_data[,118]))
# MSE
print(sum((v_predicted - m_testing_data[,118]) ^ 2) / length(v_predicted))


########################### Section D/ Lasso regularization ###################################
print("Lasso R-Squared")

# latitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,117], keep = TRUE, alpha = 1)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_data[,117] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
v_predicted <- predict(fit, as.matrix(m_testing_data[,c(-117,-118)]))
# R Squared trianing 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))
print(var(v_predicted)/var(m_data[,117]))
# MSE
print(sum((v_predicted - m_testing_data[,117]) ^ 2) / length(v_predicted))
#no of non zero
print(fit$nzero[s_lambdaPos])

# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 1)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
#plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
v_predicted <- predict(fit, as.matrix(m_testing_data[,c(-117,-118)]))
# R Squared trianing 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))
print(var(v_predicted)/var(m_data[,118]))
# MSE
print(sum((v_predicted - m_testing_data[,118]) ^ 2) / length(v_predicted))
#no of non zero
print(fit$nzero[s_lambdaPos])

########################### Section E/ Elastic net ###################################
print("Elastic net R-Squared")

print("Alpha = 0.25")
# latitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,117], keep = TRUE, alpha = 0.25)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
v_predicted <- predict(fit, as.matrix(m_testing_data[,c(-117,-118)]))
# R Squared trianing 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))
print(var(v_predicted)/var(m_data[,117]))
# MSE
print(sum((v_predicted - m_testing_data[,117]) ^ 2) / length(v_predicted))
#no of non zero
print(fit$nzero[s_lambdaPos])

# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 0.25)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
v_predicted <- predict(fit, as.matrix(m_testing_data[,c(-117,-118)]))
# R Squared trianing 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))
print(var(v_predicted)/var(m_data[,118]))
# MSE
print(sum((v_predicted - m_testing_data[,118]) ^ 2) / length(v_predicted))
#no of non zero
print(fit$nzero[s_lambdaPos])

print("Alpha = 0.5")
# latitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,117], keep = TRUE, alpha = 0.5)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
v_predicted <- predict(fit, as.matrix(m_testing_data[,c(-117,-118)]))
# R Squared trianing 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))
print(var(v_predicted)/var(m_data[,117]))
# MSE
print(sum((v_predicted - m_testing_data[,117]) ^ 2) / length(v_predicted))
#no of non zero
print(fit$nzero[s_lambdaPos])

# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 0.5)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
v_predicted <- predict(fit, as.matrix(m_testing_data[,c(-117,-118)]))
# R Squared trianing 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))
print(var(v_predicted)/var(m_data[,118]))
# MSE
print(sum((v_predicted - m_testing_data[,118]) ^ 2) / length(v_predicted))
#no of non zero
print(fit$nzero[s_lambdaPos])

print("Alpha = 0.75")
# latitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,117], keep = TRUE, alpha = 0.75)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
v_predicted <- predict(fit, as.matrix(m_testing_data[,c(-117,-118)]))
# R Squared trianing 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))
print(var(v_predicted)/var(m_data[,117]))
# MSE
print(sum((v_predicted - m_testing_data[,117]) ^ 2) / length(v_predicted))
#no of non zero
print(fit$nzero[s_lambdaPos])

# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 0.75)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
v_predicted <- predict(fit, as.matrix(m_testing_data[,c(-117,-118)]))
# R Squared trianing 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))
print(var(v_predicted)/var(m_data[,118]))
# MSE
print(sum((v_predicted - m_testing_data[,118]) ^ 2) / length(v_predicted))
#no of non zero
print(fit$nzero[s_lambdaPos])

