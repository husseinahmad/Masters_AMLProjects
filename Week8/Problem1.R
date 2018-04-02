library(glmnet)
library(MASS)
library(caret)
#library(DAAG)

m_data <- read.table("default_plus_chromatic_features_1059_tracks.txt", sep = ',', header=FALSE)
 
#shift lat and long

m_data[,117] <- m_data[,117] + 90
m_data[,118] <- m_data[,118] + 90

########################### Section A ###################################
print("Question 1 R-Squared")

# latitude regression
fit_lat <- lm(formula = V117 ~ ., data = m_data[, c(-118)] )
# RSquared
print(var(fit_lat$fitted.values)/var(m_data[,117] ))
# residuals against fitted
plot(fit_lat$fitted.values, fit_lat$residuals, ylab = "Residuals", xlab = "Fitted Values")

# CV error, 10 folds
foldSize <- nrow(m_data) / 10
current <- 1
s_cvError <- 0
for(i in 1:10)
{
  m_trainingPositions <- as.matrix(seq(current,current+foldSize-1))
  current <- current + foldSize
  
  m_trainingData <- m_data[-m_trainingPositions,]
  m_testing_data <- m_data[m_trainingPositions,]
  fit_lat <- lm(formula = V117 ~ ., data = m_trainingData[, c(-118)] )
  v_predicted_lat <- predict(fit_lat, m_testing_data[,c(-117,-118)])
  s_cvError <- s_cvError  + (sum((v_predicted_lat - m_testing_data[,117]) ^ 2) / length(v_predicted_lat)) 
}
print(paste("Unrelglarized Lat CV error", s_cvError/10))

#################################################################################
# longitude regression
fit_long <- lm(formula = V118 ~ ., data = m_data[, c(-117)] )
# RSquared 
print(var(fit_long$fitted.values)/var(m_data[,118] ))
# residuals against fitted
plot(fit_long$fitted.values, fit_long$residuals, ylab = "Residuals", xlab = "Fitted Values")

# MSE
# CV error, 10 folds
foldSize <- nrow(m_data) / 10
current <- 1
s_cvError <- 0
for(i in 1:10)
{
  m_trainingPositions <- as.matrix(seq(current,current+foldSize-1))
  current <- current + foldSize
  
  m_trainingData <- m_data[-m_trainingPositions,]
  m_testing_data <- m_data[m_trainingPositions,]
  fit_long <- lm(formula = V118 ~ ., data = m_trainingData[, c(-117)] )
  v_predicted_long <- predict(fit_long, m_testing_data[,c(-117,-118)])
  s_cvError <- s_cvError  + (sum((v_predicted_long - m_testing_data[,118]) ^ 2) / length(v_predicted_long)) 
}
print(paste("Unrelglarized Long CV error", s_cvError/10))

########################### Section B ###################################
print("Boxcox transformation R-Squared")

# latitude transformation
t_boxcox_lat <- boxcox(fit_lat) #after analysis, lambda equal 2 is best
transformed_lat <- ((m_data[,117] ^ 2) - 1) / 2
m_boxcox_lat_data <- cbind(m_data[,c(-117,-118)], transformed_lat)
fit_lat_boxCox <- lm(formula = transformed_lat ~ ., data = m_boxcox_lat_data )
reconstructed_lat <- sqrt((fit_lat_boxCox$fitted.values * 2) + 1)
# R-Squared 
print(var(reconstructed_lat)/var(m_data[,117]))
plot(reconstructed_lat, m_data[,117] - reconstructed_lat, ylab = "Residuals", xlab = "Fitted Values")

foldSize <- nrow(m_data) / 10
current <- 1
s_cvError <- 0
for(i in 1:10)
{
  m_trainingPositions <- as.matrix(seq(current,current+foldSize-1))
  current <- current + foldSize
  
  m_trainingData <- m_data[-m_trainingPositions,]
  m_testing_data <- m_data[m_trainingPositions,]
  transformed_lat <- ((m_testing_data[,117] ^ 2) - 1) / 2
  v_predicted_lat_boxcox <- predict(fit_lat_boxCox, m_testing_data[,-117])
  reconstructed_lat_test <- sqrt((v_predicted_lat_boxcox * 2) + 1)
# MSE
  s_cvError <- s_cvError + ((sum((reconstructed_lat_test - m_testing_data[,117]) ^ 2) / length(v_predicted_lat_boxcox)))
}
print(paste("BoxCox CV error", s_cvError/10))

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
#plot(fit$fit.preval[,s_lambdaPos], m_data[,117] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
# R Squared 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))
# CV Error
print(fit$cvm[s_lambdaPos])
print(fit$lambda.min)


# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 0)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
#plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
# R Squared  
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))
# MSE
print(fit$cvm[s_lambdaPos])
print(fit$lambda.min)

########################### Section D/ Lasso regularization ###################################
print("Lasso R-Squared")

#latitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,117], keep = TRUE, alpha = 1)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
#plot(fit$fit.preval[,s_lambdaPos], m_data[,117] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
# R Squared 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))
# CV Error
print(fit$cvm[s_lambdaPos])
print(fit$lambda.min)
#no of non zero
print(fit$nzero[s_lambdaPos])

# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 1)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
#plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
# R Squared  
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))
# MSE
print(fit$cvm[s_lambdaPos])
print(fit$lambda.min)
#no of non zero
print(fit$nzero[s_lambdaPos])

########################### Section E/ Elastic net ###################################
print("Elastic net 0.25")


#latitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,117], keep = TRUE, alpha = 0.25)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
#plot(fit$fit.preval[,s_lambdaPos], m_data[,117] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
# R Squared 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))
# CV Error
print(fit$cvm[s_lambdaPos])
print(fit$lambda.min)
#no of non zero
print(fit$nzero[s_lambdaPos])

# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 0.25)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
#plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
# R Squared  
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))
# MSE
print(fit$cvm[s_lambdaPos])
print(fit$lambda.min)
#no of non zero
print(fit$nzero[s_lambdaPos])


print("Elastic net 0.5")


#latitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,117], keep = TRUE, alpha = 0.5)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
#plot(fit$fit.preval[,s_lambdaPos], m_data[,117] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
# R Squared 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))
# CV Error
print(fit$cvm[s_lambdaPos])
print(fit$lambda.min)
#no of non zero
print(fit$nzero[s_lambdaPos])

# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 0.5)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
#plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
# R Squared  
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))
# MSE
print(fit$cvm[s_lambdaPos])
print(fit$lambda.min)
#no of non zero
print(fit$nzero[s_lambdaPos])


print("Elastic net 0.75")


#latitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,117], keep = TRUE, alpha = 0.75)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
#plot(fit$fit.preval[,s_lambdaPos], m_data[,117] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
# R Squared 
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))
# CV Error
print(fit$cvm[s_lambdaPos])
print(fit$lambda.min)
#no of non zero
print(fit$nzero[s_lambdaPos])

# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 0.75)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
#plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
# R Squared  
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))
# MSE
print(fit$cvm[s_lambdaPos])
print(fit$lambda.min)
#no of non zero
print(fit$nzero[s_lambdaPos])
