library(glmnet)
library(MASS)
library(caret)

m_data <- read.table("default_plus_chromatic_features_1059_tracks.txt", sep = ',', header=FALSE)
 
#shift lat and long

m_data[,117] <- m_data[,117] + 90
m_data[,118] <- m_data[,118] + 90



########################### Section A ###################################
print("Question 1 R-Squared")

# latitude regression
fit_lat <- lm(formula = V117 ~ ., data = m_data[, c(-118)] )
# RSquared in coordinates
print(var(fit_lat$fitted.values)/var(m_data[,117] ))
plot(fit_lat$fitted.values, fit_lat$residuals, ylab = "Residuals", xlab = "Fitted Values")

# longitude regression
fit_long <- lm(formula = V118 ~ ., data = m_data[, c(-117)] )
# RSquared in coordinates
print(var(fit_long$fitted.values)/var(m_data[,118] ))
plot(fit_long$fitted.values, fit_long$residuals, ylab = "Residuals", xlab = "Fitted Values")


########################### Section B ###################################
print("Boxcox transformation R-Squared")

# latitude transformation
t_boxcox_lat <- boxcox(fit_lat) #after analysis, lambda equal 2 is best
transformed_lat <- ((m_data[,117] ^ 2) - 1) / 2
m_boxcox_lat_data <- cbind(m_data[,c(-117,-118)], transformed_lat)
fit_lat_boxCox <- lm(formula = transformed_lat ~ ., data = m_boxcox_lat_data )  
print(var(fit_lat_boxCox$fitted.values)/var(transformed_lat))

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
plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))


# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 0)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))


########################### Section D/ Lasso regularization ###################################
print("Lasso R-Squared")

# latitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,117], keep = TRUE, alpha = 1)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))
print("no of non zero coefficients")
print(fit$nzero[s_lambdaPos])

# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 1)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))
print("no of non zero coefficients")
print(fit$nzero[s_lambdaPos])

########################### Section E/ Elastic net ###################################
print("Elastic net R-Squared")

print("Alpha = 0.5")
# latitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,117], keep = TRUE, alpha = 0.5)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))
print("no of non zero coefficients")
print(fit$nzero[s_lambdaPos])


# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 0.5)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))

print("Alpha = 0.75")
# latitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,117], keep = TRUE, alpha = 0.75)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))


# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 0.75)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))


print("Alpha = 0.25")
# latitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,117], keep = TRUE, alpha = 0.25)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,117]))


# longitude regression
fit <- cv.glmnet(as.matrix(m_data[,c(-117,-118)]), m_data[,118], keep = TRUE, alpha = 0.25)
s_lambdaPos <- match(fit$lambda.min, fit$lambda)
plot(fit)
plot(fit$fit.preval[,s_lambdaPos], m_data[,118] - fit$fit.preval[,s_lambdaPos], ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fit.preval[,s_lambdaPos])/var(m_data[,118]))

