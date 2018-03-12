library(glmnet)

m_data <- read.table("abalone.txt", sep = ',', header=TRUE)

# converting rings to age
m_data[,9] = m_data[,9] + 1.5

# converting sex to 1,0,-1
v_males <- m_data[,c(1)] == "M" 
v_is <- m_data[,c(1)] == "I" 
v_females <- m_data[,c(1)] == "F" 
m_data$Sex <- as.numeric(as.character(m_data$Sex))
m_data[v_males,1] <- 1
m_data[v_females,1] <- -1
m_data[v_is,1] <- 0

# part a
fit <- lm(Rings ~ . , data=m_data[,-1])
plot(fit$fitted.values, fit$residuals, ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fitted.values)/var(m_data$Rings))

# part b
fit <- lm(Rings ~ . , data=m_data)
plot(fit$fitted.values, fit$residuals, ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fitted.values)/var(m_data$Rings))

# part c
fit <- lm(log(Rings) ~ . , data=m_data[,-1])
plot(fit$fitted.values, fit$residuals, ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fitted.values)/var(log(m_data$Rings)))

# part d
fit <- lm(log(Rings) ~ . , data=m_data)
plot(fit$fitted.values, fit$residuals, ylab = "Residuals", xlab = "Fitted Values")
print(var(fit$fitted.values)/var(log(m_data$Rings)))


#part f.a
fit <- cv.glmnet(as.matrix(m_data[,c(-1,-9)]), m_data[,9], keep = TRUE)
#doesn't provide cross validated error
#fit <- glmnet(as.matrix(m_data[,c(-1,-9)]), m_data[,9], alpha = 0)
# plotting residual against fitted values for comparison, using average lambda
plot(fit$fit.preval[,83], m_data[,9] - fit$fit.preval[,83], ylab = "Residuals", xlab = "Fitted Values")
# plotting lambda against cross validated error
plot(fit$lambda, fit$cvm)
print(var(fit$fit.preval[,83])/var(m_data$Rings))

#part f.b
fit <- cv.glmnet(as.matrix(m_data[,c(-9)]), m_data[,9], keep = TRUE)
#doesn't provide cross validated error
#fit <- glmnet(as.matrix(m_data[,c(-1,-9)]), m_data[,9], alpha = 0)
# plotting residual against fitted values for comparison, using average lambda
plot(fit$fit.preval[,83], m_data[,9] - fit$fit.preval[,83], ylab = "Residuals", xlab = "Fitted Values")
# plotting lambda against cross validated error
plot(fit$lambda, fit$cvm)
print(var(fit$fit.preval[,83])/var(m_data$Rings))

#part f.c
fit <- cv.glmnet(as.matrix(m_data[,c(-1,-9)]), log(m_data[,9]), keep = TRUE)
#doesn't provide cross validated error
#fit <- glmnet(as.matrix(m_data[,c(-1,-9)]), m_data[,9], alpha = 0)
# plotting residual against fitted values for comparison, using average lambda
plot(fit$fit.preval[,83], log(m_data[,9]) - fit$fit.preval[,83])
# plotting lambda against cross validated error
plot(fit$lambda, fit$cvm)
print(var(fit$fit.preval[,83])/var(log(m_data$Rings)), ylab = "Residuals", xlab = "Fitted Values")

#part f.d
fit <- cv.glmnet(as.matrix(m_data[,c(-9)]), log(m_data[,9]), keep = TRUE)
#doesn't provide cross validated error
#fit <- glmnet(as.matrix(m_data[,c(-1,-9)]), m_data[,9], alpha = 0)
# plotting residual against fitted values for comparison, using average lambda
plot(fit$fit.preval[,83], log(m_data[,9]) - fit$fit.preval[,83], ylab = "Residuals", xlab = "Fitted Values")
# plotting lambda against cross validated error
plot(fit$lambda, fit$cvm)

print(var(fit$fit.preval[,83])/var(log(m_data$Rings)))