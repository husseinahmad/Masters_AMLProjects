library(glmnet)

m_data <- read.table("abalone.txt", sep = ',', header=TRUE)
v_males <- m_data[,c(1)] == "M" 
v_is <- m_data[,c(1)] == "I" 
v_females <- m_data[,c(1)] == "F" 

m_data$Sex <- as.numeric(as.character(m_data$Sex))
m_data[v_males,1] <- 1
m_data[v_females,1] <- -1
m_data[v_is,1] <- 0

fit <- cv.glmnet(as.matrix(m_data[,-9]), m_data[,9])
#glmnet(as.matrix(m_data[,-9]), m_data[,9], alpha = 0)
plot(fit)
#plot(fit$lambda, fit$cvm)
# fit <- lm(Mass ~ . , data=m_data)
# plot(fit$fitted.values, fit$residuals)

#fit <- lm(Mass ^ (1/3) ~ . , data=m_data)
#plot(fit$fitted.values, fit$residuals)

#m_data <- cbind(m_data[,-1], m_data[1] ^ (1/3))
#fit <- lm(Mass ~ . , data=m_data)
#plot(fit$fitted.values, fit$residuals)