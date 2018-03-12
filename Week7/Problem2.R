m_data <- read.table("Physical.txt", sep = '\t', header=TRUE)

#3.1  Mass against all
fit <- lm(Mass ~ . , data=m_data)
print(var(fit$fitted.values)/var(m_data$Mass))  #R-Squared for sanity check
plot(fit$fitted.values, fit$residuals, ylab = "Residuals", xlab = "Fitted Values")

# 3.2 Mass cube root against all
fit2 <- lm(Mass ^ (1/3) ~ . , data=m_data)
print(var(fit2$fitted.values)/var(m_data$Mass ^ (1/3)))  #R-Squared for sanity check

# 3.2.a Plot in Cube root coordinates
plot(fit2$fitted.values, fit2$residuals, ylab = "Residuals", xlab = "Fitted Values")

#m_data <- cbind(m_data[,-1], m_data[1] ^ (1/3)) #not needed

# 3.2.b Plot in original  coordinates
plot(fit2$fitted.values ^ 3, m_data$Mass -  (fit2$fitted.values ^ 3), ylab = "Residuals", xlab = "Fitted Values")