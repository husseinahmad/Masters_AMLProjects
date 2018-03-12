
m_data <- read.table("brunhild.txt", sep = '\t', header=TRUE)

#fit a linear regression with the logs
fit <- lm(log(Sulfate) ~ log(Hours) , data=m_data)

# plot in log coordinates
plot(log(m_data), ylab="Log(Sulfate)", xlab = "Log(Hours)")
abline(fit)
# plot residuals against fitted values in log coordinates
plot(fit$fitted.values, fit$residuals, ylab = "Residuals", xlab = "Fitted Values")


# plot in original coordinates
plot(m_data)
lines(m_data$Hours, exp(fit$fitted.values))
# plot residuals against fitted values in original coordinates
plot(exp(fit$fitted.values), m_data$Sulfate - exp(fit$fitted.values), ylab = "Residuals", xlab = "Fitted Values") 


# R-Squared -> .98
print(var(fit$fitted.values)/var(log(m_data$Sulfate)))
