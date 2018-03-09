
m_data <- read.table("brunhild.txt", sep = '\t', header=TRUE)

# plot in original coordinates
fit <- lm(log(Sulfate) ~ log(Hours) , data=m_data)
plot(m_data)
lines(m_data$Hours, exp(fit$fitted.values))

plot(exp(fit$fitted.values), m_data$Sulfate - exp(fit$fitted.values)) 


# plot in log coordinates
plot(log(m_data))
abline(fit)

plot(fit$fitted.values, fit$residuals)

# R-Squared -> .98
#var(fit$fitted.values)/var(log(m_data$Sulfate))
