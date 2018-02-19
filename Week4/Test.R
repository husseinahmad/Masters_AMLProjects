
#m_matrix <- matrix(1:30, nrow = 10, ncol = 3, byrow = FALSE)

 m_matrix <- matrix(runif(30,1,30), nrow = 10, ncol = 3, byrow = FALSE)
# pca <- prcomp(m_matrix)
# p <- pca$x #x or scores
# p[,3] <- 0
# restoration <- t(pca$rotation %*% t(p) + pca$center) #pca
# difference <- (restoration - m_matrix) 
# difference2 <- difference ^ 2
# 
# errorSum <- apply(difference2, c(1), sum)
# print(mean(errorSum))
# print(pca$sdev[3] ^ 2)

# method 2

pca2 <- princomp(m_matrix) #princomp(m_matrix) #
transformed <- pca2$scores #pca2$x
#p[,3] <- 0
eigenVecs <- pca2$loadings #pca2$rotation #
eigenVecs[,3] <- 0
restoration <- t((eigenVecs %*% t(transformed)) + pca2$center) #pca
difference <- (restoration - m_matrix) 
difference2 <- difference ^ 2

errorSum <- apply(difference2, c(1), sum)
print(c("Restoration error: ", mean(errorSum)))
print(c("Variance Error: ",pca2$sdev[3] ^ 2))


#error1_1 <- sum(sapply(pca$x[,3], function(x) x^2)) / 10
#error1_2 <- pca$sdev


#error2_1 <- sum(sapply(pca2$scores[,3], function(x) x^2)) / 10
#error2_2 <- pca$sdev

#m_distances <- cbind(c(0,5,8), c(5,0,13), c(8,13,0))


#working conversion -> identical to pca$x
#t(t(m_matrix) - pca$center) %*% pca$rotation

# restoration
# t(pca$rotation %*% t(p) + pca$center)