
m_distancesTest <- matrix(0,4,4)
m_distancesTest[1,] <- c(0,10,1,5)
m_distancesTest[2,] <- c(0,0,8,5)
m_distancesTest[3,] <- c(0,0,0,5)
mdsTest <- cmdscale(m_distancesTest, eig=TRUE)

mdsTest.data <- data.frame(X=mdsTest$points[,1],Y=mdsTest$points[,2])
ggplot(data=mdsTest.data, aes(x=X, y=Y, label=c("1","2","3","4"))) +
  geom_text() +
  theme_bw() +
  xlab("Dimension 1") +
  ylab("Dimension 2") +
  ggtitle("Distance Between Categories")