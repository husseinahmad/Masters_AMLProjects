library(ggplot2)

#m_data <- read.csv("cifar-10-batches-bin/allData.csv", header=TRUE)
v_labels <- m_data[,1]
m_features <- m_data[,-1]

m_meanImages <- matrix(0,10,3072)
m_distances <- matrix(0,10,10)

#calculating errors for 10 categories
for(i in 1:10)
{
  print(c("starting category numer", i))
  v_positions <-  v_labels == i - 1
  m_categoryFeatures <- m_features[v_positions,]
  #print()
  m_meanImages[i,] <- apply(m_categoryFeatures, c(2), mean)
}

print("Calculate Distance")
for(i in 1:9)
{
  for(j in (i+1):10)
  {
    print(c(i,"and",j))
    m_distances[i,j] <- sqrt(sum((m_meanImages[i,]  - m_meanImages[j,]) ^ 2))
  }
}

mds <- cmdscale(m_distances, eig=TRUE)
v_catnames <- read.table("cifar-10-batches-bin/batches.meta.txt")
mds.data <- data.frame(X=mds$points[,1],Y=mds$points[,2])
ggplot(data=mds.data, aes(x=X, y=Y, label=as.character(v_catnames[,]))) +
  geom_text() +
  theme_bw() +
  xlab("Dimension 1") +
  ylab("Dimension 2") +
  ggtitle("Distance Between Categories")

#barplot(v_errors, main="Reconstruction Error Per Category",ylab="Error", xlab="Categories", names.arg=v_catnames[,])