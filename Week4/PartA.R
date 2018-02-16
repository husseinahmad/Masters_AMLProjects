m_data <- read.csv("cifar-10-batches-bin/allData.csv", header=TRUE)
v_labels <- m_data[,1]
m_features <- m_data[,-1]

v_errors <- vector("numeric", 10)
#calculating errors for 10 categories
for(i in 0:9)
{
  print(c("starting category numer", i + 1))
  v_positions <-  v_labels == i
  m_categoryFeatures <- m_features[v_positions,]
  pca <- princomp(m_categoryFeatures) 
  v_errors[i + 1] <- pca$sdev[21:3072]
}

v_catnames <- read.table("cifar-10-batches-bin/batches.meta.txt")
barplot(v_errors, main="Reconstruction Error Per Category", 
        ylab="Error", xlab="Categories", names.arg=v_catnames[,])