library(ggplot2)

# preprocessed data, 1 labels then 3072 features in one line
m_data <- read.csv("cifar-10-batches-bin/allData.csv", header=TRUE)
v_labels <- m_data[,1]
m_features <- m_data[,-1]

m_distances <- matrix(0,10,10)
pcas <- vector("list", 10)
l_m_categoriesData <- vector("list", 10)

#calculating PCA for 10 categories, automatically calculates mean
for(i in 1:10)
{
  print(c("starting category numer", i))
  v_positions <-  v_labels == i - 1 # labels starts from 0
  l_m_categoriesData[[i]] <- m_features[v_positions,]
  temp <- princomp(l_m_categoriesData[[i]]) 
  #set all transformed left out(after 20), and eigen vectors, to 0
  temp$scores[,21:3072] <- 0 
  temp$loadings[,21:3072] <- 0 
  pcas[[i]] <- temp
}

print("Calculate Distance")
for(i in 1:9)
{
  for(j in (i+1):10)
  {
    print(c(i,"and",j))
    # on the transformed i data, pcas[[i]]$scores, use j eigen vectors, pcas[[j]]$loadings and i mean
    #irestoration <- t(pcas[[j]]$loadings %*% t(pcas[[i]]$scores) + pcas[[i]]$center)
    #jrestoration <- t(pcas[[i]]$loadings %*% t(pcas[[j]]$scores) + pcas[[j]]$center)
    # use j's eigenv vecs (PC) to transform/reconstruct i along with i's mean, same for j
    irestoration <- t(pcas[[j]]$loadings %*% t((as.matrix(l_m_categoriesData[[i]]) - pcas[[i]]$center) %*% pcas[[j]]$loadings)) + pcas[[i]]$center
    jrestoration <- t(pcas[[i]]$loadings %*% t((as.matrix(l_m_categoriesData[[j]]) - pcas[[j]]$center) %*% pcas[[i]]$loadings)) + pcas[[j]]$center
    iError <- mean(sqrt(apply((l_m_categoriesData[[i]] - irestoration) ^ 2, c(1), sum)))
    jError <- mean(sqrt(apply((l_m_categoriesData[[j]] - jrestoration) ^ 2, c(1), sum)))
    m_distances[i,j] <- (iError + jError) / 2
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

