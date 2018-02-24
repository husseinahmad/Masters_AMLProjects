library("cluster")
#library("ape")
m_data <- read.table("EuropeanJobs.txt", sep = '\t', header=TRUE)

v_names <- m_data[,1]
m_features <- m_data[,-1]
m_scaledFeats <- scale(m_features)

##### Part1
# d <- dist(m_scaledFeats, method = "euclidean") # distance matrix
# fit <- hclust(d, method="average") 
# fit$labels <- v_names
# plot(fit, hang = -1) # display dendogram
##plot(as.phylo(hclust(dist(mtcars))),type="fan")
#######
#groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
#rect.hclust(fit, k=5, border="red")

########### KMeans with error chart for different K

 wss <- (nrow(m_scaledFeats)-1)*sum(apply(m_scaledFeats,2,var))
for (i in 2:15) 
  wss[i] <- sum(kmeans(m_scaledFeats, 
                                      centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

## kmeans with plot for clusters 
#nclus <- 6
#model <- kmeans ( m_scaledFeats , nclus )
#colr <- c( "red" , "green", "blue", "yellow")
#clusplot ( m_scaledFeats , model$cluster , color=TRUE, shade=TRUE, labels=0, lines=0)