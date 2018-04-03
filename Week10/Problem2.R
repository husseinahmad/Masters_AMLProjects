library("jpeg")
img1 <- readJPEG("TestImages\\Image1.jpg", native = FALSE)
s_width <- dim(img1)[1]
s_height <- dim(img1)[2]
s_depth <- dim(img1)[3]
s_clusters <- 10
dim(img1) <- c(s_width * s_height, s_depth)
set.seed(1)
model_kmeans <- kmeans ( img1 , centers = s_clusters )
img1_new <- matrix(0, s_width * s_height, s_depth )

v_pis <- vector("numeric", s_clusters)
print("Running K means")
m_means <- model_kmeans$centers

for(m in 1:nrow(img1))
{
  #if(m %% 10000 == 0)
  #  print(m)
  v_distances <- apply(m_means, c(1), function(x)sqrt(sum((img1[m,] - x) ^ 2)))
  s_minIndex <- which.min(v_distances)
  v_pis[s_minIndex] <- v_pis[s_minIndex] + 1
  img1_new[m,] <- m_means[s_minIndex,]
}
# reconstruct the image
dim(img1_new) <- c(s_width, s_height, s_depth)
writeJPEG(img1_new, "TestImages\\Image1_kmeans.jpg") 

v_pis <- v_pis / sum(v_pis)

print("K means pis")
print(v_pis)
print(m_means)

# apply EM algorith
m_deltas <- matrix(0, s_width * s_height, s_clusters)

for(iteration in 1:10)
{
  print(paste("iteration", iteration))
  # E Step
  print("E Step")
  for(i in 1:nrow(img1))
  {
    #if(i %% 10000 == 0)
    #  print(i)
    for(j in 1:s_clusters)
    {
      m_deltas[i,j] <- exp(-0.5 * (t(img1[i,]) %*% img1[i,])) * v_pis[j]
    }
    
    m_deltas[i,] <- m_deltas[i,] / sum(m_deltas[i,])
  }
  
  # M Step
  print("M Step")
  m_means <- t(img1) %*% m_deltas
  normalizer <- apply(m_deltas, c(2), sum)
  m_means <- m_means / normalizer
  
  v_pis <- normalizer / nrow(img1)
  print(v_pis)
  print(m_means)
}

# reconstruct the image after EM
img1_new <- matrix(0, s_width * s_height, s_depth )
m_means <- t(m_means)
for(m in 1:nrow(img1))
{
  #if(m %% 10000 == 0)
  #  print(m)
  v_distances <- apply(t(m_means), c(1), function(x)sqrt(sum((img1[m,] - x) ^ 2)))
  s_minIndex <- which.min(v_distances)
  v_pis[s_minIndex] <- v_pis[s_minIndex] + 1
  img1_new[m,] <- m_means[s_minIndex,]
}
dim(img1_new) <- c(s_width, s_height, s_depth)
writeJPEG(img1_new, "TestImages\\Image1_EM.jpg") 