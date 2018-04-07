library("jpeg")
ptm <- proc.time()
s_imgNumber <- 2
s_clusters <- 50
img1 <- readJPEG(paste("TestImages\\Image", s_imgNumber, ".jpg", sep = ""), native = FALSE)
img1 <- img1 * 255
s_width <- dim(img1)[1]
s_height <- dim(img1)[2]
s_depth <- dim(img1)[3]
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
v_pis <- v_pis / sum(v_pis)
# reconstruct the image
dim(img1_new) <- c(s_width, s_height, s_depth)
img1_new <- img1_new / 255
writeJPEG(img1_new, paste("TestImages\\Image", s_imgNumber, "_kmeans.jpg", sep = "")) 



print("K means pis")
print(v_pis)
print(m_means)

# apply EM algorith
m_deltas <- matrix(0, s_width * s_height, s_clusters)

m_old_means <- m_means
for(iteration in 1:50)
{
  print(paste("iteration", iteration))
  # E Step
  print("E Step")
  
  # computePixelDeltas= function(pixel, m_means){
  #   m_diff <- t(apply(m_means, c(1), function(x) x - pixel))
  #   m_diff2 <- apply(m_diff, c(1), function(x)t(x) %*% x)
  #   m_diff2 <- m_diff2 - min(m_diff2)
  #   return(exp(-0.5 * m_diff2))
  # }
  # 
  # m_deltas <- t(apply(img1, c(1), computePixelDeltas, m_means))
  # m_deltas <- t(apply(m_deltas, c(1), function(x) x * v_pis))
  # m_deltas <- t(apply(m_deltas, c(1), function(x) x / sum(x)))
  
  #technique 2
  for(j in 1:s_clusters)
  {
    m_diff <- apply(img1, c(1), function(x) x - m_means[j,])
    m_deltas[,j] <- apply(m_diff, c(2), function(x) sum(x^2))
  }
  
  m_deltas <- t(apply(m_deltas, c(1), function(x) exp(-0.5 *(x - min(x)))))
  m_deltas <- t(apply(m_deltas, c(1), function(x) x * v_pis))
  m_deltas <- t(apply(m_deltas, c(1), function(x) x / sum(x)))
  
  # M Step
  print("M Step")
  m_means <- t(img1) %*% m_deltas
  normalizer <- apply(m_deltas, c(2), sum)
  m_means <- t(m_means)
  for(j in 1:s_clusters)
  {
    m_means[j,] <- m_means[j,] / normalizer[j]
  }
  
  v_pis <- normalizer / nrow(img1)
  
  print(v_pis)
  print(m_means)
  
  s_biggestDiff <- max(abs(m_means - m_old_means))
  print(s_biggestDiff)
  if(s_biggestDiff < exp(-7))
  {
    break
  }
  else
  {
    m_old_means <- m_means
  }
}

# reconstruct the image after EM
img1_new <- matrix(0, s_width * s_height, s_depth )
#m_means <- t(m_means)
for(m in 1:nrow(img1))
{
  #if(m %% 10000 == 0)
  #  print(m)
  v_distances <- apply(m_means, c(1), function(x)sqrt(sum((img1[m,] - x) ^ 2)))
  s_minIndex <- which.min(v_distances)
  img1_new[m,] <- m_means[s_minIndex,]
}
dim(img1_new) <- c(s_width, s_height, s_depth)
img1_new <- img1_new / 255
writeJPEG(img1_new, paste("TestImages\\Image", s_imgNumber, "_EM.jpg", sep = "")) 
print(proc.time() - ptm)
