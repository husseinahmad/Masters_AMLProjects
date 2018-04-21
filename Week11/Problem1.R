load_image_file <- function(filename) {
  ret = list()
  f = file(filename,'rb')
  readBin(f,'integer',n=1,size=4,endian='big')
  ret$n = readBin(f,'integer',n=1,size=4,endian='big')
  nrow = readBin(f,'integer',n=1,size=4,endian='big')
  ncol = readBin(f,'integer',n=1,size=4,endian='big')
  x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
  ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
  ret$x = ret$x / 255 # scale to be from 0 to 1
  #ret$x[ret$x < 128] = -1
  #ret$x[ret$x > 127] = 1
  ret$x[ret$x < 0.5] = -1
  ret$x[ret$x > 0.5] = 1
  close(f)
  ret
}

show_digit <- function(arr784, col=gray(12:1/12), ...) {
  #image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

show_digit_2d <- function(m_img, col=gray(12:1/12), ...) {
  #image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
  image(m_img, col=col, ...)
}

convert_2d <- function(arr784) {
  return(matrix(arr784, nrow=28, byrow = F)[,28:1])
  
}

apply_noise <- function(images){
  m_noise <- read.table("SupplementaryAndSampleData\\SupplementaryAndSampleData\\NoiseCoordinates.csv", sep = ',', header=TRUE)
  m_noise <- m_noise[,-1]
  results <- list(20)
  for(i in 1:20){
    m_noiseLocs <- t(m_noise[((2*i)-1):(2*i),])
    m_noiseLocs <- m_noiseLocs + 1
    #m_noiseLocs[,1] <- 29 - m_noiseLocs[,1]
    temp_img <- convert_2d(images[i,])
    temp_img <- t(temp_img[,28:1])
    temp_img[m_noiseLocs] <- sapply(temp_img[m_noiseLocs], function(x) if(x == -1) {x <- 1} else { x <- -1}) 
    results[[i]] <- temp_img
  }
  
  return(results)
}

readInitialPIs <- function(){
  m_pis <- read.table("SupplementaryAndSampleData\\SupplementaryAndSampleData\\InitialParametersModel.csv", sep = ',', header=FALSE)
}

readUpdateOrder <- function(){
  m_updateOrder <- read.table("SupplementaryAndSampleData\\SupplementaryAndSampleData\\UpdateOrderCoordinates.csv", sep = ',', header=TRUE)
  m_updateOrder <- m_updateOrder[,-1]
  results <- list(20)
  for(i in 1:20){
    m_imageUpdateOrder <- t(m_updateOrder[((2*i)-1):(2*i),])
    m_imageUpdateOrder <- m_imageUpdateOrder + 1
    #m_imageUpdateOrder[,1] <- 29 - m_imageUpdateOrder[,1]
    results[[i]] <- m_imageUpdateOrder
  }
  
  return(results)
}

calculateHiddenSum <- function(m_pis, s_xPos, s_yPos, s_hiddenTheta){
  s_hiddenSum <- 0
  #left node
  if(s_xPos > 1)
    s_hiddenSum <- s_hiddenSum + (s_hiddenTheta * ( (2 * m_pis[s_xPos - 1, s_yPos]) - 1 ) )
  #right node
  if(s_xPos < 28)
    s_hiddenSum <- s_hiddenSum + (s_hiddenTheta * ( (2 * m_pis[s_xPos + 1, s_yPos]) - 1 ) )
  #up node
  if(s_yPos > 1)
    s_hiddenSum <- s_hiddenSum + (s_hiddenTheta * ( (2 * m_pis[s_xPos, s_yPos - 1]) - 1 ) )
  #down node
  if(s_yPos < 28)
    s_hiddenSum <- s_hiddenSum + (s_hiddenTheta * ( (2 * m_pis[s_xPos, s_yPos + 1]) - 1 ) )
  
  return(s_hiddenSum)
}

calculateXSum <- function(m_image, s_xPos, s_yPos, s_xTheta){
  return(m_image[s_xPos, s_yPos] * s_xTheta)
}

calculateEPHX_HiddenSum <- function(m_pis, s_xPos, s_yPos, s_hiddenTheta){
  s_pixelEQ <- (2 * m_pis[s_xPos, s_yPos]) - 1 
  s_hiddenSum <- 0
  #left node
  if(s_xPos > 1)
    s_hiddenSum <- s_hiddenSum + (s_hiddenTheta * ( (2 * m_pis[s_xPos - 1, s_yPos]) - 1 ) * s_pixelEQ)
  #right node
  if(s_xPos < 28)
    s_hiddenSum <- s_hiddenSum + (s_hiddenTheta * ( (2 * m_pis[s_xPos + 1, s_yPos]) - 1 ) * s_pixelEQ)
  #up node
  if(s_yPos > 1)
    s_hiddenSum <- s_hiddenSum + (s_hiddenTheta * ( (2 * m_pis[s_xPos, s_yPos - 1]) - 1 ) * s_pixelEQ)
  #down node
  if(s_yPos < 28)
    s_hiddenSum <- s_hiddenSum + (s_hiddenTheta * ( (2 * m_pis[s_xPos, s_yPos + 1]) - 1 ) * s_pixelEQ)
  
  return(s_hiddenSum)
}

calculateEPHX_XSum <- function(m_pis, m_image, s_xPos, s_yPos, s_xTheta){
  s_pixelEQ <- (2 * m_pis[s_xPos, s_yPos]) - 1 
  return(m_image[s_xPos, s_yPos] * s_xTheta * s_pixelEQ)
}

updatePIs <- function(m_image, m_updateOrder, m_pis){
  s_hiddenTheta <- 0.8
  s_xTheta <- 2
  
  # will update pis in position
  for(i in 1:nrow(m_updateOrder)) #784
  {
    s_xPos <- m_updateOrder[i,1]
    s_yPos <- m_updateOrder[i,2]
    
    s_hiddenSumPos <- calculateHiddenSum(m_pis, s_xPos, s_yPos, s_hiddenTheta)
    s_hiddenSumNeg <- calculateHiddenSum(m_pis, s_xPos, s_yPos, -1 * s_hiddenTheta)
    s_xSumPos <- calculateXSum(m_image, s_xPos, s_yPos, s_xTheta)
    s_xSumNeg <- calculateXSum(m_image, s_xPos, s_yPos, -1 * s_xTheta)
    
    m_pis[s_xPos, s_yPos] <- exp(s_hiddenSumPos + s_xSumPos) / (exp(s_hiddenSumPos + s_xSumPos) + exp(s_hiddenSumNeg + s_xSumNeg))
  }
  
  return(m_pis)
}

data = load_image_file("..\\..\\Datasets\\MNSIT\\train-images-idx3-ubyte")
images <- data$x[1:20,]
#show_digit(images[1,])
m_noisedImages <- apply_noise(images)
#show_digit_2d(convert_2d(images[1,]))
#show_digit_2d(m_noisedImages[[1]])
m_pis <- readInitialPIs()
l_updateOrders <- readUpdateOrder()

# intialize pis for all images
l_pis <- list(20)
for(i in 1:20)
{
  l_pis[[i]] <- m_pis
}

v_energy <- vector("numeric", 20)
s_epsilon <- 10 ^ -10
s_hiddenTheta <- 0.8
s_xTheta <- 2
m_variationalEnergy <- matrix(0, 20, 10)


for(iteration in 1:10)
{
  print(paste("Iteration: ", iteration))
  for(image_index in 1:20)
  {
    #calculate Q
    s_eqLogQ <- sum(sapply(unlist(l_pis[[image_index]]), function(x) (x*log(x + s_epsilon)) + ((1-x)*log((1-x) + s_epsilon))  ))
    #print(s_eqLogQ)
    s_eqLogPHX <- 0
    for(r in 1:28){
      for(c in 1:28){
        s_eqLogPHX <- s_eqLogPHX + calculateEPHX_HiddenSum(l_pis[[image_index]], r, c, s_hiddenTheta) + calculateEPHX_XSum (l_pis[[image_index]], m_noisedImages[[image_index]], r, c, s_xTheta)
      }
    }
    
    m_variationalEnergy[image_index, iteration] <- s_eqLogQ - s_eqLogPHX
  
    # update pis
    l_pis[[image_index]] <- updatePIs(m_noisedImages[[image_index]], l_updateOrders[[image_index]], l_pis[[image_index]])
  }
}

write.csv(m_variationalEnergy, file="Energy.csv", row.names=FALSE)
print(m_variationalEnergy)


m_results <- l_pis[[11]]
for(i in 12:20)
{
  m_results <- cbind(m_results, l_pis[[i]])
}

m_results[m_results <= 0.5] <- 0
m_results[m_results > 0.5] <- 1

write.csv(m_results, file="denoised.csv", row.names=FALSE, col.names = FALSE)

show_digit_2d(as.matrix(m_results[,1:28]))
