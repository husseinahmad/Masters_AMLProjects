load_image_file <- function(filename) {
  ret = list()
  f = file(filename,'rb')
  readBin(f,'integer',n=1,size=4,endian='big')
  ret$n = readBin(f,'integer',n=1,size=4,endian='big')
  nrow = readBin(f,'integer',n=1,size=4,endian='big')
  ncol = readBin(f,'integer',n=1,size=4,endian='big')
  x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
  ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
  #ret$x = ret$x / 255 # scalre to be from 0 to 1
  ret$x[ret$x < 128] = -1
  ret$x[ret$x > 127] = 1
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
  return(matrix(arr784, nrow=28)[,28:1])
  
}

apply_noise <- function(images){
  m_noise <- read.table("SupplementaryAndSampleData\\SupplementaryAndSampleData\\NoiseCoordinates.csv", sep = ',', header=TRUE)
  m_noise <- m_noise[,-1]
  results <- list(20)
  for(i in 1:20){
    m_noiseLocs <- t(m_noise[((2*i)-1):(2*i),])
    m_noiseLocs <- m_noiseLocs + 1
    temp_img <- convert_2d(images[i,])
    temp_img[m_noiseLocs] <- sapply(temp_img[m_noiseLocs], function(x) if(x == -1) {x <- 1} else { x <- -1}) 
    results[[i]] <- temp_img
  }
  
  return(results)
}


data = load_image_file("..\\..\\Datasets\\MNSIT\\train-images-idx3-ubyte")
images <- data$x[1:20,]
#show_digit(images[1,])
m_noisedImages <- apply_noise(images)
show_digit_2d(convert_2d(images[1,]))
show_digit_2d(m_noisedImages[[1]])