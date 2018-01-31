
# load label files
apply_Thresholding = function(m_dataFrame) {
  m_above <- which(m_dataFrame > 0)
  m_dataFrame[m_above] <- 1
  return (m_dataFrame)
}

apply_HBounding = function(m_matrix) {
  
    first_row = 1
    
    for(i in 1:28){
      if(Reduce("+", m_matrix[i,]) > 0)
      {
        first_row = i
        break
      }
    }
      
    last_row = 28
    for(i in 28:1){
      if(Reduce("+", m_matrix[i,]) > 0)
      {
        last_row = i
        break
      }
    }
  
    m_cropped = m_matrix[first_row:last_row ,]
    return (m_cropped)
}

apply_VBounding = function(m_matrix) {
  
  first_column = 1
  
  for(j in 1:28){
    if(Reduce("+", m_matrix[,j]) > 0)
    {
      first_column = j
      break
    }
  }
  
  last_column = 28
  for(j in 28:1){
    if(Reduce("+", m_matrix[,j]) > 0)
    {
      last_column = j
      break
    }
  }
  
  m_cropped = m_matrix[,first_column:last_column]
  return (m_cropped)
}

PreProcessImage = function(t_inputFile, t_outputFile) {
m_testingData <- read.csv(t_inputFile, header=TRUE)
v_testingLabels <- m_testingData[,785]
m_testingFeatures <- m_testingData[,1:784]
m_resizedTestingFeatures <- matrix(0, length(v_testingLabels), 400) 
for(i in 1:length(v_testingLabels)){
  m_testingFeatures[i,] <- apply_Thresholding(m_testingFeatures[i,])
  m_tempImage <- as.matrix(m_testingFeatures[i,],28,28)
  m_tempImage <- matrix(m_tempImage,28,28)
  m_tempImage <- apply_HBounding(m_tempImage)
  m_tempImage <- apply_VBounding(m_tempImage)
  # add scaling lines here
  img_tempImage <- as.cimg(m_tempImage)
  img_tempImage <- resize(img_tempImage, 20, 20)
  m_tempImage <- matrix(img_tempImage, 20, 20)
  m_resizedTestingFeatures[i,] <- unlist(m_tempImage)
}
m_resizedTestingFeatures <- cbind(m_resizedTestingFeatures, v_testingLabels)
write.csv(m_resizedTestingFeatures, file=t_outputFile, row.names=FALSE, col.names = FALSE)
}

PreProcessImage('mnist_test.csv', "mnist_test_stretched.csv")
PreProcessImage('mnist_train.csv', "mnist_train_stretched.csv")
