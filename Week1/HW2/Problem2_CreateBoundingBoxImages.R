
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
  
    if(  last_row - first_row != 19 ) # image spaces are bigger than 8 rows we want to remove
    {
        if(last_row < 20)
          last_row = 20
        first_row = last_row - 19     #move first row down or up 
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
  
  if(  last_column - first_column != 19 ) 
  {
    if(last_column < 20)
      last_column = 20
    first_column = last_column - 19    
  }
  
  m_cropped = m_matrix[,first_column:last_column]
  return (m_cropped)
}

PreProcessImage = function(t_inputFile, t_outputFile) {
m_testingData <- read.csv(t_inputFile, header=TRUE)
v_testingLabels <- m_testingData[,785]
m_testingFeatures <- m_testingData[,1:784]
m_croppedTestingFeatures <- matrix(0, length(v_testingLabels), 400) 
for(i in 1:length(v_testingLabels)){
  m_testingFeatures[i,] = apply_Thresholding(m_testingFeatures[i,])
  m_tempImage <- matrix(m_testingFeatures[i,],28,28)
  m_tempImage <- apply_HBounding(m_tempImage)
  m_tempImage <- apply_VBounding(m_tempImage)
  m_croppedTestingFeatures[i,] <- unlist(m_tempImage)
  if(i %% 10 == 0)
    print(i)
}
m_croppedTestingFeatures <- cbind(m_croppedTestingFeatures, v_testingLabels)
write.csv(m_croppedTestingFeatures, file=t_outputFile, row.names=FALSE, col.names = FALSE)
}

#PreProcessImage('mnist_test.csv', "mnist_test_bounded.csv")
PreProcessImage('mnist_train.csv', "mnist_train_bounded.csv")
