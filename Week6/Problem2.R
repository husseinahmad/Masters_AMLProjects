library(klaR)
library(caret)
library(randomForest)

# reads all the folders/files in list of lists where each list represents a category
ReadData = function()
{
    v_folderNames <- list.dirs(path = "./HMP_Dataset", full.names = TRUE, recursive = FALSE)
    s_totalReadFiles <- 0
    v_catNames <- vector("character", 14)
    category_number <- 0
    m_data <- vector("list", 14)
    for (folder in v_folderNames)
    {
      
      # if not a model as I a mnot sure if Model folders are replicates
      if(grepl("MODEL", folder) == 0)
      {
        category_number <- category_number + 1
        v_catNames[category_number] <- basename(folder)  #folder name to be used as label
        sample_number <- 0
        v_files <- list.files(path = folder, full.names = TRUE)
        v_tempList <- list()
        # go over the files to extract their data
        for(file in v_files)
        {
          sample_number <- sample_number + 1
          m_sample <- read.table(file, sep = ' ', header=FALSE)
          v_tempList[[sample_number]] <- m_sample
          s_totalReadFiles <- s_totalReadFiles + 1
        }
        m_data[[category_number]] <- v_tempList
      }
    }
    
    names(m_data) <- v_catNames
    print(paste("Total read files is :", s_totalReadFiles))
    return (m_data)
}


#Quantize one list(category) to a vector of s_signalSize * 3 for each sample in that category
QuantizeList = function(l_catActivities, s_signalSize)
{
  m_quantizedData <- matrix(0, nrow = 10000, ncol = s_signalSize * 3) # initial and will be truncated later
  s_totalRows <- 1
  for(i in 1:length(l_catActivities) )
  {
    currentPos <- 0
    while( (currentPos + s_signalSize) <= nrow(l_catActivities[[i]]))
    {
      m_currentSignal <- l_catActivities[[i]][(currentPos+1):(currentPos+s_signalSize),]
      v_quantizedSignal <- as.vector(unlist(m_currentSignal)) #embed into one row
      m_quantizedData[s_totalRows,] <- v_quantizedSignal
      s_totalRows <- s_totalRows + 1
      currentPos <- currentPos + s_signalSize
    }
  }
  return (m_quantizedData[1:s_totalRows - 1,])
}

QuantizeSample = function(m_activity, s_signalSize)
{
  currentPos <- 0
  currentRow <- 1
  m_quantizedSample <- matrix(0,nrow = nrow(m_activity) / s_signalSize, ncol = s_signalSize * 3)
  while( (currentPos + s_signalSize) <= nrow(m_activity))
  {
    m_currentSignal <- m_activity[(currentPos+1):(currentPos+s_signalSize),]
    v_quantizedSignal <- as.vector(unlist(m_currentSignal)) #embed into one row
    m_quantizedSample[currentRow,] <- v_quantizedSignal
    currentRow <- currentRow + 1
    currentPos <- currentPos + s_signalSize
  }
  return(m_quantizedSample)
}

PlotKmeansError = function(m_quantizedData)
{
  #model <- kmeans ( m_quantizedData , 100 )
  v_errorClusters <- 0
  for(i in seq(50, 1000, by = 50))
      v_errorClusters[(i / 50)] <- sum(kmeans(m_quantizedData, centers=i)$withinss)
  
  plot(seq(50, 1000, by = 50), v_errorClusters, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
}

#build a histogram of number of clusters for a quantized sample
#sample here represents a file, so it's a set of rows each of the quantization size * 3
#same row size for each sample is each center's size
FeaturizeSample = function(model, m_quantizedSample)
{
  v_FeatureHist <- vector(mode = "numeric", length = nrow(model$centers))
  for(m in 1:nrow(m_quantizedSample))
  {
    v_distances <- apply(model$centers, c(1), function(x)sqrt(sum((m_quantizedSample[m,] - x) ^ 2)))
    s_minIndex <- which.min(v_distances)
    v_FeatureHist[s_minIndex] <- v_FeatureHist[s_minIndex] + 1
  }
  
  return(v_FeatureHist)
}

FeaturizeData = function(s_centersCount, model, m_data, s_signalHeight)
{
  m_classificationData <- matrix(0, 0, s_centersCount)
  v_labels <- vector(mode = "numeric", length = 0)
  for(s_catIndex in 1:14)
  {
    print(paste("Featurizing category ", s_catIndex))
    m_catData <- matrix(0, length(m_data[[s_catIndex]]), s_centersCount)
    for(s_actIndex in 1:length(m_data[[s_catIndex]]))
    { 
      m_quantizedSample <- QuantizeSample(m_activity = m_data[[s_catIndex]][[s_actIndex]], s_signalSize = s_signalHeight)
      v_sampleFeatures <- FeaturizeSample(model, m_quantizedSample)
      m_catData[s_actIndex,] <- v_sampleFeatures
    }
    
    #append this category samples to all
    m_classificationData <- rbind(m_classificationData, m_catData) 
    #append this category labels to all
    v_labels <- c(v_labels, rep(s_catIndex, length(m_data[[s_catIndex]])))
  }
  
  return (list(v_labels = v_labels, m_classificationData = m_classificationData))
}

#split data ccording to given percentage, returns same structure as input
#list (size 14) of list (size based on splitting) of data frames (where each one represents a whole file)
SplitData = function(m_data, trainPrc)
{
  l_training = vector("list", length = 14)
  l_testing = vector("list", length = 14)
  for(s_catIndex in 1:14)
  {
    l_currentCatTrain <- list()
    l_currentCatTest <- list()
    for(s_actIndex in 1:length(m_data[[s_catIndex]]))
    {
      if(runif(1,0,1) < trainPrc)
      {
        l_currentCatTrain[[length(l_currentCatTrain) + 1]] <- m_data[[s_catIndex]][[s_actIndex]]
      }
      else
      {
        l_currentCatTest[[length(l_currentCatTest) + 1]] <- m_data[[s_catIndex]][[s_actIndex]]
      }
    }
    
    #if no data passed to testing category activities, force sharing one instance
    if(length(l_currentCatTest) == 0) 
    {
      l_currentCatTest <- l_currentCatTrain[1]
      l_currentCatTrain <- l_currentCatTrain[-1]
    }
    
    l_training[[s_catIndex]] <- l_currentCatTrain
    l_testing[[s_catIndex]] <- l_currentCatTest
    print(paste(length(l_currentCatTrain), " ",length(l_currentCatTest)))
  }
  
  return (list(train = l_training, test = l_testing))
}

print("Reading Data")
m_data <- ReadData()
m_splitData <- SplitData(m_data, 0.8)
m_data <- m_splitData$train
m_test <- m_splitData$test # untouched data

s_signalHeight <- 25 #25/300 got 80%
s_centersCount <- 300 #also represents number of features as it's a histogram
m_quantizedData <- matrix(0,0,s_signalHeight * 3)
for(i in 1:14)
{
  print(paste("Quantizing Category", i))
  m_tempMatrix <- QuantizeList(m_data[[i]], s_signalHeight)
  m_quantizedData <- rbind(m_quantizedData, m_tempMatrix)
}

print("Building clusters for quantized data")
model <- kmeans(m_quantizedData, centers=s_centersCount)

print("Featurizing training activities(files) based on clusters by building histogram")
m_features <- FeaturizeData(s_centersCount, model, m_data, s_signalHeight)
v_trainingLabels <- m_features$v_labels
m_trainingFeatures <- m_features$m_classificationData

print("Building classification model")
classificationModel <- randomForest(x = m_trainingFeatures, y = as.factor(v_trainingLabels))

print("Featurizing testing activities(files) based on clusters by building histogram")
m_features <- FeaturizeData(s_centersCount, model, m_test, s_signalHeight)
v_testingLabels <- m_features$v_labels
m_testingFeatures <- m_features$m_classificationData

print("Performing Prediction")
v_predictionResults <- predict(classificationModel, m_testingFeatures)
s_accuracy <- sum(v_testingLabels == v_predictionResults) / length(v_predictionResults)
print(s_accuracy)


