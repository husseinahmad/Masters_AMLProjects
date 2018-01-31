library(h2o)

CalculateAccuracy = function(v_results, v_labels){
  v_equal <- as.vector(v_results) == as.vector(v_labels)
  return(sum(v_equal) / length(v_equal))
}

ApplyRFClassifier = function(trainFileName, testFileName, imageSize, treeDepth, noOfTrees) {
  
  m_trainingData <- read.csv(trainFileName, header=TRUE)
  m_trainingFeatures <- m_trainingData[,-c(imageSize+1)]
  v_trainingLabels <- as.factor(m_trainingData[,c(imageSize+1)])
  
  m_testingData <- read.csv(testFileName, header=TRUE)
  m_testingFeatures <- m_testingData[,-c(imageSize+1)]
  v_testingLabels <- as.factor(m_testingData[,c(imageSize+1)])
  
  model <- h2o.randomForest(y = c(imageSize+ 1), training_frame = as.h2o(cbind(m_trainingFeatures,v_trainingLabels)), ntrees = noOfTrees, max_depth = treeDepth)
  h2o_results <- h2o.predict(model, as.h2o(m_testingFeatures))
  v_results <- as.data.frame(h2o_results)
  print(confusionMatrix(data=v_results[,1], v_testingLabels))
  
}

h2o.init()
#ApplyRFClassifier("mnist_train.csv", "mnist_test.csv", 28 * 28, 4, 10)
#ApplyRFClassifier("mnist_train.csv", "mnist_test.csv", 28 * 28, 4, 20)
#ApplyRFClassifier("mnist_train.csv", "mnist_test.csv", 28 * 28, 4, 30)
#ApplyRFClassifier("mnist_train.csv", "mnist_test.csv", 28 * 28, 8, 10)
#ApplyRFClassifier("mnist_train.csv", "mnist_test.csv", 28 * 28, 8, 20)
#ApplyRFClassifier("mnist_train.csv", "mnist_test.csv", 28 * 28, 8, 30)
#ApplyRFClassifier("mnist_train.csv", "mnist_test.csv", 28 * 28, 16, 10)
#ApplyRFClassifier("mnist_train.csv", "mnist_test.csv", 28 * 28, 16, 20)
#ApplyRFClassifier("mnist_train.csv", "mnist_test.csv", 28 * 28, 16, 30)

#ApplyRFClassifier("mnist_train_stretched.csv", "mnist_test_stretched.csv", 20 * 20, 4, 10)
#ApplyRFClassifier("mnist_train_stretched.csv", "mnist_test_stretched.csv", 20 * 20, 4, 20)
#ApplyRFClassifier("mnist_train_stretched.csv", "mnist_test_stretched.csv", 20 * 20, 4, 30)
#ApplyRFClassifier("mnist_train_stretched.csv", "mnist_test_stretched.csv", 20 * 20, 8, 10)
#ApplyRFClassifier("mnist_train_stretched.csv", "mnist_test_stretched.csv", 20 * 20, 8, 20)
#ApplyRFClassifier("mnist_train_stretched.csv", "mnist_test_stretched.csv", 20 * 20, 8, 30)
#ApplyRFClassifier("mnist_train_stretched.csv", "mnist_test_stretched.csv", 20 * 20, 16, 10)
#ApplyRFClassifier("mnist_train_stretched.csv", "mnist_test_stretched.csv", 20 * 20, 16, 20)
ApplyRFClassifier("mnist_train_stretched.csv", "mnist_test_stretched.csv", 20 * 20, 16, 30)