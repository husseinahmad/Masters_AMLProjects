library(klaR)
library(caret)
library(naivebayes)
library(quanteda)

CalculateAccuracy = function(v_results, v_labels){
  v_equal <- as.vector(v_results) == as.vector(v_labels)
  return(sum(v_equal) / length(v_equal))
}

ApplyNaiveClassifier = function(trainFileName, testFileName, imageSize, isGaussian = TRUE) {
  
m_trainingData <- read.csv(trainFileName, header=TRUE)
m_trainingFeatures <- m_trainingData[,-c(imageSize+1)]
v_trainingLabels <- as.factor(m_trainingData[,c(imageSize+1)])

m_testingData <- read.csv(testFileName, header=TRUE)
m_testingFeatures <- m_testingData[,-c(imageSize+1)]
v_testingLabels <- as.factor(m_testingData[,c(imageSize+1)])

# start training
if(isGaussian)
  model<-naive_bayes(m_trainingFeatures, v_trainingLabels)
else
{
  dfm_trainingset <- as.dfm(m_trainingFeatures)
  model <- textmodel_nb(dfm_trainingset, v_trainingLabels, distribution = "Bernoulli",  prior = "docfreq")
}
print("Training is finished")

# start evaluation
if(isGaussian)
{
  teclasses<-predict(model,newdata=m_testingFeatures)
  accuracy <- CalculateAccuracy(teclasses, v_testingLabels)
}
else
{
  teclasses<-predict(model,newdata=as.dfm(m_testingFeatures))
  accuracy <- CalculateAccuracy(teclasses$nb.predicted, v_testingLabels)
}
print("Evaluation is finished")

# calculating accuracy
#metrics <- confusionMatrix(data=teclasses, v_testingLabels)



if(isGaussian)
  print("Gaussian Metrics")
else
  print("Bernulli Metrics")


print(accuracy)

}

#ApplyNaiveClassifier("mnist_train.csv", "mnist_test.csv", 28 * 28, TRUE)
#ApplyNaiveClassifier("mnist_train.csv", "mnist_test.csv", 28 * 28, FALSE)
ApplyNaiveClassifier("mnist_train_stretched.csv", "mnist_test_stretched.csv", 20 * 20, TRUE)
ApplyNaiveClassifier("mnist_train_stretched.csv", "mnist_test_stretched.csv", 20 * 20, FALSE)