source("StochasticSVM.R")
m_data1 <- read.csv("adult.data", header=FALSE)
m_data2 <- read.csv("adult.test", header=FALSE)

m_data <- rbind(m_data1, m_data2)
v_labels <- m_data[,c(15)]
v_labels <- as.numeric(v_labels)
#m_nLabels <- which(m_labels[m_abels == 0])
v_labels[v_labels == 2] <- -1
m_data <- m_data[,c(1,3,5,11,12,13)]

# scale data
m_dataMean <- sapply(m_data, mean, na.rm = T)
m_dataSD <- sapply(m_data, sd, na.rm = T)
m_dataPosOff <- t(t(m_data) - m_dataMean)
m_data  <- t(t(m_dataPosOff) / m_dataSD)

v_trainingData <- createDataPartition(y = v_labels, p = 0.9, list = FALSE)
m_trainingFeatures <- m_data[v_trainingData,]
v_trainingLabels <- v_labels[v_trainingData]

m_testingFeatures <- m_data[-v_trainingData,]
v_testingLabels <- v_labels[-v_trainingData]

#split training data into train and validation
v_validationData <- createDataPartition(y = v_trainingLabels, p = 1/9, list = FALSE)

m_validationFeatures <- m_trainingFeatures[v_validationData,]
v_validationLabels <- v_trainingLabels[v_validationData]

m_trainingFeatures <- m_trainingFeatures[-v_validationData,]
v_trainingLabels <- v_trainingLabels[-v_validationData]

v_lambdas = c(1,0.1,0.01,0.001,0.0001)
v_modelsAccuracy <- vector("numeric", length(v_lambdas))
v_models <- matrix(0, length(v_lambdas), ncol(m_trainingFeatures) + 1)
counter <- 1

for(s_lmbda in v_lambdas){
  
  classificationResults <- apply_ssvm(m_trainingFeatures, v_trainingLabels, s_lmbda, s_holdOutEvalSize = 500, s_seasons = 50)
  model <- classificationResults[[1]]
  v_cost_plot <- classificationResults[[2]]
  v_weights_abs <- classificationResults[[3]]
  if(counter == 1){
    plot(1:length(v_weights_abs), v_weights_abs, type="n")
  }
  lines(1:length(v_weights_abs), v_weights_abs )
  
  v_currentResults <- apply_ssvm_eval(m_validationFeatures, model)
  s_accuracy <- CalculateAccuracy(v_currentResults, v_validationLabels)
  v_modelsAccuracy[counter] <- s_accuracy
  v_models[counter,] <- model
  print(append("Accuracy when lambda:", s_lmbda))
  print(s_accuracy)
  counter <- counter + 1
}

s_bestModelIndex <- which.max(v_modelsAccuracy)
print(append("Best model is with lambda equal to", v_lambdas[s_bestModelIndex]))
v_testResults <- apply_ssvm_eval(m_testingFeatures, model)
#print(results)
s_accuracy <- CalculateAccuracy(v_testResults, v_testingLabels)
print("Final Accuracy")
print(accuracy)

#compare to svm light
#model<- svmlight(m_trainingFeatures, v_trainingLabels)
#m_predictions<-predict(model,newdata=m_testingFeatures)
#labels<-m_predictions$class
#accuracy <- sum(labels==v_testingLabels)/(sum(labels==v_testingLabels)+sum(!(labels==v_testingLabels)))
#print(accuracy)

