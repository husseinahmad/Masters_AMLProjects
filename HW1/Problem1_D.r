library(klaR)
library(caret)

m_data <- read.csv('pima-indians-diabetes.data', header=TRUE)
m_features <- m_data[,-c(9)]
v_labels <- as.factor(m_data[,c(9)])
m_trainingData <- createDataPartition(y = v_labels, p = 0.8, list = FALSE)
m_trainingFeatures <- m_features[m_trainingData,]
m_trainingLabels <- v_labels[m_trainingData]

m_testingFeatures <- m_features[-m_trainingData,]
m_testingLabels <- v_labels[-m_trainingData]

model<- svmlight(m_trainingFeatures, m_trainingLabels)
m_predictions<-predict(model,newdata=m_testingFeatures)
labels<-m_predictions$class
accuracy <- sum(labels==m_testingLabels)/(sum(labels==m_testingLabels)+sum(!(labels==m_testingLabels)))

print(accuracy)
