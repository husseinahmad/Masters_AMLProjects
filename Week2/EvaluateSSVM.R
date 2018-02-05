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

v_trainingData <- createDataPartition(y = v_labels, p = 0.8, list = FALSE)
m_trainingFeatures <- m_data[v_trainingData,]
v_trainingLabels <- v_labels[v_trainingData]

m_testingFeatures <- m_data[-v_trainingData,]
v_testingLabels <- v_labels[-v_trainingData]
model <- apply_ssvm(m_trainingFeatures, v_trainingLabels, 0.01, s_holdOutEvalSize = 50, n = 100, s_seasons = 50)

results <- apply_ssvm_eval(m_testingFeatures, model)
print(results)
accuracy <- CalculateAccuracy(results, v_testingLabels)

print("Accuracy")
print(accuracy)

#compare to svm light
#model<- svmlight(m_trainingFeatures, v_trainingLabels)
#m_predictions<-predict(model,newdata=m_testingFeatures)
#labels<-m_predictions$class
#accuracy <- sum(labels==v_testingLabels)/(sum(labels==v_testingLabels)+sum(!(labels==v_testingLabels)))
#print(accuracy)

