library(klaR)
library(caret)

m_data <- read.csv('pima-indians-diabetes.data', header=FALSE)
# additional step for 1.B, replace 0 with NA for specific columns
m_data[,c(3,4,6,8)][m_data[,c(3,4,6,8)] == 0] <- NA
m_features <- m_data[,-c(9)]
v_labels <- m_data[,c(9)]
m_trainingData <- createDataPartition(y = v_labels, p = 0.8, list = FALSE)
m_trainingFeatures <- m_features[m_trainingData,]
m_trainingLabels <- v_labels[m_trainingData]
v_class <- m_trainingLabels > 0

m_trainingPos <- m_trainingFeatures[v_class,]
m_trainingNeg <- m_trainingFeatures[!v_class,]  

m_testingFeatures <- m_features[-m_trainingData,]
m_testingLabels <- v_labels[-m_trainingData]

# Distribution Parameters #
m_trainPosMean <- sapply(m_trainingPos, mean, na.rm = T)
m_trainNegMean <- sapply(m_trainingNeg, mean, na.rm = T)

m_trainPosSD <- sapply(m_trainingPos, sd, na.rm = T)
m_trainNegSD <- sapply(m_trainingNeg, sd, na.rm = T)

s_posPrior <- nrow(m_trainingPos) / nrow(m_trainingFeatures)
s_negPrior <- nrow(m_trainingNeg) / nrow(m_trainingFeatures)

#########################################################

m_trainPosOff <- t(t(m_trainingFeatures) - m_trainPosMean)
m_trainPosScale  <- t(t(m_trainPosOff) / m_trainPosSD)
m_trainPosLogs    <- -(1/2) * rowSums(apply(m_trainPosScale, c(1,2),
                                     function(x) x^2), na.rm = T) - sum(log(m_trainPosSD))
                                      + log(s_posPrior)
m_trainNegOff <- t(t(m_trainingFeatures) - m_trainNegMean)
m_trainNegScale  <- t(t(m_trainNegOff) / m_trainNegSD)
m_trainNegLogs    <- -(1/2) * rowSums(apply(m_trainNegScale, c(1,2),
                                            function(x) x^2), na.rm = T) - sum(log(m_trainNegSD))
                                      + log(s_negPrior)
m_labeledTrueTrain <- m_trainPosLogs > m_trainNegLogs
m_labeledCorrectTrain <- m_labeledTrueTrain == m_trainingLabels
s_trainAccuracy <- sum(m_labeledCorrectTrain)/(sum(m_labeledCorrectTrain)+sum(!m_labeledCorrectTrain))

#Applying on testing data

# apply classification equations
m_testPosOff <- t(t(m_testingFeatures) - m_trainPosMean)
m_testPosScale  <- t(t(m_testPosOff) / m_trainPosSD)
m_testPosLogs    <- -(1/2) * rowSums(apply(m_testPosScale, c(1,2),
                                            function(x) x^2), na.rm = T) - sum(log(m_trainPosSD))
                                            + log(s_posPrior)
m_testNegOff <- t(t(m_testingFeatures) - m_trainNegMean)
m_testNegScale  <- t(t(m_testNegOff) / m_trainNegSD)
m_testNegLogs    <- -(1/2) * rowSums(apply(m_testNegScale, c(1,2),
                                            function(x) x^2), na.rm = T) - sum(log(m_trainNegSD))
                                            + log(s_negPrior)
m_labeledTrueTest <- m_testPosLogs > m_testNegLogs
m_labeledCorrectTest <- m_labeledTrueTest == m_testingLabels
s_testAccuracy <- sum(m_labeledCorrectTest)/(sum(m_labeledCorrectTest)+sum(!m_labeledCorrectTest))

#where is application on testing
# where is Prior probability

print(s_trainAccuracy)
print(s_testAccuracy)