
source("StochasticSVM.R")
m_trainingFeatures <- cbind(c(4,6,1,2,6,-3), c(5,8,4,1,6,4))
v_trainingLabels <- c(1,1,-1,-1,1,-1)
model <- apply_ssvm(m_trainingFeatures, v_trainingLabels, 0.01, s_holdOutEvalSize = 2)

m_testingFeatures <- cbind(c(1,-7,11,8), c(1,2,4,11))
v_testingLabels <- c(-1,-1,1,1)

results <- apply_ssvm_eval(m_testingFeatures, model)
print(results)
accuracy <- CalculateAccuracy(results, v_testingLabels)

print("Accuracy")
print(accuracy)