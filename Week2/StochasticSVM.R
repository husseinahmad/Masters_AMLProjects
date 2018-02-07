library(klaR)
library(caret)
library(plotly)

calculate_iteration_cost = function(m_x, v_y, v_a, s_b, s_lambda){
  aXiplusb <- apply(m_x, c(1), function(x) sum(x*t(t(v_a))) + s_b )
  v_multiplication <- aXiplusb * v_y
  training_cost <- sum(unlist(lapply(1 - v_multiplication, function(x) max(0,x))))
  training_cost <- training_cost / nrow(m_x)
  s_g0 <- s_lambda/2 * sum(v_a * t(t(v_a)))
  return (training_cost + s_g0)
}

CalculateAccuracy = function(v_results, v_labels){
  v_equal <- as.vector(v_results) == as.vector(v_labels)
  return(sum(v_equal) / length(v_equal))
}

apply_ssvm_eval = function(data, model){
  s_featCount <- length(model) - 1
  scores <- apply(data, c(1), function(x) sum(x * model[-length(model)]) + model[length(model)])
  scores[scores > 0] <- 1
  scores[scores < 0] <- -1
  return (scores)
}

# using gradient of one example
update_weights = function(v_x,s_y,v_a,s_b,step,lambda){
  mult_score <- s_y * (sum(v_x * v_a) + s_b)
  a_gradient <- lambda * v_a #default if mult_score is bigger than 1
  b_gradient <- 0
  if(mult_score < 1)
  {
    a_gradient <- (lambda * v_a) - (s_y * v_x)
    b_gradient <- -1 * s_y 
  }
  
  updated_a <- v_a - (step * a_gradient)
  updated_b <- s_b - (step * b_gradient)
  return(list(updated_a, updated_b))
}

# main function
apply_ssvm = function(m_trainingFeatures, v_trainingLabels, v_lambda, s_seasons = 50, s_steps = 300, s_evaluationRate = 30, m = 1, n = 50, s_holdOutEvalSize = 50) {

  v_cost_plot <- vector("numeric", (s_steps / s_evaluationRate) * s_seasons)
  v_weights_abs <- vector("numeric", (s_steps / s_evaluationRate) * s_seasons)
  print(length(v_cost_plot))
  v_a <- runif(ncol(m_trainingFeatures),0,1)
  s_b <- 1
  for(i in 1:s_seasons)
  {
    step <- m / (i + n)
    epoch_held_out <- sample(1:length(v_trainingLabels),s_holdOutEvalSize)
    m_seasonTrainingFeatures <- m_trainingFeatures[-epoch_held_out,]
    m_seasonTrainingLabels <- v_trainingLabels[-epoch_held_out]
    m_seasonValidationFeatures <- m_trainingFeatures[epoch_held_out,]
    m_seasonValidationLabels <- v_trainingLabels[epoch_held_out]
    
    for(j in 1:s_steps)
    {
      # training example 
      s_trainingExampleIndex <- sample(1:length(m_seasonTrainingLabels), 1)
      v_x <- m_seasonTrainingFeatures[s_trainingExampleIndex,]
      s_y <- m_seasonTrainingLabels[s_trainingExampleIndex]
      l_updatedWeights <- update_weights(v_x, s_y, v_a, s_b, step, v_lambda)
      v_a <- l_updatedWeights[[1]]
      s_b <- l_updatedWeights[[2]]
      if(j %% s_evaluationRate == 0)
      {
        #v_currentCost <- calculate_iteration_cost(m_seasonValidationFeatures, m_seasonValidationLabels, v_a, s_b, v_lambda)
        v_currentValidationScores <- apply_ssvm_eval(m_seasonValidationFeatures, c(v_a,s_b))
        v_currentCost <- CalculateAccuracy(v_currentValidationScores, m_seasonValidationLabels)
        #print(append((j / s_evaluationRate) + ((i-1)*(s_steps/s_evaluationRate)), v_currentCost))
        s_currentIndex <- (j / s_evaluationRate) + ((i-1)*(s_steps/s_evaluationRate))
        v_cost_plot[s_currentIndex] = v_currentCost
        v_weights_abs[s_currentIndex] = sum(sapply(v_a, function(x) x^2)) + s_b^2
      } 
    }
  }
    #plot(1:length(v_cost_plot), v_cost_plot, type="n")
    #lines(1:length(v_cost_plot), v_cost_plot)
    #plot_ly(as.data.frame(v_cost_plot), type = 'scatter', mode = 'lines')
    print(v_a)
    print(s_b)
    return(list(c(v_a,s_b), v_cost_plot, v_weights_abs))
}

