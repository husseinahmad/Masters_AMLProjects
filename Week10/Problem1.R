library(klaR)
library(caret)
library(matrixStats)

s_smoothingConstant <- 0.001
v_voc <- read.table("..\\..\\Datasets\\Topics\\vocab.nips.txt", sep = '\t', header=FALSE)
m_data <- read.table("..\\..\\Datasets\\Topics\\docword.nips.txt\\docword.nips.txt", sep = ' ', header=FALSE, skip = 3)
m_docs <- matrix(0, 1500, 12419)


# populate docs
for(i in 1:nrow(m_data))
{
  m_docs[m_data[i,1],m_data[i,2]] <- m_data[i,3]
}

# random initialization
print("Random initialization")
topics <- vector("list", length = 30)
for(m in 1:nrow(m_docs))
{
  s_randTopic <- ceiling(runif(1, 0, 30))
  topics[[s_randTopic]] <- rbind(topics[[s_randTopic]], m_docs[m,])
}

# calculate the p's for each topic
computeLM = function(topic){
  v_counts <- apply(topic, c(2), sum)
  #v_weights <- (v_counts + 1) / (sum(v_counts) + length(v_counts))
  #v_weights <- v_counts / sum(v_counts)
  v_weights <- (v_counts / sum(v_counts)) + s_smoothingConstant
  return(v_weights)
}
m_topic_probabilities <- (lapply(topics, computeLM))
m_topic_probabilities <- do.call(rbind, m_topic_probabilities)
# normalize back
m_topic_probabilities <- m_topic_probabilities / (1 + (s_smoothingConstant * 12419))

# calculate the pi's for each topic
v_topic_pis <- unlist(lapply(topics,nrow))
v_topic_pis <- v_topic_pis / sum(v_topic_pis)
# as a test, the following statement results in 30 1's
#apply(m_topic_probabilities, c(1), sum)
print("PI's")
print(v_topic_pis)
print("Topics")
for(t in 1:30)
{
  print(v_voc[tail(order(m_topic_probabilities[t,]), 10),1])
}

#Apply EM algorithm

m_hidden_deltas <- matrix(0, 1500, 30)
m_hidden_deltas_old <- matrix(0, 1500, 30)
# calculate sums of docs whch will not be changed t(Xi) %*% 1
# its size should be 1500 * 1 to denote documents size
v_docs_sums <- apply(m_docs, c(1), sum)

for(iteration in 1:50)
{
  print(paste("iteration", iteration))
  # E Step
  print("E Step")
  for(i in 1:nrow(m_docs))
  {
    #print(paste("document", i))
    for(j in 1:nrow(m_topic_probabilities))
    {
      m_hidden_deltas[i,j] <- sum(m_docs[i,] * log(m_topic_probabilities[j,])) + log(v_topic_pis[j])
    }
    m_hidden_deltas[i,] <- m_hidden_deltas[i,] - logSumExp(m_hidden_deltas[i,])
    m_hidden_deltas[i,] <- exp(m_hidden_deltas[i,]) #convert back by taking exp
  }
  
  # M Step

  print("M Step")
  
  m_topic_probabilities <- t(m_hidden_deltas) %*%  m_docs
  normalizer <- t(m_hidden_deltas) %*% apply(m_docs, c(1), sum) 
  m_topic_probabilities <- apply(m_topic_probabilities, c(2), function(x) x/normalizer)
  
  
  # adjust topic probabilities again
  m_topic_probabilities <- (lapply(topics, computeLM))
  m_topic_probabilities <- do.call(rbind, m_topic_probabilities)
  # normalize back
  m_topic_probabilities <- m_topic_probabilities / (1 + (0.00001 * 12419))
  
  
  #update pis
  v_topic_pis <- apply(m_hidden_deltas, c(2), sum) / nrow(m_docs)
  
  # check convergence
  s_biggestDiff <- max(abs(m_hidden_deltas - m_hidden_deltas_old))
  print(s_biggestDiff)
  if(s_biggestDiff < exp(-7))
  {
    break
  }
  else
  {
    m_hidden_deltas_old <- m_hidden_deltas
  }
  
}

print("PI's")
print(v_topic_pis)
print("Topics")
# sorted incrementally, picking tail 10
m_topicsWords <- apply(m_topic_probabilities, c(1), function(x) v_voc[tail(order(x), 10),1])
write.table(t(m_topicsWords), file="output.txt", row.names=FALSE, col.names=FALSE, sep = "\t", quote = TRUE)
v_TopicNames <- sprintf("Topic%s",seq(1:30))
barplot(v_topic_pis, main="Topics Distribution", names.arg = v_TopicNames, ylab="Probability", las=2)


