library(klaR)
library(caret)
library(randomForest)

v_voc <- read.table("..\\..\\Datasets\\Topics\\vocab.nips.txt", sep = '\t', header=FALSE)
m_data <- read.table("..\\..\\Datasets\\Topics\\docword.nips.txt\\docword.nips.txt", sep = ' ', header=FALSE, skip = 3)
m_docs <- matrix(0, 1500, 12419)

# populate docs
for(i in 1:nrow(m_data))
{
  m_docs[m_data[i,1],m_data[i,2]] <- m_data[i,3]
}

# initialize with kmeans to define initial parameters for Topic Modeling
init_model <- kmeans ( m_docs , centers = 30 )
topics <- vector("list", length = 30)
v_docs_topics <- vector("numeric", 1500)

#assigning each document to a topic/cluster
for(m in 1:nrow(m_docs))
{
  if(m %% 10 == 0)
    print(m)
  v_distances <- apply(init_model$centers, c(1), function(x)sqrt(sum((m_docs[m,] - x) ^ 2)))
  s_minIndex <- which.min(v_distances)
  topics[[s_minIndex]] <- rbind(topics[[s_minIndex]],m_docs[m,])
  v_docs_topics[m] <- s_minIndex
}

# calculate the p's for each topic
computeLM = function(topic){
  v_counts <- apply(topic, c(2), sum)
  #v_weights <- (v_counts + 1) / (sum(v_counts) + length(v_counts))
  #v_weights <- v_counts / sum(v_counts)
  v_weights <- (v_counts / sum(v_counts)) + 0.00001
  return(v_weights)
}
m_topic_probabilities <- (lapply(topics, computeLM))
m_topic_probabilities <- do.call(rbind, m_topic_probabilities)
# normalize back
m_topic_probabilities <- m_topic_probabilities / (1 + (0.00001 * 12419))

# calculate the pi's for each topic
v_topic_pis <- unlist(lapply(topics,nrow))
v_topic_pis <- v_topic_pis / sum(v_topic_pis)
# as a test, the following statement results in 30 1's
#apply(m_topic_probabilities, c(1), sum)

#Apply EM algorithm

m_hidden_deltas <- matrix(0, 1500, 30)
# calculate sums of docs whch will not be changed t(Xi) %*% 1
# its size should be 1500 * 1 to denote documents size
v_docs_sums <- apply(m_docs, c(1), sum)

for(iteration in 1:50)
{
  print(paste("iteration", iteration))
  # E Step
  for(i in 1:nrow(m_docs))
  {
    print(paste("document", i))
    for(j in 1:nrow(m_topic_probabilities))
    {
      m_hidden_deltas[i,j] <- exp(sum(log(m_topic_probabilities[j,] ^ m_docs[i,]))) * v_topic_pis[j]
    }
  }
  
  # M Step
  # update p's
  m_topic_probabilities <- t(m_hidden_deltas) %*% m_docs
  # then normalize, same size as hidden deltas, 1500 * 30
  v_normalizer <- m_hidden_deltas * v_docs_sums
  # now sum it per topic to be equal to 30
  v_normalizer <- apply(v_normalizer, c(1), sum)
  m_topic_probabilities <- m_topic_probabilities / v_normalizer
  
  #update pis
  v_topic_pis <- apply(m_hidden_deltas, c(2), sum) / nrow(m_docs)
}


