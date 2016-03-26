run_LDA <- function(corpus, k, rowids, sparsity, corpus2)
{
  
  dtm <- DocumentTermMatrix(corpus)
  dtm <- removeSparseTerms(dtm, sparse=sparsity)
  topics <- LDA(dtm, k)
  topic_probabilities <- as.data.frame(topics@gamma)
  colnames(topic_probabilities) <- lapply(seq(k), function(x) { paste0("Topic_",x)})
  
  newdtm <- DocumentTermMatrix(corpus2)
  newdtm <- removeSparseTerms(newdtm, sparse=sparsity)
  preds <- posterior(topics, newdata = newdtm)
  colnames(preds$topics) <- lapply(seq(k), function(x) { paste0("Topic_",x)})
  
 
  topic_probabilities <- rbind(topic_probabilities,preds$topics)
  rownames(topic_probabilities) <-  rowids

  return(topic_probabilities)
  
}

