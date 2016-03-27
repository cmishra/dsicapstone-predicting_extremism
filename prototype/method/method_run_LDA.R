run_LDA <- function(traincorpus, k, groupids, docids, sparsity, testcorpus)
{
  
  dtm <- DocumentTermMatrix(traincorpus)
  dtm <- removeSparseTerms(dtm, sparse=sparsity)

  topics <- LDA(dtm, k)
  topic_probabilities <- as.data.frame(topics@gamma)
  colnames(topic_probabilities) <- lapply(seq(k), function(x) { paste0("Topic_",x)})
  
  testdtm <- DocumentTermMatrix(testcorpus)
  testdtm <- removeSparseTerms(testdtm, sparse=sparsity)
  preds <- posterior(topics, newdata = testdtm)
  colnames(preds$topics) <- lapply(seq(k), function(x) { paste0("Topic_",x)})
  
  
  topic_probabilities <- rbind(topic_probabilities,preds$topics)
  rownames(topic_probabilities) <-  docids
  topic_probabilities$GroupID <- groupids
  
  return(topic_probabilities)
  
}