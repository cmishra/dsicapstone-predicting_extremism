#' Calculating Sentiment from Lexicon of Pos/Neg Words
#' Lexicon Source: https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon


calc_Sentiment <- function(tokenizedCorpus, groupName) {
  
  # Read in Labeled Words Sets
  positives= readLines("data_dsicap/ref/positive-words.txt")
  negatives = readLines("data_dsicap/ref/negative-words.txt")
  
  posStemmed <- VCorpus(VectorSource(positives))
  posStemmed <- tm_map(posStemmed, content_transformer(wordStem))
  posStemmed <- data.frame(unlist(lapply(sapply(posStemmed, "[", "content"),paste,collapse="\n")), stringsAsFactors = F)
  newPositives <- unique(posStemmed)
  
  negStemmed <- VCorpus(VectorSource(negatives))
  negStemmed <- tm_map(negStemmed, content_transformer(wordStem))
  negStemmed <- data.frame(unlist(lapply(sapply(negStemmed, "[", "content"),paste,collapse="\n")), stringsAsFactors = F)
  newNegatives <- unique(negStemmed)
  
  # bigMaster <- c(fileName = c(), totalWords = c(), posWords = c(), negWords = c())
  bigMaster <- c()
  
  # for (d in 1:4){
  for (d in 1:length(tokenizedCorpus)){
    # Replace with Tokens
    #doc <- as.String(corpus[[d]])
    
    fileName <- (meta(tokenizedCorpus[[d]], "id"))
    
    #test <- cleanTxt(doc)
    
    #word_list = str_split(test, " ")
    #words = unlist(word_list)
    #words = tolower(words)
    
    words = tokenizedCorpus[[d]]$content
    
    positive.matches = words %in% positives
    #sum(positive.matches)
    
    negative.matches = words %in% negatives
    #sum(negative.matches)
    
    total.words = length(words)
    #neutral.words = total.words - positive.matches - negative.matches
    
    newRow <- c(as.numeric(total.words), as.numeric(sum(positive.matches)), as.numeric(sum(negative.matches)))
    bigMaster <- rbind(bigMaster, newRow)
  }
  
  bigMaster <- as.data.frame(bigMaster)
  colnames(bigMaster) <- c("TotalWords","PosWords", "NegWords")
  bigMaster$sentiment <- ifelse(bigMaster$PosWords > bigMaster$NegWords, "Pos","Neg")
  # View(bigMaster)
  
  #sum(bigMaster$sentiment == "Pos")/nrow(bigMaster)
  #x <- subset(bigMaster, bigMaster$sentiment == "Pos")
  #sum(x$PosWords)/sum(bigMaster$TotalWords)
  
  
  #sum(bigMaster$PosWords)/sum(bigMaster$TotalWords)
  #sum(bigMaster$NegWords)/sum(bigMaster$TotalWords)
  
  
  metrics <- data.frame("group" = groupName, 
                        "%PosWords" = round(sum(bigMaster$PosWords)/sum(bigMaster$TotalWords),4),
                        "%NegWords" = round(sum(bigMaster$NegWords)/sum(bigMaster$TotalWords),4),
                        "%PosDoc" = round(sum(bigMaster$sentiment == "Pos")/nrow(bigMaster),4),
                        "%NegDoc" = round(sum(bigMaster$sentiment == "Neg")/nrow(bigMaster),4))
  return(metrics)
}

