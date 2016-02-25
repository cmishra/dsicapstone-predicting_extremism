# install.packages(c("cluster", "tm", "ggplot2", "RWeka", "snowball"))
# install.packages("wordspace", repos="http://R-Forge.R-project.org")

library(tm)
library(wordspace)
library(RWeka)
library(SnowballC)
library(parallel)
library(data.table)
library(compiler)
library(Rcpp)
library(RcppArmadillo)
# library(testthat)
library(stringr)

# stpwds <- readLines("../../stopWords_Religious.txt")
stpwds <-stopwords()
  
removePunctuationExceptPeriod <- cmpfun(function(x) {
  gsub("[^[:alpha:][:space:].?!]", "", x)
})

individualizeEndOfSent <- cmpfun(function(x) {
  gsub("[.?!]+", " . ", x)
})

simpleTokenizer <- cmpfun(function(x) {
  unlist(strsplit(x, split="[[:space:]]+"))
})

removeEmptyStrings <- cmpfun(function(x) {
  x[!x == ""]
})

preprocessing <- function(texts, stpwds=T, wordStem=F){
  texts <- tm_map(texts, content_transformer(individualizeEndOfSent))
  texts <- tm_map(texts, content_transformer(simpleTokenizer))
  texts <- tm_map(texts, content_transformer(removePunctuationExceptPeriod))
  texts <- tm_map(texts, content_transformer(tolower))
  if (!("logical" %in% class(stpwds)))
    texts <- tm_map(texts, content_transformer(removeWords), stpwds)
  if (!("logical" %in% class(wordStem)))
    texts <- tm_map(texts, content_transformer(wordStem))
  texts <- tm_map(texts, content_transformer(removeEmptyStrings))
  texts
}

# content = input of a document as a string
# k = size of window, ignored if bySentence=True
# bySentence = measure co-occurrances by sentence
wordCoOccurences <- cmpfun(function(content, k, bySentence = F) {
  if (length(content) == 1)
    return(data.table())
  endOfSentences <- which(content == ".")
  ret <- rbindlist(lapply(1:length(content), function(i) {
    if (bySentence) {
      beforePeriodIndexes <- endOfSentences < i
      startingIndex <- max(c(endOfSentences[beforePeriodIndexes], 0))
      endingIndex <- min(c(endOfSentences[!beforePeriodIndexes], length(content)))
      contextWords <- content[setdiff(startingIndex:endingIndex, i)]
    } else {
      contextWords <- c(tail(content[setdiff(1:(i-1), endOfSentences)], k),
                        head(content[
                          setdiff((i+1):length(content), endOfSentences)], k))
    }
    data.frame(target=content[i], context=contextWords, freq=1)
    # as.data.table(data.frame(target=content[i], context=contextWords, freq=1))
  }))
  # ret[,.(freq=sum(freq)), by=.(target, context)][!(target == "." | context == ".")]
})

fasterCooccurrences <- cmpfun(function(content, k, bySentence=F){
  if (length(content) == 1)
    return(data.table())
  if (bySentence)
    load_string_sentence(content)
  else 
    load_string_k(sentence, k)
  cooccurrences <- pull_table()
  
}) 

text_venn_diagram <- function(set1, set2) {
  output <- list()
  output[[2]] <- intersect(set1, set2)
  output[[1]] <- setdiff(set1, set2)
  output[[3]] <- setdiff(set2, set1)
  output
  
}

mock_map_info <- function(word, context, freq) {
  paste0(word, occurrences_sepstr, context, " - ", freq)
}

Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
occurrences_sepstr <- "_!_%_?_"
sepstr_regex <- "_!_%_\\?_"
reload_cooccurrences <- function() {
  if (!is.na(str_match(getwd(), "tests")))
    sourceCpp("../cooccurences.cpp")
  else 
    sourceCpp("cooccurences.cpp")
}

# tdmify <- function(corp, n){
#   tokenizr <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
#   
#   ctrl <- list(tokenize=tokenizr,
#                tolower=T,
#                removePunctation=T,
#                removeNumbers=T,
#                stopwords=stpwds,
#                stemming=wordStem,
#                wordLengths=c(1,Inf)
#   )
#   TermDocumentMatrix(corp, ctrl)
# }

cVectors<-function(target_corpus,dsm,window_length=15) {
  #Create dataframe to save all context vectors
  context_vector_df=data.frame('target','context_vector')
  
  #Transpose and convert dsm into indexical dataframe
  dsm.df=data.frame(t(dsm))
  
  #for each document in corpus
  for (i in length(target_corpus)){
    
    #extract text
    text_string=target_corpus[[i]]$content
    n=length(text_string)
    
    #Create dataframe
    doc_context_vector_df=data.frame(text_string)
    colnames(doc_context_vector_df)=c('target')
    doc_context_vector_df$context_vector=""
    
    #For each word in corpus
    for (j in n){
      
      #Set window bounds
      lower_bound=j-15
      upper_bound=j+15
      if(lower_bound<1){
        lower_bound=1
      }
      if(upper_bound>n){
        upper_bound=n
      }
      
      #Identify target word
      id_word=text_string[j]
      
      #Get context vector ids and extract target word
      context_names=text_string[lower_bound:upper_bound]
      context_names=context_names[-j]
      
      #Create context vector for specified id
      context_vector=vector(mode = 'list',length = length(context_names))
      names(context_vector)=context_names
      for (word_name in context_names){
        #Get distributional vector for name and assign to 
        context_vector[word_name]=dsm.df[word_name]
      }
      doc_context_vector_df$context_vector[j]=context_vector
    }
    #Bind document context vectors dataframe to main dataframe
    context_vector_df=rbind(context_vector,doc_context_vector_df)
  }
  return(context_vector_df)
}



