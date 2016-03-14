##################################
#####Context vectors Function#####
##################################
#Load requisite packages
setwd("~/GitHub/dsicapstone-predicting_extremism/nmvenuti_sandbox")
library(lsa)
source("preprocessing.R")

filepath="C:/Users/nmvenuti/Desktop/UVA MSDS/Capstone/webscraping westboro/"
minMatches=25
window_length=15
most_freq_words=c('god','lord','jesus')
sim_count=1000

#Co occurenn
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



#Quantify context vectors
quantifyContextVectors<-function(filepath,most_freq_words,minMatches=25,window_length=15,sim_count=1000){
  
  #Check Load requisite data
<<<<<<< HEAD:nmvenuti_sandbox/NMV updates/quantifyContext.R
  target_corpus=load(paste(filepath,'Rdata/processedTokens.RData',sep="/"))
=======
  load(paste(filepath,'Rdata/processedTokens.RData',sep="/"))
  load(paste(filepath,'Rdata/wordCocurrences.RData',sep="/"))
  target_corpus <- processedTokens
>>>>>>> 928d417e7cfb9a90ba5691179423e9262e6bf045:nmvenuti_sandbox/quantifyContext.R
  
  #Create coocurence matrix
  k=5
  bySentence=F
  wordCoocurrences<-tm_map(processedTokens, content_transformer(wordCoOccurences),k, bySentence)
  save(wordCoocurrences, file=paste0(filepath,'/Rdata/wordCoocurrences.RData')) 
  
  # #Subset vectors
  # countWords <- wordCoocurrences[,.(length(context)), 
  #                                             by=target][V1 > minMatches]$target
  # wordCoocurrences <- wordCoocurrences[target %in% countWords &
  #                                                            context %in% countWords]
  # wordCoocurrences[,c("target", "context"):=
  #                              list(as.factor(target), as.factor(context))]
  
  #run Distributional semantic model(DSM)
<<<<<<< HEAD:nmvenuti_sandbox/NMV updates/quantifyContext.R
  rawDsm <- dsm(target=wordCoocurrences$target,feature=wordCoocurrences$context,score=wordCoocurrences$freq,N=100)
=======
  rawDsm <- dsm(target=wordCoocurrences$target,
                feature=wordCoocurrences$context,
                score=wordCoocurrences$freq,
                N=100, raw.freq=TRUE)
>>>>>>> 928d417e7cfb9a90ba5691179423e9262e6bf045:nmvenuti_sandbox/quantifyContext.R
  
  
  #Subset DSM for, project DSM into lower-dimenstional subspace
  rawDsm <- subset(rawDsm, nnzero >= 10, nnzero >= 10, T)
  dsmProj <- dsm.projection(rawDsm, "svd")
  
  #Transpose and convert dsm into indexical dataframe, only take first svd
  dsmDF <- data.frame(t(rawDsm[,1]))
  
  
  #Get context vectors
  
  #for each document in corpus
  for (i in 1:length(target_corpus)){
    
    #extract text
    text_string <- target_corpus[[i]]$content
    n <- length(text_string)
    doc_context_vector_df <- data.frame(text_string)
    colnames(doc_context_vector_df)=c('target')
    doc_context_vector_df$context_vector=""
    
    for (j in 1:n){
      
      #Set window bounds
      lower_bound <- window_length - 15
      upper_bound <- window_length + 15
      if(lower_bound<1){
        lower_bound=1
      }
      if(upper_bound>n){
        upper_bound=n
      }
      
      #Identify target word
      id_word <- text_string[j]
      
      #Get context vector ids and extract target word
      context_names <- text_string[lower_bound:upper_bound]
      # context_names=context_names[-(j-lower_bound)]
      
      #Remove words stripped from dsm
      context_names <- context_names[context_names %in% colnames(dsmDF)]
      
      #Add in section to extract context vectors
      doc_context_vector_df$context_vector[j] <- paste(context_names,collapse = "-") 
      
    }
    
    
    if(exists('context_vector_df')){
      context_vector_df<-rbind(context_vector_df,doc_context_vector_df)
    }else{
      context_vector_df<-data.frame(doc_context_vector_df)
      colnames(context_vector_df)<-colnames(doc_context_vector_df)
    }
  }
  
  #Loop through each word in words_to_analyze
  words_to_analyze <- len(most_freq_words)
  cvCosineSim<-list(rep(0, words_to_analyze))
  for (word_id in 1:words_to_analyze){
    
    #Extract words
    search_word <- most_freq_words[word_id]
    context_vector<-context_vector_df
    
    context_vector_subset<-context_vector$context_vector[context_vector$target==search_word]
    
    cosine_results <- list(rep(0, sim_count))
    
    for (j in 1:sim_count){
      
      x=ceiling(runif(sim_count,min = 0,max = length(context_vector_subset)))
      y=ceiling(runif(sim_count,min = 0,max = length(context_vector_subset)))
      
      
      
      x_list=as.vector(strsplit(context_vector_subset[x[j]],"-"))[[1]]
      y_list=as.vector(strsplit(context_vector_subset[y[j]],"-"))[[1]]
      
      #Get list of unique words
      column_list<-c(x_list,y_list)
      unique_words<-unique(column_list)
      
      #Extract dsm columns for calculations
      dsm.reduce <- dsmDF[,unique_words]
      
      #Create empty matricies for x and y
      x_mat<-matrix(0,nrow=nrow(dsm.reduce),ncol=length(column_list))
      y_mat<-x_mat
      
      for (k in 1:length(column_list)){
        #If column word in x_list, add to x_mat
        if(column_list[k] %in% x_list){
          x_mat[,k] <- dsm.reduce[,column_list[k]]
        }
        #If column word in y_list, add to y_mat
        if(column_list[k] %in% y_list){
          y_mat[,k] <- dsm.reduce[,column_list[k]]
        }
      }
      
      #Calculate cosine simularity
      cosine_results[j] <- cosine(as.vector(x_mat),as.vector(y_mat))[[1,1]]
    }
    cosine_results_x <- data.frame(cosine_results)
    cvCosineSim[word_id] <- apply(cosine_results_x,1,mean)
  }
  
  #Save backup for average cosine similarity
  save(cvCosineSim, file=paste(filepath,'Rdata/cvCosineSim.RData',sep="/"))
}

#TEst function
quantifyContextVectors(filepath,most_freq_words,minMatches,window_length,sim_count)