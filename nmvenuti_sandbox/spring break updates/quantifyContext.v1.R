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
most_freq_words=c('god','lord')
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



###############################################################################################################################
#Quantify context vectors
quantifyContextVectors<-function(filepath,most_freq_words,minMatches=25,window_length=15,sim_count=1000){
  
  #Check Load requisite data
  load(paste(filepath,'Rdata/processedTokens.RData',sep="/"))
  target_corpus<-processedTokens
  #Create coocurence matrix
  k=5
  bySentence=F
  wordCooccurences<-rbindlist(lapply( target_corpus, function(doc) wordCoOccurences(doc$content, k, bySentence)))
  wordCooccurences<-wordCooccurences[target != "." & context != ".",
                                     .(freq=sum(freq)), 
                                     .(target, context)]
  save(wordCooccurences, file=paste0(filepath,'/Rdata/wordCoocurrences.RData')) 
  
  
  
  
  #Subset vectors only greater than min matches
  countWords <- wordCooccurences[,.(length(context)), 
                                 by=target][V1 > minMatches]$target
  wordCooccurences <- wordCooccurences[target %in% countWords &
                                         context %in% countWords]
  wordCooccurences[,c("target", "context"):=
                     list(as.factor(target), as.factor(context))]
  
  #run Distributional semantic model(DSM)
  rawDSM <- dsm(target=wordCooccurences$target,
                feature=wordCooccurences$context,
                score=wordCooccurences$freq,
                N=100)
  
  
  #Subset DSM for, project DSM into lower-dimenstional subspace
  rawDSM <- subset(rawDSM, nnzero >= 10, nnzero >= 10, T)
  dsmProj <- dsm.projection(rawDSM, "svd")
  
  #Save DSM projections
  save(dsmProj, file=paste0(filepath,'/Rdata/dsmProj.RData')) 
  
  #Transpose and convert dsm into indexical dataframe, only take first svd
  dsmDF=data.frame(t(dsmProj[,1]))
  
  
  #Get context vectors
  
  #for each document in corpus
  for (i in 1:length(target_corpus)){
    
    #extract text
    text_string=target_corpus[[i]]$content
    n=length(text_string)
    doc_context_vector_df=data.frame(text_string)
    colnames(doc_context_vector_df)=c('target')
    doc_context_vector_df$context_vector=""
    
    for (j in 1:n){
      
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
      # context_names=context_names[-(j-lower_bound)]
      
      #Remove words stripped from dsm
      context_names=context_names[context_names %in% colnames(dsmDF)]
      
      #Add in section to extract context vectors
      doc_context_vector_df$context_vector[j]=paste(context_names,collapse = "-") 
      
    }
    
    
    if(exists('context_vector_df')){
      context_vector_df<-rbind(context_vector_df,doc_context_vector_df)
    }else{
      context_vector_df<-data.frame(doc_context_vector_df)
      colnames(context_vector_df)<-colnames(doc_context_vector_df)
    }
  }
  
  #Loop through each word in words_to_analyze
  words_to_analyze=length(most_freq_words)
  cvCosineSim<-list(rep(0, words_to_analyze))
  
  for (word_id in 1:words_to_analyze){
    
    #Extract words
    search_word<-most_freq_words[word_id]
    
    context_vector_subset<-context_vector_df$context_vector[context_vector_df$target==search_word]
    
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
      dsm.reduce=dsmDF[,unique_words]
      
      #Create empty matricies for x and y
      x_mat<-matrix(0,nrow=nrow(dsm.reduce),ncol=length(column_list))
      y_mat<-x_mat
      
      for (k in 1:length(column_list)){
        #If column word in x_list, add to x_mat
        if(column_list[k] %in% x_list){
          x_mat[,k]=dsm.reduce[,column_list[k]]
        }
        #If column word in y_list, add to y_mat
        if(column_list[k] %in% y_list){
          y_mat[,k]=dsm.reduce[,column_list[k]]
        }
      }
      
      #Calculate cosine simularity
      cosine_results[j]=cosine(as.vector(x_mat),as.vector(y_mat))[[1,1]]
    }
    cosine_results_x=data.frame(cosine_results)
    cvCosineSim[word_id]=apply(cosine_results_x,1,mean)
  }
  
  #Save backup for average cosine similarity
  save(cvCosineSim, file=paste(filepath,'Rdata/cvCosineSim.RData',sep="/"))
}

#TEst function
quantifyContextVectors(filepath,most_freq_words,minMatches,window_length,sim_count)
load(paste(filepath,'Rdata/cvCosineSim.RData',sep="/"))
