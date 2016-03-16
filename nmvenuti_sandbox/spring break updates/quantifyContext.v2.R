##################################
#####Context vectors Function#####
##################################
#Load requisite packages
setwd("~/GitHub/dsicapstone-predicting_extremism/prototype")
library(lsa)
source("method_preprocessing.R")
source("method_monteCarlo.R")

filepath="C:/Users/nmvenuti/Desktop/UVA MSDS/Capstone/webscraping westboro/"
datafile_name='test_1'
minMatches=25
window_length=15
most_freq_words=c('god','lord')
sim_count=1000

#Co occurenn
# wordCoOccurences <- cmpfun(function(content, k, bySentence = F) {
#   if (length(content) == 1)
#     return(data.table())
#   endOfSentences <- which(content == ".")
#   ret <- rbindlist(lapply(1:length(content), function(i) {
#     if (bySentence) {
#       beforePeriodIndexes <- endOfSentences < i
#       startingIndex <- max(c(endOfSentences[beforePeriodIndexes], 0))
#       endingIndex <- min(c(endOfSentences[!beforePeriodIndexes], length(content)))
#       contextWords <- content[setdiff(startingIndex:endingIndex, i)]
#     } else {
#       contextWords <- c(tail(content[setdiff(1:(i-1), endOfSentences)], k),
#                         head(content[
#                           setdiff((i+1):length(content), endOfSentences)], k))
#     }
#     data.frame(target=content[i], context=contextWords, freq=1)
#     # as.data.table(data.frame(target=content[i], context=contextWords, freq=1))
#   }))
#   # ret[,.(freq=sum(freq)), by=.(target, context)][!(target == "." | context == ".")]
# })



###############################################################################################################################
#Quantify context vectors
# createWordCoOccurences<-function(filepath,datafile_name,processedTokens){
#   
#   #Check Load requisite data
# 
#   target_corpus<-processedTokens
#   #Create coocurence matrix
#   k=5
#   bySentence=F
#   wordCooccurences<-rbindlist(lapply( target_corpus, function(doc) wordCoOccurences(doc$content, k, bySentence)))
#   wordCooccurences<-wordCooccurences[target != "." & context != ".",
#                                      .(freq=sum(freq)), 
#                                      .(target, context)]
#   save(wordCooccurences, file=paste0(filepath,'/Rdata/wordCooccurences_',datafile_name,'.RData'))
# }  
  
 
createDSM<-function(filepath,datafile_name,wordCo){
  #Subset vectors only greater than min matches
  countWords <- wordCo[,.(length(context)), 
                                 by=target][V1 > minMatches]$target
  wordCo <- wordCo[target %in% countWords &
                                         context %in% countWords]
  wordCo[,c("target", "context"):=
                     list(as.factor(target), as.factor(context))]
  
  #run Distributional semantic model(DSM)
  rawDSM <- dsm(target=wordCo$target,
                feature=wordCo$context,
                score=wordCo$freq,
                N=100)
  
  
  #Subset DSM for, project DSM into lower-dimenstional subspace
  rawDSM <- subset(rawDSM, nnzero >= 10, nnzero >= 10, T)
  dsmProj <- dsm.projection(rawDSM, "svd")
  
  #Transpose and convert dsm into indexical dataframe, only take first 300
  dsmProj=data.frame(t(dsmProj[,300]))
  
  dsmProjName<-dsmProj_filename(filepath, datafile_name)
  #Save DSM projections
  save(dsmProj, file=dsmProjName)
}
 
  #Get context vectors
quantifyContext<-function(filepath,datafile_name,target_corpus,dsmProj,most_freq_words,minMatches=25,window_length=15,sim_count=1000){ 
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
    
    #Calculate context vectors
    for (cvIndex in 1:nrow(context_vector_subset)){
      cvWords=as.vector(strsplit(context_vector_subset[cvIndex],"-"))[[1]]
      #Get list of unique words
      unique_words<-unique(cvWords)
      
      #Extract dsm columns for calculations
      dsm.reduce=dsmProj[,unique_words]

      #Create empty vector for cv
      cv<-rep(0,nrow(dsm.reduce))
      
      #Calculate context vector
      for (wordIndex in 1:length(unique_words)){
        cv=dsm.reduce[,unique_words[wordIndex]]+cv
      }
      
    }
    
    #Calculate cosine similarity of context vectors
    cvCosineSim[word_id]=estimate_cos_simil(cv,sim_count)
  }
  
  #Save backup for average cosine similarity
  save(cvCosineSim, file=paste(filepath,'Rdata/cvCosineSim.RData',sep="/"))
}

#TEst function
wordCo<-wordCooccurences
createDSM(filepath,datafile_name,wordCo)
