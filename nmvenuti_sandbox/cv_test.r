#Test context vectors

setwd("C:/Users/nmvenuti/Desktop/UVA MSDS/Fall/DS 6001/")
source("methods.R")
library(lsa)


#Get corpus
# Set up clustering parameters
numCores <- 4
cl <- makeCluster(mc <- getOption("cl.cores", numCores))
clusterEvalQ(cl, {source("methods.R")})

#Define corpuses for Piper and WBC. Segment and combine for parrelization
data_johnpiper <- VCorpus(DirSource("C:/Users/nmvenuti/Desktop/UVA MSDS/Fall/DS 6001/Data Sources/john_piper"))
data_wbc <- VCorpus(DirSource("C:/Users/nmvenuti/Desktop/UVA MSDS/Fall/DS 6001/Data Sources/westboro_sermons/"))
meta(data_johnpiper, "author") <- "johnpiper"
meta(data_wbc, "author") <- "westboro"
corps <- c(data_johnpiper, data_wbc)

#Preprocess combined documents (done in parallel)
splitIndex <- rep_len(1:numCores, length(corps))
processedCorps <- parLapply(cl, split(corps, splitIndex),
                            function (x) preprocessing(x, stpwds, wordStem))
processedCorp <- do.call(function(...) c(..., recursive=T), 
                         processedCorps)


#Write and separate corpuses
# writeCorpus(processedCorp, "processedCorps_wb_johnpiper")

piperCorps <- processedCorp[meta(corps) == "johnpiper"]
wbcCorps <- processedCorp[meta(corps) == "westboro"]


#Set up clustering for word occurrences analysis
clusterEvalQ(cl, {
  k <- 5
  bySentence <- F
})

#Load dsm
load("dsm_decomposed.RData")






#Test function manually
target_corpus=piperCorps
window_length=15
dsm=jp.dsm.proj

#Transpose and convert dsm into indexical dataframe, only take first svd
dsm.df=data.frame(t(dsm[,1]))

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
    context_names=context_names[context_names %in% colnames(dsm.df)]
    
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

#Words to analyze
words_to_analyze=50

#Get most common words
most_freq_words=data.frame(table(context_vector_df$target))
most_freq_words=most_freq_words[with(most_freq_words, order(-Freq)), ]
most_freq_words=most_freq_words$Var[1:words_to_analyze]

#Loop through each word in words_to_analyze
average_cosine_similarity<-list(rep(0, words_to_analyze))
for (word_id in 1:words_to_analyze){

  #Extract words
  search_word=most_freq_words[word_id]
  context_vector<-context_vector_df
  sim_count=1000
  
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
    dsm.reduce=dsm.df[,unique_words]
    
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
  average_cosine_similarity[word_id]=apply(cosine_results_x,1,mean)
}

average_cosine_similarity

#Convert mannual process to function
# average_cosine_similarity_context_vectors<-function(target_corpus, dsm, window_length=15, words_to_analyze=50,sim_count=1000){
target_corpus=piperCorps
dsm=jp.dsm.proj
window_length=15
words_to_analyze=50
sim_count=1000
  #Transpose and convert dsm into indexical dataframe, only take first svd
  dsn_df=data.frame(t(dsm[,1]))
  
  
  #for each document in corpus, get all potential context_vectors
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
      context_names=context_names[context_names %in% colnames(dsn_df)]
      
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
  
  
  #Get most common words
  most_freq_words=data.frame(table(context_vector_df$target))
  most_freq_words=most_freq_words[with(most_freq_words, order(-Freq)), ]
  most_freq_words=most_freq_words$Var[1:words_to_analyze]
  
  #Loop through each word in words_to_analyze
  average_cosine_similarity<-list(rep(0, words_to_analyze))
  for (word_id in 1:words_to_analyze){
    
    #Extract words
    search_word=most_freq_words[word_id]
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
      dsm.reduce=dsn_df[,unique_words]
      
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
    average_cosine_similarity[word_id]=apply(cosine_results_x,1,mean)
  }
  
#   return(context_vector,average_cosine_similarity)
# }

#Test function
c(cv_test, cosine_test)<-average_cosine_similarity_context_vectors(data_johnpiper,jp.dsm.proj, window_length=15, words_to_analyze=50,sim_count=1000)
