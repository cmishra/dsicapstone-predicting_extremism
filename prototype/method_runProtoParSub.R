
runPrototype <- function(filepath, resample = F,
                       tokenize = F,
                       sentiment= F,
                       getTopWords = F,
                       judgements = F,
                       BOW = F,
                       createCo = F,
                       createDSM = F,
                       semContext = F,
                       semACOM = F,
                       network = F) {
  
  # All Required Packages
  # Need to figure out 'wordspace'
  requiredLibs <- c('tm','RWeka','SnowballC','parallel','data.table',
                    'compiler','Rcpp','RcppArmadillo','stringr','plyr', 'openNLP', 'lsa')
  
  
  
  checkandloadlibrary <- function(package){
    if(!package %in% .packages(all.available = TRUE)){
      print(paste0("installing ",package,"..."))
      install.packages(package, repos = 'http://cran.cnr.berkeley.edu/')
    }
    print(paste0("loading ",package,"..."))
    library(package, character.only = T)
  }
  
  for (lib in requiredLibs){
    checkandloadlibrary(lib)
  }
  
  install.packages('prototype/wordspace_0.1-14.tar.gz', repos = NULL, type="source")
  print(paste0("loading ","wordspace","..."))
  library(wordspace)
  # Load Functions from Script
  
  function.sources = list.files('./prototype', pattern="method_")
  function.sources = sapply(function.sources, function(x) paste0('./prototype/', x))
  sapply(function.sources,source,.GlobalEnv)
  
  # Get List of Groups in Folder Structure
  files <- list.files('./data_dsicap')
  groups  <- files[files != 'ref']
  print(groups)
  
  # Create Randomize Test/Train Index for Each Group
  if(resample == T){
    
    for (group in groups) {
      testTrainSplit(paste0('./data_dsicap/',group), 10)
    }
    
  }
  
  # Create RData files containing Corpii of Each Random Group
  if(tokenize == T) {
  
    for (group in groups) {
      print(group)
      for (type in c('train','test')){
        indices <- read.csv(paste0('./data_dsicap/',group,'/RData/',type,'_split.csv'))
        for (i in seq(1,nrow(indices))){
          binnedCorpus <- createBinnedCorpus(paste0('./data_dsicap/',group), unlist(indices[i,]))
          preprocessDocuments(binnedCorpus,filepath = paste0('./data_dsicap/',group),datafile_name = paste0(type,i))
        }
      }
    }
    
  }
  
  # Get the 20 Target Words for BOW Baseline Analysis
  if(getTopWords == T) {
    print("here")
    # start <- proc.time()
    # Get Top Adj/Adv from all Group's in Training Corpus
    allTopAdjAdv <- c()
    for (group in groups) {
      print(group)
      for (type in c('train','test')){
        indices <- read.csv(paste0('./data_dsicap/',group,'/RData/',type,'_split.csv'))
        for (i in seq(1,nrow(indices))){
          print(i)
          # Load in Preprocesse=d Strings to Process for Sentiment
          # load('./WBC/RData/processedStrings.RData')
          binnedCorpus <- createBinnedCorpus(paste0('./data_dsicap/',group), unlist(indices[i,]))
          topWords <- getTopAdjAdv(binnedCorpus) #, paste0(group,"_bin",gsub("[^0-9]","",binnedTokens)))
          
          write.csv(topWords[1:20,], paste0('./data_dsicap/',group,'/RData/','targetWords_',type,i,'.csv'))
          
          # Mod topWords to allow for aggregation
#           rownames(topWords) <- topWords[,'Var1']
#           topWords <- t(topWords['Freq'])
#           topWordsDF <- as.data.frame(topWords)
#           
#           allTopAdjAdv <- rbind.fill(allTopAdjAdv,topWordsDF)
#           #sentimentMetrics <- rbind(sentimentMetrics, adjAdv)
          
          remove(binnedCorpus)
        }
      }
    }
#     finalTopAdjAdv <- colSums(allTopAdjAdv, na.rm = TRUE)
#     finalTop20AdjAdv <- sort(finalTopAdjAdv, decreasing = TRUE)[1:50]
#     write.csv(finalTop20AdjAdv, "./data_dsicap/ref/top20AdjAdv.csv")
    #proc.time() - start
    
  }
  
  
  # Calculate Sentiment Baseline Signal
  if(sentiment == T){
    
    sentimentMetrics <- c()
    for (group in groups) {
      print(group)
      for (type in c('train','test')){
        dataFiles <- list.files(paste0('./data_dsicap/',group,"/RData"))
        IDs <- unique(str_extract(dataFiles,paste0(type,"[0-9]+")))
        IDs <- IDs[!is.na(IDs)]
        for (id in IDs) {
          groupID <- paste0(group,"_",id)
          print(groupID)
          
          # Load in Preprocessed Strings to Process for Sentiment
          # load('./WBC/RData/processedStrings.RData')
          #Load binnedTokens
          tokName<-paste0('processedTokens_', id,'.RData')
          load(paste0('./data_dsicap/',group,'/RData/',tokName))
          #load(paste0('./data_dsicap/',group,'/RData/',binnedTokens))
          
          metrics <- calc_Sentiment(processedTokens, groupID)
          sentimentMetrics <- rbind(sentimentMetrics, metrics)
          
          remove(processedTokens)
        }
      }
    }
    write.csv(sentimentMetrics, "./data_dsicap/ref/signal_sentiment.csv")
    
  }
  
  
  
  # Calculate Judgement Signal
  if(judgements == T){
    
    jugMetrics <- c()
    for (group in groups) {
      print(group)
      for (type in c('train','test')){
        indices <- read.csv(paste0('./data_dsicap/',group,'/RData/',type,'_split.csv'))
        for (i in seq(1,nrow(indices))){
          print(i)

          groupID <- paste0(group,"_",type,i)
          print(groupID)
          # Load in Preprocessed Strings to Process for Sentiment
          # load('./WBC/RData/processedStrings.RData')
          
          # load binnedTokens
          start <- proc.time()
          binnedCorpus <- createBinnedCorpus(paste0('./data_dsicap/',group), unlist(indices[i,]))
          metrics <- create_Judgements(binnedCorpus, groupID)
          proc.time()-start
          jugMetrics <- rbind(jugMetrics, metrics)
          
          remove(binnedCorpus)
        }
      }
    }
    write.csv(jugMetrics, "./data_dsicap/ref/signal_judgements.csv")
    
  }
  
  # Calculate BOW Signal
  
  # Calculate Semantic Density (Context Vector) Signal
  # ** quantifyContextVectors() ** CORPUS
  
  # Calculate Semantic Density (ACOM) Signal
  
  # Calculate Network Quant Signal
  


  if(createCo == T){
    # Create RData files containing Co-occurences of Each Random Group
    for (group in groups) {
      print(group)
      for (type in c('train','test')){
        dataFiles <- list.files(paste0('./data_dsicap/',group,"/RData"))
        IDs <- unique(str_extract(dataFiles,paste0(type,"[0-9]+")))
        IDs <- IDs[!is.na(IDs)]
        for (id in IDs) {
          groupID <- paste0(group,"_",id)
          print(groupID)
          
          #Load processed tokens
          tokName<-paste0('processedTokens_', id,'.RData')
          load(paste0('./data_dsicap/',group,'/RData/',tokName))
          
          filepath <- paste0('./data_dsicap/',group,'/RData/')
          output_cooccurrences(processedTokens, filepath, groupID, window_length = 15)
          remove(processedTokens)
        }
      }
    }
  }
  
  # Create RData files containing DSM projections of Each Random Group
  if(createDSM == T){
    for (group in groups) {
      print(group)
      for (type in c('train','test')){
        dataFiles <- list.files(paste0('./data_dsicap/',group,"/RData"))
        IDs <- unique(str_extract(dataFiles,paste0(type,"[0-9]+")))
        IDs <- IDs[!is.na(IDs)]
        for (id in IDs) {
          groupID <- paste0(group,"_",id)
          print(groupID)
          
          
          # Load WordCo
          wordCoName<-paste0('wordCo_',id,'.RData')
          load(paste0('./data_dsicap/',group,'/RData/',wordCoName))
          
          filepath<-paste0('./data_dsicap/',group,'/RData/')
          createDSM(filepath,groupID,wordCo)
          
          print("completed bin")
          remove(wordCo)
        }
      }
    }
  }
  
  #Quantify context vectors
  if (semContext == T){
    cvMetrics <- c()
    for (group in groups) {
      print(group)
      for (type in c('train','test')){
        dataFiles <- list.files(paste0('./data_dsicap/',group,"/RData"))
        IDs <- unique(str_extract(dataFiles,paste0(type,"[0-9]+")))
        IDs <- IDs[!is.na(IDs)]
        for (id in IDs) {
          groupID <- paste0(group,"_",id)
          print(groupID)
          
          #Load processed tokens
          tokName<-paste0('processedTokens_', id,'.RData')
          load(paste0('./data_dsicap/',group,'/RData/',tokName))
          
          #Load DSM
          dsmName<-paste0('dsmProj_', id,'.RData')
          load(paste0('./data_dsicap/',group,'/RData/',dsmName))
          
          #Load targetWords
          targetWord_fileName <- paste0("targetWords","_",id,".csv")
          targetWords <- read.csv(paste0('./data_dsicap/',group,'/RData/',targetWord_fileName), stringsAsFactors = F)
         
          metrics <- quantifyContext(filepath,groupID,processedTokens,dsmProj,targetWords$Var1[1:10],minMatches=25,window_length=15,sim_count=1000)
          cvMetrics <- rbind(cvMetrics, metrics)

          remove(processedTokens)
          remove(dsmProj)
          remove(targetWords)
          
        }
      }
    }
    write.csv(cvMetrics, "./data_dsicap/ref/signal_semContext.csv")
    print("Finished Sem Density - Context Vectors")
  }
  
  #Quantify network, ACOM
  if (semACOM == T){
    
    
    
    acom_All <- c()
    # network_All<- c()
    for (group in groups) {
      print(group)
      for (type in c('train','test')){
        dataFiles <- list.files(paste0('./data_dsicap/',group,"/RData"))
        IDs <- unique(str_extract(dataFiles,paste0(type,"[0-9]+")))
        IDs <- IDs[!is.na(IDs)]
        for (id in IDs) {
          groupID <- paste0(group,"_",id)
          print(groupID)
          
          #Load DSM
          dsmName<-paste0('dsmProj_', id,'.RData')
          load(paste0('./data_dsicap/',group,'/RData/',dsmName))
          
          # Load WordCo
          wordCoName<-paste0('wordCo_',id,'.RData')
          load(paste0('./data_dsicap/',group,'/RData/',wordCoName))
          
          #Load targetWords
          targetWord_fileName <- paste0("targetWords","_",id,".csv")
          targetWords <- read.csv(paste0('./data_dsicap/',group,'/RData/',targetWord_fileName), stringsAsFactors = F)
          
          acom_result <- tot_frequency_DSM(wordCo, dsmProj, targetWords$Var1[1:20], groupID)
          acom_All <- rbind(acom_All, acom_result)
          # network_result <- network_signal(dsmProj, groupID)
          # network_All <- rbind(network_All, network_result)
          
          # remove(processedTokens)
          remove(dsmProj)
          remove(targetWords)
        }
      }
    }
    
    write.csv(acom_All, "./data_dsicap/ref/signal_semACOM.csv")
    # write.csv(network_All, "./data_dsicap/ref/signal_network.csv")
    print("THE GRAND CONCLUSION")
  }

  if (network == T){
    
    checkandloadlibrary('igraph')
    
    network_All<- c()
    for (group in groups) {
      print(group)
      for (type in c('train','test')){
        dataFiles <- list.files(paste0('./data_dsicap/',group,"/RData"))
        IDs <- unique(str_extract(dataFiles,paste0(type,"[0-9]+")))
        IDs <- IDs[!is.na(IDs)]
        for (id in IDs) {
          groupID <- paste0(group,"_",id)
          print(groupID)
          
          #Load DSM
          dsmName<-paste0('dsmProj_', id,'.RData')
          load(paste0('./data_dsicap/',group,'/RData/',dsmName))
          
          # Load WordCo
          wordCoName<-paste0('wordCo_',id,'.RData')
          load(paste0('./data_dsicap/',group,'/RData/',wordCoName))
          
          #Load targetWords
          targetWord_fileName <- paste0("targetWords","_",id,".csv")
          targetWords <- read.csv(paste0('./data_dsicap/',group,'/RData/',targetWord_fileName), stringsAsFactors = F)
          
          # acom_result <- tot_frequency_DSM(wordCo, dsmProj, targetWords[1:20], groupID)
          # acom_All <- rbind(acom_All, acom_result)
          network_result <- network_signal(dsmProj, groupID)
          network_All <- rbind(network_All, network_result)
          
          # remove(processedTokens)
          remove(dsmProj)
          remove(targetWords)
        }
      }
    }
    write.csv(network_All, "./data_dsicap/ref/signal_network.csv")
    # print("THE GRAND CONCLUSION")
  }

}
