
runPrototype <- function(filepath, resample = F,
                       tokenize = F,
                       sentiment= F,
                       getTopWords = F,
                       judgements = F,
                       BOW = F,
                       createCo = F,
                       createDSM = F,
                       semContext = F) {
  
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
      for (type in c('test')){
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
        bins <- dataFiles[str_detect(dataFiles,paste0(type,"[0-9]+"))]
        for (binnedTokens in bins) {
          print(binnedTokens)
          # Load in Preprocessed Strings to Process for Sentiment
          # load('./WBC/RData/processedStrings.RData')
          load(paste0('./data_dsicap/',group,'/RData/',binnedTokens))
          
          metrics <- calc_Sentiment(processedTokens, paste0(group,"_",type,gsub("[^0-9]","",binnedTokens)))
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
        dataFiles <- list.files(paste0('./data_dsicap/',group,"/RData"))
        bins <- dataFiles[str_detect(dataFiles,paste0(type,"[0-9]+"))]
        for (binnedTokens in bins) {
          print(binnedTokens)
          # Load in Preprocessed Strings to Process for Sentiment
          # load('./WBC/RData/processedStrings.RData')
          load(paste0('./data_dsicap/',group,'/RData/',binnedTokens))
          
          metrics <- create_Judgements(processedTokens, paste0(group,"_",type,gsub("[^0-9]","",binnedTokens)))
          jugMetrics <- rbind(jugMetrics, metrics)
          
          remove(processedTokens)
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
        bins <- dataFiles[str_detect(dataFiles,paste0('processedTokens_',type,"[0-9]+"))]
        for (binnedTokens in bins) {
          load(paste0('./data_dsicap/',group,'/RData/',binnedTokens))
          datafile_name<-paste0(type,gsub("[^0-9]","",binnedTokens))
          filepath <- paste0('./data_dsicap/',group,'/RData/')
          output_cooccurrences(processedTokens, filepath, datafile_name, window_length = 15)
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
        print(dataFiles)
        bins <- dataFiles[str_detect(dataFiles,paste0('wordCo_',type,"[0-9]+"))]
        print(bins)
        for (binnedCooccurences in bins) {
          print(binnedCooccurences)
          print(paste0('./data_dsicap/',group,'/RData/',binnedCooccurences))
          load(paste0('./data_dsicap/',group,'/RData/',binnedCooccurences))
          datafile_name<-paste0(type,gsub("[^0-9]","",binnedCooccurences))
          filepath<-paste0('./data_dsicap/',group,'/RData/')
          createDSM(filepath,datafile_name,wordCo)
          
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
        bins <- dataFiles[str_detect(dataFiles,paste0('processedTokens_',type,"[0-9]+"))]
        for (binnedTokens in bins) {
          print(binnedTokens)
          
          #Load processed tokens
          load(paste0('./data_dsicap/',group,'/RData/',binnedTokens))
          
          #Load DSM
          dsmName<-paste0('dsmProj_',gsub("processedTokens_", "", binnedTokens))
          load(paste0('./data_dsicap/',group,'/RData/',dsmName))
          
          #Load targetWords
          targetWord_fileName <- paste0("targetWords","_",type,gsub("[^0-9]","",binnedTokens),".csv")
          targetWords <- read.csv(paste0('./data_dsicap/',group,'/RData/',targetWord_fileName), stringsAsFactors = F)
          #targetName<-paste0('targetWords_',gsub("processedTokens_", "", binnedTokens))
          #load(paste0('./data_dsicap/',group,'/RData/',targetName))
          
          metrics <- quantifyContext(filepath,datafile_name,processedTokens,dsmProj,targetWords$Var1[1:20],minMatches=25,window_length=15,sim_count=1000)
          cvMetrics <- rbind(cvMetrics, metrics)
          
          remove(processedTokens)
          remove(dsmProj)
          remove(targetWords)
        }
      }
    }
    write.csv(cvMetrics, "./data_dsicap/ref/signal_semContext.csv")
  }

}
