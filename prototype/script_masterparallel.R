


runParallelPrototype <- function(filepath, binNum, resample = F,
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
                    'compiler','Rcpp','RcppArmadillo','stringr','plyr', 'openNLP', 'lsa',
                    'topicmodels')
  
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
  
  install.packages('prototype/method/wordspace_0.1-14.tar.gz', repos = NULL, type="source")
  print(paste0("loading ","wordspace","..."))
  library(wordspace)
  # Load Functions from Script
  
  function.sources = list.files('./prototype/method', pattern="method_")
  function.sources = sapply(function.sources, function(x) paste0('./prototype/method/', x))
  sapply(function.sources,source,.GlobalEnv)
  
  # Get List of Groups in Folder Structure
  files <- list.files('./data_dsicap')
  groups  <- files[files != 'ref']
  print(groups)
  
  # Get name of All Groups & All Subgroups
  
  subgroupList<-c()
  for (group in groups) {
    # print(group)
    for (type in c('train','test')){
      indices <- read.csv(paste0('./data_dsicap/',group,'/RData/',type,'_split.csv'))
      for (i in seq(1,nrow(indices))){
        groupID <- paste0(group,"_",type,i)
        print(groupID)
        subgroupList<-c(subgroupList,groupID)
      }
    }
  }
  
  id = NULL
  group = NULL
  
  groupID <- subgroupList[binNum]
  bin <- str_split(groupID,"_")
  id <- bin[[1]][2]
  group <- bin[[1]][1]
  type <- gsub("[^a-z]","",id)
  index <- gsub("[^0-9]","",id)
  
  
  # Create Randomize Test/Train Index for Each Group
#   if(resample == T){
#     
#     for (group in groups) {
#       testTrainSplit(paste0('./data_dsicap/',group), 10)
#     }
#     
#   }

  
  # Create RData files containing Corpii of Each Random Group
  if(tokenize == T) {
    start <- proc.time()
    
    indices <- read.csv(paste0('./data_dsicap/',group,'/RData/',type,'_split.csv'))
    binnedCorpus <- createBinnedCorpus(paste0('./data_dsicap/',group), unlist(indices[index,]))
    preprocessDocuments(binnedCorpus,filepath = paste0('./data_dsicap/',group),datafile_name = paste0(type,index))
    
    
    tokenize.TIMED = (proc.time()-start)
  }
 
  # Get the 20 Target Words for BOW Baseline Analysis

  if(getTopWords == T) {
    start <- proc.time()

    indices <- read.csv(paste0('./data_dsicap/',group,'/RData/',type,'_split.csv'))
    binnedCorpus <- createBinnedCorpus(paste0('./data_dsicap/',group), unlist(indices[index,]))
    topWords <- getTopAdjAdv(binnedCorpus) #, paste0(group,"_bin",gsub("[^0-9]","",binnedTokens)))  
    write.csv(topWords[1:20,], paste0('./data_dsicap/',group,'/RData/','targetWords_',type,index,'.csv'))
          
    remove(binnedCorpus)
    
    getTopWords.TIMED = (proc.time()-start)
  }
  

  
  # Calculate Sentiment Baseline Signal
  if(sentiment == T){
    start = proc.time()
    
    tokName<-paste0('processedTokens_', id,'.RData')
    load(paste0('./data_dsicap/',group,'/RData/',tokName))
    #load(paste0('./data_dsicap/',group,'/RData/',binnedTokens))
    
    metrics <- calc_Sentiment(processedTokens, groupID)
    
    sigName = "sentiment"
    fileName = paste0("./data_dsicap/ref/signal_",sigName,"_",groupID,".csv")
    write.csv(metrics, fileName)
    
    sentiment.TIMED = (proc.time()-start)
  }
  
  
  
  # Calculate Judgement Signal
  if(judgements == T){
    start = proc.time()
    
    indices <- read.csv(paste0('./data_dsicap/',group,'/RData/',type,'_split.csv'))

    binnedCorpus <- createBinnedCorpus(paste0('./data_dsicap/',group), unlist(indices[index,]))
    metrics <- create_Judgements(binnedCorpus, groupID)

    sigName = "judgements"
    fileName = paste0("./data_dsicap/ref/signal_",sigName,"_",groupID,".csv")
    write.csv(metrics, fileName)
    
    judgements.TIMED = (proc.time()-start)
  }
  
  if(createCo == T){
    start = proc.time()
          
    #Load processed tokens
    tokName<-paste0('processedTokens_', id,'.RData')
    load(paste0('./data_dsicap/',group,'/RData/',tokName))
    
    filepath <- paste0('./data_dsicap/',group,'/RData/')
    output_cooccurrences(processedTokens, filepath, id, window_length = 15)
    remove(processedTokens)
    
    createCo.TIMED = (proc.time()-start) 
  }
  
  # Create RData files containing DSM projections of Each Random Group
  if(createDSM == T){
    start = proc.time()
    
    # Load WordCo
    wordCoName<-paste0('wordCo_',id,'.RData')
    load(paste0('./data_dsicap/',group,'/RData/',wordCoName))
    
    filepath<-paste0('./data_dsicap/',group,'/RData/')
    createDSM(filepath,id,wordCo)

    remove(wordCo)
    createDSM.time = (proc.time()-start)
  }
  
  #Quantify context vectors
  if (semContext == T){
    
    start = proc.time()
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


    remove(processedTokens)
    remove(dsmProj)
    remove(targetWords)
    
    sigName = "semContext"
    fileName = paste0("./data_dsicap/ref/signal_",sigName,"_",groupID,".csv")
    write.csv(metrics, fileName)
    
    semContent.TIMED = (proc.time()-start)
  }
  
  #Quantify network, ACOM
  if (semACOM == T){
    start = proc.time()
    #Load DSM
    dsmName<-paste0('dsmProj_', id,'.RData')
    load(paste0('./data_dsicap/',group,'/RData/',dsmName))
    
    # Load WordCo
    wordCoName<-paste0('wordCo_',id,'.RData')
    load(paste0('./data_dsicap/',group,'/RData/',wordCoName))
    
    #Load targetWords
    targetWord_fileName <- paste0("targetWords","_",id,".csv")
    targetWords <- read.csv(paste0('./data_dsicap/',group,'/RData/',targetWord_fileName), stringsAsFactors = F)
    
    metrics <- tot_frequency_DSM(wordCo, dsmProj, targetWords$Var1[1:20], groupID)
    
    remove(dsmProj)
    remove(targetWords)
    
    sigName = "semACOM"
    fileName = paste0("./data_dsicap/ref/signal_",sigName,"_",groupID,".csv")
    write.csv(metrics, fileName)
    
    semACOM.TIMED = proc.time() - start
  }

  vars <- ls()
  vars = vars[str_detect(vars,"TIMED")] 
  varValues = c()
  for (i in vars){
    varValues <- c(varValues,get(i)[[3]])
  }
  varHrs <- (varValues*328)/3600
  varsDF <- data.frame(vars,varValues, varHrs)
  write.csv(varsDF,'./data_dsicap/ref/timeit.csv')


  if (network == T){
    start = proc.time()
    
    checkandloadlibrary('igraph')
    
    #Load DSM
    dsmName<-paste0('dsmProj_', id,'.RData')
    load(paste0('./data_dsicap/',group,'/RData/',dsmName))
    
    # Load WordCo
    wordCoName<-paste0('wordCo_',id,'.RData')
    load(paste0('./data_dsicap/',group,'/RData/',wordCoName))
    
    #Load targetWords
    targetWord_fileName <- paste0("targetWords","_",id,".csv")
    targetWords <- read.csv(paste0('./data_dsicap/',group,'/RData/',targetWord_fileName), stringsAsFactors = F)
    
    
    metrics <- network_signal(dsmProj, targetWords, groupID)
          
    # remove(processedTokens)
    remove(dsmProj)
    remove(targetWords)

    sigName = "network"
    fileName = paste0("./data_dsicap/ref/signal_",sigName,"_",groupID,".csv")
    write.csv(metrics, fileName)

    network.TIMED = (proc.time()-start)
  }

  vars <- ls()
  vars = vars[str_detect(vars,"TIMED")] 
  varValues = c()
  for (i in vars){
    varValues <- c(varValues,get(i)[[3]])
  }
  varHrs <- (varValues*328)/3600
  varsDF <- data.frame(vars,varValues, varHrs)
  write.csv(varsDF,'./data_dsicap/ref/timeit.csv')

}

