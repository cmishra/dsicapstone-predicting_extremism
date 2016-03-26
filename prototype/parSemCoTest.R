
parSemCo <- function(filepath,i) {
  
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
  

  subgroupList<-c()
  for (group in groups) {
    print(group)
    for (type in c('train','test')){
      dataFiles <- list.files(paste0('./data_dsicap/',group,"/RData"))
      IDs <- unique(str_extract(dataFiles,paste0(type,"[0-9]+")))
      IDs <- IDs[!is.na(IDs)]
      for (id in IDs) {
        groupID <- paste0(group,"_",id)
        print(groupID)
        subgroupList<-c(subgroupList,groupID)
      }
    }
  }
  
  id = NULL
  group = NULL
  
  groupID <- subgroupList[i]
  bin <- str_split(groupID,"_")
  id <- bin[[1]][2]
  group <- bin[[1]][1]

  #Load processed tokens
  tokName<-paste0('processedTokens_', id,'.RData')
  load(paste0('./data_dsicap/',group,'/RData/',tokName))
  
  #Load DSM
  dsmName<-paste0('dsmProj_', id,'.RData')
  load(paste0('./data_dsicap/',group,'/RData/',dsmName))
  
  #Load targetWords
  #Load targetWords
  targetWord_fileName <- paste0("targetWords","_",id,".csv")
  targetWords <- read.csv(paste0('./data_dsicap/',group,'/RData/',targetWord_fileName), stringsAsFactors = F)
  #targetName<-paste0('targetWords_',gsub("processedTokens_", "", binnedTokens))
  #load(paste0('./data_dsicap/',group,'/RData/',targetName))
  
  metrics <- quantifyContext(filepath,groupID,processedTokens,dsmProj,targetWords$Var1[1:10],minMatches=25,window_length=10,sim_count=1000)
  

  write.csv(metrics, paste0("./data_dsicap/ref/signal_semContext_",groupID,".csv"))
  print("Finished Sem Density - Context Vectors")
}
