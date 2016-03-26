#setwd("C:\\Users\\brian\\Documents\\UVA\\Capstone\\Git\\Git2")

# Calculate LDA Signal
if(LDA == T){
  traincorpus <- VCorpus(VectorSource(NULL))
  testcorpus <- VCorpus(VectorSource(NULL))
  rowids <- NULL

  for (group in groups) {
    print(group)
    for (type in c('train','test')){
      dataFiles <- list.files(paste0('./data_dsicap/',group,"/RData"))
      IDs <- unique(str_extract(dataFiles,paste0(type,"[0-9]+")))
      IDs <- IDs[!is.na(IDs)]
      for (id in IDs) {
        print(id)
        # Load in Preprocessed Strings to Process for Sentiment
        # load('./WBC/RData/processedStrings.RData')
        #Load DSM
        tokName<-paste0('processedTokens_', id,'.RData')
        load(paste0('./data_dsicap/',group,'/RData/',tokName))
        #load(paste0('./data_dsicap/',group,'/RData/',binnedTokens))
        #processedTokens <- VCorpus(VectorSource(processedTokens))
        #metrics <- calc_Sentiment(processedTokens, paste0(group,"_",id))
        if(type=="train")
        {
          traincorpus <- c(traincorpus,processedTokens)
        }
        
        if(type=="test")
        {
          testcorpus <- c(testcorpus,processedTokens)
        }

        remove(processedTokens)
      }
     
      
    }
    trainids <- unlist(transpose(read.csv(paste0('./data_dsicap/',group,'/RData/',"train","_split.csv"))))
    trainids <- trainids[!is.na(trainids)]
    
    testids <- unlist(transpose(read.csv(paste0('./data_dsicap/',group,'/RData/',"test","_split.csv"))))
    testids <- testids[!is.na(testids)]
    
    rowids <- c(rowids,
                lapply(trainids, function(x) { paste0(group,"_",x)}), 
                lapply(testids, function(x) { paste0(group,"_",x)}))
    
  }
  
  lda_data <- run_LDA(traincorpus, k=20, sparsity=.85, testcorpus)
  write.csv(lda_data, "./data_dsicap/ref/lda_signal.csv")
  
}

