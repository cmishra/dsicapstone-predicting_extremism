
# All Required Packages
# Need to figure out 'wordspace'
requiredLibs <- c('tm','RWeka','SnowballC','parallel','data.table',
                  'compiler','Rcpp','RcppArmadillo','stringr','plyr', 'openNLP')

checkandloadlibrary <- function(package){
  if(!package %in% .packages(all.available = TRUE)){
    print(paste0("installing ",package,"..."))
    install.packages(package)
  }
  print(paste0("loading ",package,"..."))
  library(package, character.only = T)
}

for (lib in requiredLibs){
  checkandloadlibrary(lib)
}


# Load Functions from Script
setwd('/Users/hopeemac/Documents/Code/GIT')
source('dsicapstone-predicting_extremism/prototype/method_preprocessing.R')
source('dsicapstone-predicting_extremism/prototype/method_testTrainSplit.R')
source('dsicapstone-predicting_extremism/prototype/method_createBinnedCorpus.R')
source('dsicapstone-predicting_extremism/prototype/method_getTopAdjAdv.R')
source('dsicapstone-predicting_extremism/prototype/method_get.pos.tag.R')



# Bin & Preprocess Documents
files <- list.files('./data_dsicap')
groups  <- files[files != 'ref']
print(groups)


# Create Randomize Test/Train Index for Each Group
for (group in groups) {
  testTrainSplit(paste0('./data_dsicap/',group), 10)
}

# Create RData files containing Corpii of Each Random Group
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


# Get Top Adj/Adv from all Group's in Training Corpus
allTopAdjAdv <- c()
for (group in groups) {
  print(group)
  for (type in c('train')){
    indices <- read.csv(paste0('./data_dsicap/',group,'/RData/',type,'_split.csv'))
    for (i in seq(1,nrow(indices))){
      print(i)
      # Load in Preprocesse=d Strings to Process for Sentiment
      # load('./WBC/RData/processedStrings.RData')
      binnedCorpus <- createBinnedCorpus(paste0('./data_dsicap/',group), unlist(indices[i,]))
      topWords <- getTopAdjAdv(binnedCorpus) #, paste0(group,"_bin",gsub("[^0-9]","",binnedTokens)))
      
      # Mod topWords to allow for aggregation
      rownames(topWords) <- topWords[,'Var1']
      topWords <- t(topWords['Freq'])
      topWordsDF <- as.data.frame(topWords)
      
      allTopAdjAdv <- rbind.fill(allTopAdjAdv,topWordsDF)
      #sentimentMetrics <- rbind(sentimentMetrics, adjAdv)
      
      remove(binnedCorpus)
    }
  }
}
finalTopAdjAdv <- colSums(allTopAdjAdv, na.rm = TRUE)
finalTop20AdjAdv <- sort(finalTopAdjAdv, decreasing = TRUE)[1:50]
write.csv(finalTop20AdjAdv, "./data_dsicap/ref/top20AdjAdv.csv")


# Calculate Sentiment Baseline Signal
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




# Calculate Judgement Signal
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


# Calculate Semantic Density (Context Vector) Signal
** quantifyContextVectors() ** CORPUS

semMetrics <- c()
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


