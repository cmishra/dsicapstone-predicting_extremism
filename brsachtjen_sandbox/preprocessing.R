##############################
#####Preprocessing Script#####
##############################

#Define libraries
library(tm)
#library(wordspace)
#library(RWeka)
library(SnowballC)
#library(parallel)
library(data.table)
library(compiler)
library(Rcpp)
library(RcppArmadillo)
library(stringr)
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(openNLP)
#install.packages("NLP")

#Functions from methods lib developed by Chetan Mishra
stpwds <-stopwords()

removePunctuationExceptPeriod <- cmpfun(function(x) {
  gsub("[^[:alpha:][:space:].?!]", "", x)
})

individualizeEndOfSent <- cmpfun(function(x) {
  str_replace_all(x,"[.?!]+", " . ")
})

simpleTokenizer <- cmpfun(function(x) {
  unlist(strsplit(x, split="[[:space:]]+"))
})

removeEmptyStrings <- cmpfun(function(x) {
  x<-gsub("[\r\n]", "", x)
  x[!x == ""]
  x[!nchar(x)==0]
})

testTrainSplit <- function(filepath,binsize)
{
  trainsample <- sample(seq(from=1,to=length(list.files(filepath))),.7*length(list.files(filepath)))
  testsample <- sample(seq(from=1,to=length(list.files(filepath))))[-trainsample]
  
  trainsplit <- split(trainsample, ceiling(seq_along(trainsample)/binsize))
  testsplit <- split(testsample, ceiling(seq_along(testsample)/binsize))
  
  traindocs <- NULL
  if(length(trainsample)%%binsize < binsize/2)
  {
    for(i in 1:(length(trainsplit)-1))
    {
      if(length(trainsplit[[i]]) < binsize)
      {
         difference <- binsize - length(trainsplit[[i]])
         trainsplit[[i]] <- c(trainsplit[[i]],rep(NA,difference))
      }
      traindocs[i] <- trainsplit[i]
    }
  }
  
  if(length(trainsample)%%binsize >= binsize/2)
  {
    for(i in 1:length(trainsplit))
    {
      if(length(trainsplit[[i]]) < binsize)
      {
        difference <- binsize - length(trainsplit[[i]])
        trainsplit[[i]] <- c(trainsplit[[i]],rep(NA,difference))
      }
      traindocs[i] <- trainsplit[i]
    }
  }
  
  testdocs <- NULL
  if(length(testsample)%%binsize < binsize/2)
  {
    for(i in 1:(length(testsplit)-1))
    {
      if(length(testsplit[[i]]) < binsize)
      {
        difference <- binsize - length(testsplit[[i]])
        testsplit[[i]] <- c(testsplit[[i]],rep(NA,difference))
      }
      testdocs[i] <- testsplit[i]
    }
  }
  
  if(length(testsample)%%binsize >= binsize/2)
  {
    for(i in 1:length(testsplit))
    {
      if(length(testsplit[[i]]) < binsize)
      {
        difference <- binsize - length(testsplit[[i]])
        testsplit[[i]] <- c(testsplit[[i]],rep(NA,difference))
      }
      testdocs[i] <- testsplit[i]
    }
  }
  
  alldocs <- list(traindocs,testdocs)
  #print(traindocs)
  #print(testdocs)
  print(alldocs)
  
  alldocs_train <- t(as.data.frame(alldocs[1]))
  #row.names(alldocs_train) <- seq(nrow(alldocs_train))
  write.csv(alldocs_train,paste(strsplit(filepath,"/")[[1]][1],"/RData/train_split.csv",sep=""))
  
  alldocs_test <- t(as.data.frame(alldocs[2]))
  row.names(alldocs_test) <- seq(nrow(alldocs_test))
  write.csv(alldocs_test,paste(strsplit(filepath,"/")[[1]][1],"/RData/test_split.csv",sep=""),row.names = FALSE)
}

createBinnedCorpus <- function(filepath,indexlist)
{
  #traindocs <- read.csv(paste(filepath,"/RData/train_split.csv",sep=""),header = TRUE)
  #traindocs <- traindocs[,-c(1)]
  indexlist <- indexlist[!is.na(indexlist)]
  indexlist <- as.numeric(indexlist)
  dataCorpus <- VCorpus(DirSource(paste(filepath,"/Raw",sep="")))
  binnedCorpus <- dataCorpus[indexlist]
  return(binnedCorpus)
}



preprocessDocuments<-function(corpus,datafile_name){
  
  #dataCorpus <- getCorpus("Westboro")
  #Define corpuses for data
  #filepath <- "WBC"
  
  #filepath <- "NaumanKhan/raw"
  #binsize <- 10

  #thesample <- sample(seq(from=1,to=length(dataCorpus)))
  #docsplit <- cut(thesample,length(dataCorpus)/binsize)
  #doccorpus <- Corpus(VectorSource(NULL))
  
  #for(i in 1:length(levels(docsplit)))
  #{
#     
#     temp <- which(as.numeric(docsplit)==i)
#     tempcorpus <- dataCorpus[temp]
#     doctemp <- NULL
#     #paste(tempcorpus[[2]][1])
#     for(j in 1:length(tempcorpus))
#     {
#       doctemp <- paste(doctemp,tempcorpus[[j]][1])
#     }
#     tempcorpus <- Corpus(VectorSource(doctemp))
#     
    processedStrings<-tm_map(corpus, content_transformer(individualizeEndOfSent))
    processedStrings<-tm_map(processedStrings, content_transformer(removePunctuationExceptPeriod))
    processedStrings<-tm_map(processedStrings, content_transformer(tolower))
    processedStrings<-tm_map(processedStrings, content_transformer(removeNumbers))
    processedTokens<-tm_map(processedStrings, content_transformer(simpleTokenizer))
    #processedTokens<-tm_map(processedTokens, content_transformer(removeWords), stpwds)
    processedTokens<-tm_map(processedTokens, content_transformer(wordStem))
    processedTokens<-tm_map(processedTokens, content_transformer(removeEmptyStrings))
    
   # doccorpus <- c(doccorpus,processedTokens)
  #}
  
  
  #Preprocess combined documents into clean strings
  #Normalizes
  
  
  #return(processedTokens)
  #Save backup of processed strings
  #save(processedStrings, file=paste0(filepath,'/Rdata/processedStrings.RData'))
  
  #Save backup of processed tokens
  save(processedTokens, file=paste0(filepath,'/Rdata/processedTokens_',datafile_name,'.RData'))
  
}
# filepath="C:/Users/nmvenuti/Desktop/UVA MSDS/Capstone/webscraping westboro/"
# dataCorpus <- VCorpus(DirSource(paste(filepath,"Data",sep="/")))
# dataCorpus[[1]]$content

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

get.pos.tag<-function(string){
  
  
  #get text
  s<- paste(string, collapse=" ")
  s<-as.String(s)
  #Annotate words
  a <- annotate(s, list(sent_token_annotator, word_token_annotator))
  
  # Create dataframe with POS tag probabilities off.
  a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
  a3 <- annotate(s, pos_tag_annotator, a2)
  a3w <- subset(a3, type == "word")
  
  df.posTags<- data.frame(a3w)
  
  tags <- sapply(a3w$features, `[[`, "POS")
  
  
  #Add document name to data frame
  #df.posTags$file.name<-file.name
  
  #word
  df.posTags$Word<-s[a3w]
  
  #tag
  df.posTags$Tag<-tags
  
  #subset data
  df.posTags<-df.posTags[,c('Word','Tag')]
  
  return(df.posTags)
}














#filepath <- "NaumanKhan/raw"


#filepath <- "Westboro"
#file.list <- file.list[1:5]

getTopAdjAdv <- function(filepath) {
  
  dataCorpus <- VCorpus(DirSource(filepath))
  filepath = "DorothyDay/raw"
  x <- c(1,2)
  dataCorpus[[2]]
  #dataCorpus <- VCorpus(VectorSource("God is so good"))
  #dataCorpus <- VCorpus(DirSource("Lenin/raw/1"))
  #Preprocess combined documents into clean strings
  #Normalizes
  #processedStrings<-tm_map(dataCorpus, content_transformer(individualizeEndOfSent))
  #processedStrings<-tm_map(processedStrings, content_transformer(removePunctuationExceptPeriod))
  #processedStrings<-tm_map(processedStrings, content_transformer(tolower))
  #processedStrings<-tm_map(processedStrings, content_transformer(removeNumbers))
  #processedTokens<-tm_map(processedStrings, content_transformer(simpleTokenizer))
  #processedTokens<-tm_map(processedTokens, content_transformer(removeWords), stpwds)
  #processedTokens<-tm_map(processedTokens, content_transformer(wordStem))
  #processedTokens<-tm_map(processedTokens, content_transformer(removeEmptyStrings))
  
  z <- NULL
  remove(z)
  
  #file.list <- list.files(path = path, pattern = 'txt', all.files = T,
  #                       full.names = T, ignore.case = T, include.dirs = T)
  #for (i in file.list){
  # y<-get.pos.tag(i)
  #ifelse(!exists("z"),z<-y,z<-rbind(z,y))
  #}
  
  for (i in 1:length(dataCorpus))
  { y<-get.pos.tag(dataCorpus[[i]]$content)
  ifelse(!exists("z"),z<-y,z<-rbind(z,y))
  }
  
  # clean up docs
  zlength <- nrow(z)
  z <- subset(z, z$Tag == "JJ" |  z$Tag == "JJR" |  z$Tag == "JJS" | z$Tag == "RB" |  z$Tag == "RBR" |  z$Tag == "RBS")
  z <- subset(z, !z$Word %in% stpwds)
  z <- subset(z, z$Word != "")
  z <- subset(z, z$Word != " ")
  #z <- subset(z, z$Freq > 100)
  adjlength <- nrow(subset(z, z$Tag == "JJ" |  z$Tag == "JJR" |  z$Tag == "JJS"))
  advlength <- nrow(subset(z, z$Tag == "RB" |  z$Tag == "RBR" |  z$Tag == "RBS"))
  #remove(z)
  
  z1 <- VCorpus(VectorSource(z))
  
  processedStrings<-tm_map(z1, content_transformer(individualizeEndOfSent))
  processedStrings<-tm_map(processedStrings, content_transformer(removePunctuationExceptPeriod))
  processedStrings<-tm_map(processedStrings, content_transformer(tolower))
  processedStrings<-tm_map(processedStrings, content_transformer(removeNumbers))
  processedStrings <-tm_map(processedStrings, content_transformer(wordStem))
  emptyStrings <- which(processedStrings[[1]]$content == "")
  processedStrings <-tm_map(processedStrings, content_transformer(removeEmptyStrings))
  processedStrings[[2]]$content <- processedStrings[[2]]$content[-emptyStrings]
  
  tagCount = data.frame(processedStrings[[1]]$content,processedStrings[[2]]$content)
  tagCount = table(tagCount$processedStrings..1...content)
  tagCount <- as.data.frame(tagCount)
  tagCount = tagCount[order(-tagCount$Freq),]
  topWords <- tagCount
  #topWords = tagCount[1:20,"Var1"]
  #topWords <- as.character(topWords)
  #topWords <- list(topWords,zlength,adjlength,advlength)
  
  write.csv(topWords, paste("Ref/",strsplit(filepath,"/")[[1]][1],"/targetwords.csv",sep = ""))
  write(zlength, paste("Ref/",strsplit(filepath,"/")[[1]][1],"/length.txt",sep = ""))
  
  return(topWords)
  
}
