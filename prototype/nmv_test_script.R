#All functions within script_master and packages used by quantify context to NMV's knowledge
setwd("~/GitHub/dsicapstone-predicting_extremism")
library(lsa)

library(tm)
library(wordspace)
library(RWeka)
library(SnowballC)
library(parallel)
library(data.table)
library(compiler)
library(Rcpp)
library(RcppArmadillo)
# library(testthat)
library(stringr)
source("prototype/method_preprocessing.R")#Libraries in this package bring into main script

source('prototype/method_quantifyContext.R')


########################################
#####Needed for testing will remove#####
########################################

filepath="C:/Users/nmvenuti/Desktop/UVA MSDS/Capstone/webscraping westboro"
datafile_name='test_1'
minMatches=25
window_length=15
most_freq_words=c('god','lord')
sim_count=1000

# #Create processed_tokens fils
# dataCorpus <- VCorpus(DirSource(paste(filepath,"/Raw",sep="")))
# preprocessDocuments(dataCorpus,filepath,datafile_name)
# 
# #Test output of cooccurrences
# 
# createWordCoOccurences(filepath,datafile_name,processedTokens)
# # dsmProj<-dsmProjTest 
# 
# #Test DSM function
# load(paste(filepath,"/Rdata/wordCooccurences_",datafile_name,'.Rdata',sep=""))
# wordCo<-wordCooccurences
# createDSM(filepath,datafile_name,wordCo)
# 
# #Test cv function
# load(paste0(filepath, "/Rdata/dsmProj_", datafile_name, ".RData"))
# 
# testCV<-quantifyContext(filepath,datafile_name,processedTokens,dsmProj,most_freq_words,minMatches=25,window_length=15,sim_count=1000)

# Create RData files containing Co-occurences of Each Random Group
for (group in groups) {
  print(group)
  for (type in c('train','test')){
    dataFiles <- list.files(paste0('./data_dsicap/',group,"/RData"))
    bins <- dataFiles[str_detect(dataFiles,paste0('processedTokens_',type,"[0-9]+"))]
    for (binnedTokens in bins) {
      load(paste0('./data_dsicap/',group,'/RData/',binnedTokens))
      datafile_name<-paste0(group,"_",type,gsub("[^0-9]","",binnedTokens))
      filepath<-'./data_dsicap/'
      createWordCoOccurences(filepath,datafile_name,processedTokens)
      
      remove(processedTokens)
    }
  }
}

# Create RData files containing DSM projections of Each Random Group
for (group in groups) {
  print(group)
  for (type in c('train','test')){
    dataFiles <- list.files(paste0('./data_dsicap/',group,"/RData"))
    bins <- dataFiles[str_detect(dataFiles,paste0('wordCooccurences_',type,"[0-9]+"))]
    for (binnedCooccurences in bins) {
      print(binnedCooccurences)
      load(paste0('./data_dsicap/',group,'/RData/',binnedCooccurences))
      datafile_name<-paste0(group,"_",type,gsub("[^0-9]","",binnedCooccurences))
      filepath<-'./data_dsicap/'
      createDSM(filepath,datafile_name,wordCo)
      
      remove(wordCooccurences)
    }
  }
}

#Quantify context vectors
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
      targetName<-paste0('targetWords_',gsub("processedTokens_", "", binnedTokens))
      load(paste0('./data_dsicap/',group,'/RData/',targetName))
      
      metrics <- quantifyContext(filepath,datafile_name,target_corpus,dsmProj,targetWords$Var1[1:20],minMatches=25,window_length=15,sim_count=1000)
      cvColumns<-paste('word_',1:length(most_freq_words),sep="")
      colnames(testCV)<-c('groupName',metrics)
      cvMetrics <- rbind(cvMetrics, metrics)
      
      remove(processedTokens)
      remove(dsmProj)
      remove(targetWords)
    }
  }
}
