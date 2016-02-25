##################
#####Test Doc#####
##################

setwd("~/GitHub/dsicapstone-predicting_extremism/nmvenuti_sandbox")
source("preprocessing.R")

#Test preprocessing
filepath="C:/Users/nmvenuti/Desktop/UVA MSDS/Capstone/webscraping westboro/"

preprocessDocuments(filepath)

#Check preprocessing worked
load(paste(filepath,'Rdata/processedStrings.RData',sep="/"))
load(paste(filepath,'Rdata/processedTokens.RData',sep="/"))
load(paste(filepath,'Rdata/wordCocurrences.RData',sep="/"))