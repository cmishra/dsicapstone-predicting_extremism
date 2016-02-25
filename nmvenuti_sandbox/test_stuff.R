##################
#####Test Doc#####
##################

setwd("~/GitHub/dsicapstone-predicting_extremism/nmvenuti_sandbox")
source("preprocessing.R")

#Test preprocessing
filepath="C:/Users/nmvenuti/Desktop/UVA MSDS/Capstone/webscraping westboro/"

preprocessDocuments(filepath)

#Check preprocessing worked
load(paste0(filepath,'Rdata/processedStrings.RData'))
load(paste0(filepath,'Rdata/processedTokens.RData'))