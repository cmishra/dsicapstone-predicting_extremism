setwd("Dropbox/College/4_Fourth_Year/Capstone/")

tmp_data_folder <- "chetan_mishra_sandbox/tmp_data"

# attach libs
requiredLibs <- c('tm','RWeka','SnowballC','parallel','data.table',
                  'compiler','Rcpp','RcppArmadillo','stringr','plyr', 'openNLP', 'lsa',
                  'topicmodels', 'igraph')
for (lib in requiredLibs) library(lib, character.only=TRUE)
library(wordspace)

# source
for (file in list.files("prototype/method", pattern=".*\\.R")) {
  source(paste('prototype', 'method', file, sep='/'))
}

# preprocess
sermons <- list.files("data_dsicap/", recursive = T, pattern='.*\\.txt')
sermons <- paste0("data_dsicap/", sermons)
corpus <- VCorpus(URISource(sermons))
preprocessDocuments(corpus, tmp_data_folder, "")

# topwords
topwords <- c("test", "god", "jesus", "islam", "donkey")

# cooccurrences
load(paste0(tmp_data_folder, '/RData/processedTokens_.RData'))
output_cooccurrences(processedTokens, tmp_data_folder, "", 15)

# dsm
load(paste0(tmp_data_folder, '/wordCo_.RData'))
createDSM(tmp_data_folder, "", wordCo, local_run=T)

# networks
load(paste0(tmp_data_folder, '/dsmProj_.RData'))
rm(wordCo, processedTokens)
metrics <- network_signal(dsmProj, topwords, "123")

# ACOM
tot_frequency_DSM(wordCo, dsmProj, topwords, "")
