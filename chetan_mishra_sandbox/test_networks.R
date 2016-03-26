setwd("Dropbox/College/4_Fourth_Year/Capstone/")
source("prototype/method_network.R")
library(data.table)
library(wordspace)
library(igraph)

load("data_dsicap/MehrBaba/RData/dsmProj_test1.RData")
target_words <- data.table(read.csv("data_dsicap/MehrBaba/RData/targetWords_test1.csv"))[1:20,
                                                                                        as.character(Var1)]
network_signal(dsmProj, target_words, 'test')
