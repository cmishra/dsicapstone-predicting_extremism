library(tm)
source("methods.R")

data_wbc <- VCorpus(DirSource("../webscraping westboro/sermons/"))
processed_wbc <- preprocessing(data_wbc, stpwds, wordStem)


