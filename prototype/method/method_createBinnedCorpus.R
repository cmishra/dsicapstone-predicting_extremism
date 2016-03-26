createBinnedCorpus <- function(filepath,indexlist)
{
  #traindocs <- read.csv(paste(filepath,"/RData/train_split.csv",sep=""),header = TRUE)
  #traindocs <- traindocs[,-c(1)]
  indexlist <- indexlist[!is.na(indexlist)]
  indexlist <- as.numeric(indexlist)
  dataCorpus <- VCorpus(DirSource(paste(filepath,"/raw",sep="")))
  binnedCorpus <- dataCorpus[indexlist]
  return(binnedCorpus)
} 