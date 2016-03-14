testTrainSplit <- function(filepath,binsize)
{
  fileCount <- length(list.files(paste0(filepath,'/raw')))
  trainsample <- sample(seq(from=1,to=fileCount),.7*fileCount)
  testsample <- sample(seq(from=1,to=fileCount))[-trainsample]
  
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
  #print(alldocs)
  
  alldocs_train <- t(as.data.frame(alldocs[1]))
  #row.names(alldocs_train) <- seq(nrow(alldocs_train))
  write.csv(alldocs_train,paste(filepath,"/RData/train_split.csv",sep=""),row.names = FALSE)
  
  alldocs_test <- t(as.data.frame(alldocs[2]))
  row.names(alldocs_test) <- seq(nrow(alldocs_test))
  write.csv(alldocs_test,paste(filepath,"/RData/test_split.csv",sep=""),row.names = FALSE)
}

