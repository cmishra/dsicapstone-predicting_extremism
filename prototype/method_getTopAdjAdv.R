#' @filepath: filepath to the collection of data
#' 

getTopAdjAdv <- function(dataCorpus) {
  
  z <- NULL
  remove(z)
  
  for (i in 1:length(dataCorpus)) {
    if (is.na(corpus[[i]]$content[2] == F)) {
      y <- get.pos.tag(dataCorpus[[i]]$content)
      ifelse(!exists("z"), z<-y, z<-rbind(z,y))
    }
  }
  
  # ** Do we want to normalize by the total length, pre-processed?
  zlength <- nrow(z)
  
  z <- subset(z, z$Tag == "JJ" |  z$Tag == "JJR" |  z$Tag == "JJS" | z$Tag == "RB" |  z$Tag == "RBR" |  z$Tag == "RBS")
  # z <- subset(z, !z$Word %in% stpwds)
  z <- subset(z, z$Word != "")
  z <- subset(z, z$Word != " ")
  df.posTags <- z
  #z <- subset(z, z$Freq > 100)
  # adjlength <- nrow(subset(z, z$Tag == "JJ" |  z$Tag == "JJR" |  z$Tag == "JJS"))
  # advlength <- nrow(subset(z, z$Tag == "RB" |  z$Tag == "RBR" |  z$Tag == "RBS"))
  #remove(z)
  
  df.posTags['Word'] <- apply(df.posTags['Word'], 1, str_to_lower) # convert to lower
  df.posTags['Word'] <- apply(df.posTags['Word'], 1, removeNumbers)
  df.posTags['Word'] <- apply(df.posTags['Word'], 1, removePunctuation)
  df.posTags['Word'] <- apply(df.posTags['Word'], 1, str_trim) # remove white space
  df.posTags['Word'] <- apply(df.posTags['Word'], 1, removeWords, stpwds)
  df.posTags['Word'] <- apply(df.posTags['Word'], 1, wordStem) # stem words
  
  # Remove Rows of Empty Strings Created by Pre-Processing Above
  df.posTags <- df.posTags[-which(df.posTags['Word'] == ""),]
  
  # tagCount = data.frame(processedStrings[[1]]$content,processedStrings[[2]]$content)
  tagCount = table(df.posTags$Word)
  tagCount <- as.data.frame(tagCount)
  tagCount <- tagCount[order(tagCount$Freq, decreasing = T),]
  #topWords = tagCount[1:20,"Var1"]

  topWords <- tagCount/zlength # Normalize by the Number of Words in the 
  #topWords <- list(topWords,zlength,adjlength,advlength)
  
  write.csv(tagCount, "targetwords.csv")
  # write(zlength, paste("Ref/",strsplit(filepath,"/")[[1]][1],"/length.txt",sep = ""))
  
  return(tagCount)
  
}

