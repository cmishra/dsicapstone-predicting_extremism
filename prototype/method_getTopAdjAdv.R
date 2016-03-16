#' @filepath: filepath to the collection of data
#' 

getTopAdjAdv <- function(dataCorpus) {
  
  df.posTags <- c()
  
  for (i in 1:length(dataCorpus)) {
    y <- get.pos.tag(dataCorpus[[i]]$content)
    df.posTags <- rbind(df.posTags, y)
  }
  
  df.posTags <- subset(df.posTags, df.posTags$Word != "")
  df.posTags <- subset(df.posTags, df.posTags$Word != " ")
  df.posTags['Word'] <- apply(df.posTags['Word'], 1, removePunctuation)
  
  # Remove Rows of Empty Strings Created by Pre-Processing Above
  df.posTags <- df.posTags[-which(df.posTags['Word'] == ""),]
  
  # Calculate Length of Number of Words in Corpus
  zlength <- nrow(df.posTags)
  #print(zlength)
  #print(class(zlength))
  
  df.posTags <- subset(df.posTags, df.posTags$Tag == "JJ" |  df.posTags$Tag == "JJR" |  df.posTags$Tag == "JJS" | df.posTags$Tag == "RB" |  df.posTags$Tag == "RBR" |  df.posTags$Tag == "RBS")
  df.posTags['Word'] <- apply(df.posTags['Word'], 1, str_to_lower) # convert to lower
  df.posTags['Word'] <- apply(df.posTags['Word'], 1, removeNumbers)
  df.posTags['Word'] <- apply(df.posTags['Word'], 1, str_trim) # remove white space
  df.posTags['Word'] <- apply(df.posTags['Word'], 1, removeWords, stpwds)
  df.posTags['Word'] <- apply(df.posTags['Word'], 1, wordStem) # stem words
  
  # Remove Rows of Empty Strings Created by Pre-Processing Above
  df.posTags <- df.posTags[-which(df.posTags['Word'] == ""),]
  
  tagCount = table(df.posTags$Word)
  tagCount <- as.data.frame(tagCount)
  tagCount <- tagCount[order(tagCount$Freq, decreasing = T),]
  #topWords = tagCount[1:20,"Var1"]
  # print(nrow(tagCount))
  # print(class(tagCount))
  # Note this is rounded to integer
  tagCount['Freq'] <- tagCount['Freq']/zlength # Normalize by the Number of Words in the 
  #topWords <- list(topWords,zlength,adjlength,advlength)
  
  # write.csv(tagCount, "targetwords.csv")
  # write(zlength, paste("Ref/",strsplit(filepath,"/")[[1]][1],"/length.txt",sep = ""))
  
  return(tagCount)
  
}

