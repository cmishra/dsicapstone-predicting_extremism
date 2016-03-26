sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

get.pos.tag<-function(string){
  
  
  #get text
  s<- paste(string, collapse=" ")
  s<-as.String(s)
  #Annotate words
  a <- annotate(s, list(sent_token_annotator, word_token_annotator))
  
  # Create dataframe with POS tag probabilities off.
  a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
  a3 <- annotate(s, pos_tag_annotator, a2)
  a3w <- subset(a3, type == "word")
  
  df.posTags<- data.frame(a3w)
  
  tags <- sapply(a3w$features, `[[`, "POS")
  
  
  #Add document name to data frame
  #df.posTags$file.name<-file.name
  
  #word
  df.posTags$Word<-s[a3w]
  
  #tag
  df.posTags$Tag<-tags
  
  #subset data
  df.posTags<-df.posTags[,c('Word','Tag')]
  
  return(df.posTags)
}