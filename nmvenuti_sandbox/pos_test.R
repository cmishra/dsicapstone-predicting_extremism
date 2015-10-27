library(rJava)
library("NLP")
library("openNLP")
## Extract text


setwd('C:/Users/nmvenuti/Desktop/UVA MSDS/Fall/DS 6001/Data Sources/westboro_john_piper_sermons')
#extract all desired files
file.list <- list.files(pattern = "\\.txt")
head(file.list)


## Set sentence and word token annotations.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
#pos_tag_annotator <-   pos_tag_annotator <- Maxent_POS_Tag_Annotator(probs = TRUE) #turned off for this exercise
pos_tag_annotator <- Maxent_POS_Tag_Annotator()


#Set Function to get POS tags for documents

get.pos.tag<-function(file.name){

  #get text
  s<- paste(readLines(file.name), collapse=" ")
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
  df.posTags$file.name<-file.name
  
  #word
  df.posTags$Word<-s[a3w]
  
  #tag
  df.posTags$Tag<-tags
  
  #subset data
  df.posTags<-df.posTags[,c('file.name','Word','Tag')]
  
  return(df.posTags)
}

#test function
x<-get.pos.tag(file.list[1])
head(x)
nrow(x)
#remove test
# rm(x)
# 
# #test function on multiple files
# for (i in file.list[1:5]){
#   print(i)
#   y<-get.pos.tag(i)
#   ifelse(!exists("z"),z<-y,z<-rbind(z,y))
# }
# 
# head(z)
# tail(z)
# nrow(z)
# unique(z$file.name)
# rm(z)


#perform rip on entire dataset
start.time<-proc.time()
for (i in file.list){
  y<-get.pos.tag(i)
  ifelse(!exists("z"),z<-y,z<-rbind(z,y))
}
runtime<-proc.time()[1]-start.time[1]
print(paste("Runtime for section 1: ",runtime, " Seconds", sep=""))

head(z)
tail(z)
nrow(z)
length(unique(z$file.name))
length(file.list)
file.output<-data.frame(cbind(z$features,z$file.name,z$Word))
file.output<-data.frame(lapply(file.output, as.character), stringsAsFactors=FALSE)
write.csv(z, file = "pos_test")

data.pulled<-read.csv("pos_test.csv",header=TRUE)

#adjective analysis
head(z)
df.adj<-z[ with(z,  grepl("JJ", Tag)),]
nrow(df.adj)

head(df.adj)

#unique filesnames/adjetives
df.adj.unique<-unique(df.adj[,c("file.name","Word")])
nrow(df.adj.unique)
df.adj.counts<-data.frame(table(df.adj[,c("Word")]))
df.adj.counts[order(Freq),] 
write.csv(df.adj, file = "adj_list")


#adverb analysis
head(z)
df.adv<-z[ with(z,  grepl("RB", Tag)),]
nrow(df.adv)

df.adv.unique<-unique(df.adv[,c("file.name","Word")])
nrow(df.adv.unique)

head(df.adv)
length(unique(df.adv[,c("Word")]))
write.csv(df.adv, file = "adv_list")


