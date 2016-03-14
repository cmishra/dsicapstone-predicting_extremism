library(tm)
library(caTools)
library(rpart)
library(stringr)

source('preprocessing.R')
setwd("C:/Users/brian/Documents/UVA/Capstone/Groups")


#############################################
#
# Get target words from training set
#
#############################################

targetwords <- list.files("Ref", pattern = ".csv")
targetlengths <- list.files("Ref", pattern = ".txt")

targetDF <- NULL
remove(targetDF)
totallength <- 0

for(i in 1:length(targetwords))
{
  df <- read.csv(paste("Ref/",targetwords[i], sep=""))
  length <- read.table(paste("Ref/",targetlengths[i], sep=""))[1,1]
  totallength <- totallength + length
  ifelse(!exists("targetDF"),targetDF<-df,targetDF<-rbind(targetDF,df))
  
}


targetDF <- targetDF[,c(2,3)]
targetDF$Var1 <- as.character(targetDF$Var1)
targetDF <- aggregate(. ~ Var1, data=targetDF, FUN=sum)
targetDF$Freq <- targetDF$Freq/totallength
targetDF = targetDF[order(-targetDF$Freq),]
targetWords = targetDF[1:20,"Var1"]



########################################
#
# now read in corpus and create model
#
########################################

groups <- list.dirs(".",recursive = FALSE)
groups <- groups[groups != "./ref"]

doccorpus <- VCorpus(VectorSource(NULL))

for(group in groups)
{
  
  traindocs <- list.files(paste0(group,"/RData"))[str_detect(list.files(paste0(group,"/RData")),"_train")]

  for(i in 1:length(traindocs))
  {
    load(paste0(group,"/RData/",traindocs[i]))
    doctemp <- NULL
    for(j in 1:length(processedTokens))
      {
        doctemp <- c(doctemp,processedTokens[[j]]$content)
      }
    tempcorpus <- Corpus(VectorSource(list(doctemp)))
    doccorpus <- c(doccorpus,tempcorpus)
    meta(doccorpus[[length(doccorpus)]],"id") <- paste0(group,"_train",i)
  }

}


dtm <-DocumentTermMatrix(doccorpus, control = list(weighting = function(x) weightSMART(x, spec = "nnc"),dictionary = targetWords))
labeledTerms <- as.data.frame(as.matrix(dtm))
remove('dtm')

group_scores <- c(4, 4, 1, 8, 3, 2, 6, 4, 7, 1)
elasticity <- NULL
for(i in 1:length(groups))
{
  traindocs <- list.files(paste0(groups[i],"/RData"))[str_detect(list.files(paste0(groups[i],"/RData")),"_train")]
  elasticity <- c(elasticity,rep(group_scores[i],length(traindocs)))
  
}

labeledTerms$elasticity = elasticity

#Splitting into train and test data using caTools

# set.seed(1738)
# 
# spl = sample.split(labeledTerms$elasticity, 0.7)
# 
# train = subset(labeledTerms, spl == TRUE)
# test = subset(labeledTerms, spl == FALSE)

#Build Model
model <- rpart(elasticity ~ ., data = labeledTerms)
model$variable.importance

# create test set
testcorpus <- VCorpus(VectorSource(NULL))

for(group in groups)
{
  
  testdocs <- list.files(paste0(group,"/RData"))[str_detect(list.files(paste0(group,"/RData")),"_test")]
  
  for(i in 1:length(testdocs))
  {
    load(paste0(group,"/RData/",testdocs[i]))
    doctemp <- NULL
    for(j in 1:length(processedTokens))
    {
      doctemp <- c(doctemp,processedTokens[[j]]$content)
    }
    tempcorpus <- Corpus(VectorSource(list(doctemp)))
    testcorpus <- c(testcorpus,tempcorpus)
    meta(testcorpus[[length(testcorpus)]],"id") <- paste0(group,"_test",i)
  }
  
}

testdtm <-DocumentTermMatrix(testcorpus, control = list(weighting = function(x) weightSMART(x, spec = "nnc"),dictionary = targetWords))
testlabeledTerms <- as.data.frame(as.matrix(testdtm))

group_scores <- c(4, 4, 1, 8, 3, 2, 6, 4, 7, 1)
testelasticity <- NULL
for(i in 1:length(groups))
{
  testdocs <- list.files(paste0(groups[i],"/RData"))[str_detect(list.files(paste0(groups[i],"/RData")),"_test")]
  testelasticity <- c(testelasticity,rep(group_scores[i],length(testdocs)))
  
}

# testlabeledTerms$elasticity = testelasticity

# create predictions
preds <- predict(model, newdata = testlabeledTerms)
#assign(paste("accuracy",binlength,sep=""),
accuracy <- sum(round(preds, 0) == testelasticity) / length(preds)


