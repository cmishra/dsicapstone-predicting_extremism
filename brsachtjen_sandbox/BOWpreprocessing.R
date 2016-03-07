library(tm)
library(caTools)
library(rpart)

setwd("C:/Users/brian/Documents/UVA/Capstone")
source('preprocessing.R')

# split groups into training and testing sets
binsize <- 10
randomDocSequence("DorothyDay/raw",binsize)
randomDocSequence("JohnPiper/raw",binsize)
randomDocSequence("Lenin/raw",binsize)
randomDocSequence("MehrBaba/raw",binsize)
randomDocSequence("NaumanKhan/raw",binsize)
randomDocSequence("PastorAnderson/raw",binsize)
randomDocSequence("Rabbinic/raw",binsize)
randomDocSequence("Shepherd/raw",binsize)
randomDocSequence("Unitarian/raw",binsize)
randomDocSequence("WBC/raw",binsize)


# find top adjectives and adverbs - from training sets
DorothyDay <- getTopAdjAdv("DorothyDay/raw")
DorothyDay_targetwords <- DorothyDay[[1]]
DorothyDay_length <- DorothyDay[[2]]
write.csv(DorothyDay_targetwords, paste("Ref/DorothyDay_targetwords",".csv",sep = ""))
write(DorothyDay_length, paste("Ref/DorothyDay_length",".txt",sep = ""))

JohnPiper <- getTopAdjAdv("JohnPiper/raw")
JohnPiper_targetwords <- JohnPiper[[1]]
JohnPiper_length <- JohnPiper[[2]]
write.csv(JohnPiper_targetwords, paste("Ref/JohnPiper_targetwords",".csv",sep = ""))
write(JohnPiper_length, paste("Ref/JohnPiper_length",".txt",sep = ""))

Lenin_targetwords <- NULL
Lenin_length <- 0
for(i in 1:length(list.files("Lenin/raw", pattern = ".txt")))
{
  Lenin <- getTopAdjAdv(paste("Lenin/raw/",i,sep=""))
  Lenin_targetwords <- rbind(Lenin_targetwords,Lenin[[1]])
  Lenin_length <- Lenin_length + Lenin[[2]]
}
write.csv(Lenin_targetwords, paste("Ref/Lenin_targetwords",".csv",sep = ""))
write(Lenin_length, paste("Ref/Lenin_length",".txt",sep = ""))

MehrBaba <- getTopAdjAdv("MehrBaba/raw")
MehrBaba_targetwords <- MehrBaba[[1]]
MehrBaba_length <- MehrBaba[[2]]
write.csv(MehrBaba_targetwords, paste("Ref/MehrBaba_targetwords",".csv",sep = ""))
write(MehrBaba_length, paste("Ref/MehrBaba_length",".txt",sep = ""))

NaumanKhan <- getTopAdjAdv("NaumanKhan/raw")
NaumanKhan_targetwords <- NaumanKhan[[1]]
NaumanKhan_length <- c(NaumanKhan[[2]],NaumanKhan[[3]],NaumanKhan[[4]])
write.csv(NaumanKhan_targetwords, paste("Ref/NaumanKhan_targetwords",".csv",sep = ""))
write(NaumanKhan_length, paste("Ref/NaumanKhan_length",".txt",sep = ""))

PastorAnderson <- getTopAdjAdv("PastorAnderson/raw")
PastorAnderson_targetwords <- PastorAnderson[[1]]
PastorAnderson_length <- PastorAnderson[[2]]
write.csv(PastorAnderson_targetwords, paste("Ref/PastorAnderson_targetwords",".csv",sep = ""))
write(PastorAnderson_length, paste("Ref/PastorAnderson_length",".txt",sep = ""))

rabbinic <- getTopAdjAdv("Rabbinic/raw")
rabbinic_targetwords <- rabbinic[[1]]
rabbinic_length <- rabbinic[[2]]
write.csv(rabbinic_targetwords, paste("Ref/rabbinic_targetwords",".csv",sep = ""))
write(rabbinic_length, paste("Ref/rabbinic_length",".txt",sep = ""))

Shepherd <- getTopAdjAdv("Shepherd/raw")
Shepherd_targetwords <- Shepherd[[1]]
Shepherd_length <- Shepherd[[2]]
write.csv(Shepherd_targetwords, paste("Ref/Shepherd_targetwords",".csv",sep = ""))
write(Shepherd_length, paste("Ref/Shepherd_length",".txt",sep = ""))

Unitarian <- getTopAdjAdv("Unitarian/raw")
Unitarian_targetwords <- Unitarian[[1]]
Unitarian_length <- Unitarian[[2]]
write.csv(Unitarian_targetwords, paste("Ref/Unitarian_targetwords",".csv",sep = ""))
write(Unitarian_length, paste("Ref/Unitarian_length",".txt",sep = ""))

WBC <- getTopAdjAdv("WBC/raw")
WBC_targetwords <- WBC[[1]]
WBC_length <- WBC[[2]]
write.csv(WBC_targetwords, paste("Ref/WBC_targetwords",".csv",sep = ""))
write(WBC_length, paste("Ref/WBC_length",".txt",sep = ""))



  
