setwd("../../../4_Fourth Year/Capstone/")
library(ggplot2)
library(data.table)
library(stringr)

dat <- data.table(read.csv("test (1).csv"))
str(dat)

dat[,.(group_size=length(author), flair=str_sub(author_flair_text, end=50)), 
  by=author_flair_text][order(group_size, decreasing=T)][1:100]

dat2 <- data.table(read.csv("15_08_11_250k"))
orgs <- dat2[,.(num_comments=length(author), flair=str_sub(author_flair_text, end=30),
  group_size=length(unique(author))), 
  by=.(author_flair_text, subreddit)][
    order(group_size, decreasing=T), 
    .(group_size, flair, subreddit, 
      num_comments)][
  1:100]

dat2[,.(v1=author_flair_text == ""), by=subreddit][
  ,.(num_missing = sum(v1), .N), by=subreddit]

dat2[, .(num_missing = sum(author_flair_text == ""), 
  num_has = sum(!author_flair_text == ""))]
