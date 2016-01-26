library(stringr)
library(XML)
base_url <- "http://www.sermoncentral.com/sermons"

if (!exists("webscrape.cache") && !file.exists("webscrape.cache.RData")) {
  webscrape.cache <- list()
} else if (!exists("webscrape.cache")) {
  load("webscrape.cache.RData")
} else {
  save(webscrape.cache,  file="webscrape.cache.RData")
}

webscrape <- function(link) {
  if (!link %in% names(webscrape.cache)) {
    connection <- url(link)
    webscrape.cache[[link]] <<- paste(readLines(connection), collapse="")
    close(connection)
  }
  webscrape.cache[[link]]
}

return_toc_url_piper <- function(page) {
  paste0(base_url,
         "/SearchResults.asp?AudienceAge=&Denomination=&Series=&Page=",
         page,
         "&Sort=rank&IsOutline=2&MinRating=&MaxResults=&keyword=&ScriptureBookA2=&ScriptureVerse2=&TopicID=0&series2=&audienceage2=&denomination2=&lang2=&since2=0&Audio=0&BodyFlag=0&ContributorID=42295&MultimediaTypeID="
         )
}

write.to.file <- function(file_path, content) {
  connection <- file(file_path)
  writeLines(content, connection)
  close(connection)
}

return_sermon_url<- function(sermon, page) {
  paste0(base_url,
         sermon,
         "?Page=",
         page)
}