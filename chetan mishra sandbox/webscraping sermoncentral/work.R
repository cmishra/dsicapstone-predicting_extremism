setwd("Capstone/chetan mishra sandbox/webscraping sermoncentral/")
source("methods.R")

# to capture internal urls
piper_pages <- 1:75
sermons <- unlist(lapply(piper_pages[1:2], function(pageNum) {
  toc_page <- webscrape(return_toc_url_piper(pageNum))
  matches <- unlist(str_extract_all(toc_page, 'Read the full sermon:.+?.asp'))
  matches <- unlist(str_extract(matches, "/.+\\.asp"))
  matches <- str_replace(matches, '/sermons', "")
  unique(matches)
}))

# to capture outside urls
sermons <- c(sermons, unlist(lapply(piper_pages[2:length(piper_pages)], function(pageNum) {
  toc_page <- webscrape(return_toc_url_piper(pageNum))
  matches <- unlist(str_extract_all(toc_page, 'outsideurl.+?htm'))
  matches <- str_extract(matches, 'http:.+?htm')
  unique(matches)
})))

# delete one bad link
sermons <- sermons[-c(487, 572)]

# For internal urls webscraping
unlist(lapply(sermons[1:11], function(sermon) {
  page <- webscrape(return_sermon_url(sermon, 1))
  max_num <- max(str_extract(str_extract_all(page, "\\d</a></li>")[[1]], "\\d"))
  txt <- paste(unlist(lapply(1:max_num, function(i) {
    page <- webscrape(return_sermon_url(sermon, i))
    txt <- str_extract(page, '<div id="TheSermonText.+?</div>')
    str_replace_all(txt, "<.+?>", "")
  })), collapse=" ")
  file_name <- str_replace_all(sermon, "/|\\.asp", "")
  write.to.file(paste0("john_piper/",file_name, '.txt'), txt)
}))

# for outside url scraping
unlist(lapply(sermons[12:length(sermons)], function(sermon) {
  print(which(sermon ==  sermons))
  page <- str_extract(webscrape(sermon), "<BLOCKQUOTE>.+</BLOCKQUOTE>")
  if (is.na(page))
    page <- str_extract(webscrape(sermon), "<blockquote>.+</blockquote>")
  if (is.na(page)){
    page <- webscrape(sermon)
    page <- str_replace_all(page, "<STYLE+?/STYLE>", "")
    page <- str_extract_all(page, '<div id="wb_\\d{6}Text1.+?COPYRIGHT John Piper')
  }
  page <- str_replace_all(page, "<SCRIPT+?/SCRIPT>", "")
  page <- str_replace_all(page, "<STYLE+?/STYLE>", "")
  page <- str_replace_all(page, "<!--.+?-->", "")
  page <- str_replace_all(page, "<.+?>", " ")
  page <- str_replace_all(page, "&nbsp;", "\n")
  page <- str_replace_all(page, "&quot;", '')
  page <- str_replace(page, "Piper's NotesPIPER'S NOTES SERMON LIBRARY.+?&quot;", "")
  file_name <- str_extract(sermon, "/piper[0-9k]{2}/.+?\\.")
  file_name <- str_trim(str_replace_all(file_name, "/", " "), "both")
  write.to.file(paste0("john_piper/",
                       file_name, 
                       "txt"), page)
}))

sum(is.)