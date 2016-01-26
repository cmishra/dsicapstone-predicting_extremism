setwd("Capstone/github/chetan mishra sandbox/webscraping westboro/")
source("webscraping westboro_methods.R")

# get html on page with sermon links
sermon_links <- "http://www.godhatesfags.com/sermons/outlines/"
sermon_links_html <- get_webpage_html(sermon_links)
sermon_pdfs <- unique(unlist(
  str_extract_all(sermon_links_html, "Sermon_\\d{8}.pdf")
))

# turn sermon pdfs into text
# note: code written for windows
# text_cache <- list()
load("text_cache.RData")
missing_files <- c(
  "Sermon_20090118.pdf",
  "Sermon_20070520.pdf"
)
sermon_pdfs <- setdiff(sermon_pdfs, missing_files)
lapply(sermon_pdfs, function(pdf) {
  pdf_link <- paste0(sermon_links, pdf)
  if (!pdf_link %in% names(text_cache)) {
    download.file(url=pdf_link, 
                  destfile="temp_pdf.pdf", 
                  method="auto",
                  quiet=F,
                  mode="wb")
    text_cache[[pdf_link]] <<- pdf_to_text("temp_pdf.pdf", "pdftotext.exe")
  }
}) 
save(text_cache, file="text_cache.RData")

# output code
text <- rep("", length.out=length(text_cache))
for (i in seq_along(text_cache)) {
   text[i] <- paste(text_cache[[i]], collapse=" ")
}
text <- data.table(data.frame(content=text, doc=paste0("WestboroBaptist_", sermon_pdfs)))
text[,"content":=as.character(content)]
text <- text[str_length(content) > 20]
for (i in 1:nrow(text)) {
  write(x=text[i,content], file=paste0("sermons/", 
                                      text[i, doc], ".txt"))
}

