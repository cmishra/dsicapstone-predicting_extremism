library(stringr)
library(data.table)

# designed for static websites
get_webpage_html <- function(url) {
  connection <- url(sermon_links)
  ret <- paste(readLines(connection), collapse="")
  close(connection)
  ret
}

# executable path is the path to the executable
# "pdftotext.exe"
# look here for info: https://en.wikipedia.org/wiki/Pdftotext
# written for windows
pdf_to_text <- function(pdf_path, executable_path) {
  system2(command="./pdftotext.exe",
          args=pdf_path)
  print(paste(paste0("./", executable_path), pdf_path, sep = " "))
  text_path <- str_replace(pdf_path, "[.]pdf", ".txt")
  readLines(text_path)
}