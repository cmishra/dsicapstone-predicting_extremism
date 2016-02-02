# download pdftotxt from 
# ftp://ftp.foolabs.com/pub/xpdf/xpdfbin-win-3.03.zip
# and extract to your program files folder

# here is a pdf for mining
dest <- "C:\\Users\\brian\\Documents\\UVA\\Capstone\\Unitarian\\2015-07-05 Privilege Unpacking the Knapsack, Morgan Patten.pdf"

# set path to pdftotxt.exe and convert pdf to text
exe <- "C:\\Program Files\\xpdfbin-win-3.04\\bin32\\pdftotext.exe"
system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)

# get txt-file name and open it  
filetxt <- sub(".pdf", ".txt", dest)
shell.exec(filetxt)


# do something with it, i.e. a simple word cloud 
library(tm)

txt <- readLines(filetxt) # don't mind warning..

