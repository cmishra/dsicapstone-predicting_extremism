setwd("Dropbox/College/4_Fourth_Year/Capstone/")
Sys.setenv(LD_LIBRARY_PATH=paste0(Sys.getenv()['LD_LIBRARY_PATH'],
                                  ':/home/chetan/library/gsl-2.1/.libs:/home/chetan/library/gsl-2.1/cblas/.libs'))
install.packages('topicmodels')
