# Create Randomize Test/Train Index for Each Group

runResample <- function(){
  # Get List of Groups in Folder Structure
  files <- list.files('./data_dsicap')
  groups  <- files[files != 'ref']
  print(groups)
  
  for (group in groups) {
    testTrainSplit(paste0('./data_dsicap/',group), 10)
  }
}