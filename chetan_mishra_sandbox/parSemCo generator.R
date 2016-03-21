num_groups <- 1:328

lapply(num_groups, function(i) {
  file_text <- readLines("prototype/script_parSemCo.R")
  file_text[4] <- paste0('i = ', i)
  writeLines(file_text, paste0("prototype/par_sem_co/script_parSemCo", i, '.R'))
})

lapply(num_groups, function(i) {
  file_text <- readLines("chetan_mishra_sandbox/run_parSemCo.sh")
  file_text[19] <- str_replace(file_text[19], 'script_parSemCo.R', paste0('par_sem_co/script_parSemCo', i, '.R'))
  writeLines(file_text, paste0("prototype/par_sem_co/run_parSemCo", i, '.sh'))
})