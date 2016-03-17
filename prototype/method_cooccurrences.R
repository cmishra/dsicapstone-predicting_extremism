
fasterCooccurrences <- cmpfun(function(content, k=F){
  if (length(content) == 1)
    return(data.table())
  if (!is.numeric(k))
    load_string_sentence(content)
  else 
    load_string_k(content, k)
}) 


output_to_df <- function(output) {
  target <- str_extract(output, paste0(".+", sepstr_regex))
  target <- str_sub(target, end=str_length(target)-str_length(occurrences_sepstr))
  context <- str_extract(output, paste0(sepstr_regex, ".+", " - "))
  context <- str_sub(context, start=str_length(occurrences_sepstr)+1, 
                     end=str_length(context) - 3)
  freq <- str_sub(str_extract(output, " - \\d+"), start=4)
  data.frame(target, context, freq)
}

output_cooccurrences <- function(processed_tokens, folderpath, group_id, window_length) {
  set_sepstring(occurrences_sepstr)
  invisible(lapply(processed_tokens, function(element) {
    fasterCooccurrences(element$content, 4)
  }))
  wordCo <- data.table(output_to_df(map_info()))
  wordCo <- wordCo[,.(target, context, freq=as.integer(as.character(freq)))]
  reload_cooccurrences()
  save(wordCo, file=wordCo_filename(folderpath, group_id))
}







