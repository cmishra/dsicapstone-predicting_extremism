context("cooccurrences")

test_that("sepstring check works", {
  doc <- c("the", "cat", "went", "to", "the", ".")
  reload_cooccurrences()
  expect_error(load_string_k(doc, 2), 
               "key joining string not set!")
})

test_that("sepstring is settable", {
  doc <- c("the", "cat", "went", "to", "the", ".")
  reload_cooccurrences()
  set_sepstring(occurrences_sepstr)
  expect_true(is.null(load_string_k(doc, 2)))
  reload_cooccurrences()
})

test_that("positional cooccurrences are counted correctly", {
  test_scenario <- function(doc, k) {
    reload_cooccurrences()
    set_sepstring(occurrences_sepstr)
    load_string_k(doc, k)
    map_info()
  }
  doc <- c("the", "cat", "went", "to", "the", ".")
  output1 <- sort(c(
    mock_map_info("the", "cat", 1),
    mock_map_info("cat", "the", 1),
    mock_map_info("cat", "went", 1),
    mock_map_info("went", "to", 1),
    mock_map_info("went", "cat", 1),
    mock_map_info("to", "went", 1),
    mock_map_info("to", "the", 1),
    mock_map_info("the", "to", 1)
  ))
  expect_equal(test_scenario(doc, 1), output1)
  
  output2 <- sort(c(
    mock_map_info("the", "cat", 1),
    mock_map_info("cat", "the", 1),
    mock_map_info("cat", "went", 1),
    mock_map_info("went", "to", 1),
    mock_map_info("went", "cat", 1),
    mock_map_info("to", "went", 1),
    mock_map_info("to", "the", 1),
    mock_map_info("the", "to", 1),
    mock_map_info("the", "went", 2),
    mock_map_info("went", "the", 2),
    mock_map_info("cat", "to", 1),
    mock_map_info("to", "cat", 1)
  ))
  expect_equal(test_scenario(doc, 2), output2)
  reload_cooccurrences()
})

test_that("relational cooccurrences are counted correctly", {
  test_scenario <- function(doc) {
    reload_cooccurrences()
    set_sepstring(occurrences_sepstr)
    load_string_sentence(doc)
    map_info()
  }
  
  doc1 <- c("i", "like", "you", ".")
  output1 <- sort(c(
    mock_map_info("i", "like", 1),
    mock_map_info("like", "i", 1),
    mock_map_info("like", "you", 1),
    mock_map_info("i", "you", 1),
    mock_map_info("you", "i", 1),
    mock_map_info("you", "like", 1)
))
  expect_equal(test_scenario(doc1), output1)
  
  doc2 <- c("i", "like", "you", ".",
           "i", "punch", "you", "dummy", ".")
  output2 <- sort(c(
    mock_map_info("i", "like", 1),
    mock_map_info("like", "i", 1),
    mock_map_info("like", "you", 1),
    mock_map_info("you", "like", 1),
    mock_map_info("i", "punch", 1),
    mock_map_info("punch", "i", 1),
    mock_map_info("punch", "you", 1),
    mock_map_info("you", "dummy", 1),
    mock_map_info("i", "you", 2),
    mock_map_info("punch", "dummy", 1),
    mock_map_info("you", "i", 2),
    mock_map_info("dummy", "punch", 1),
    mock_map_info("dummy", "i", 1),
    mock_map_info("dummy", "you", 1),
    mock_map_info("i", "dummy", 1),
    mock_map_info("you", "punch", 1)
  ))
  expect_equal(test_scenario(doc2), output2)

  doc3 <- c(".", "i", "like", "you", ".")
  output3 <- sort(c(
    mock_map_info("i", "like", 1),
    mock_map_info("like", "i", 1),
    mock_map_info("like", "you", 1),
    mock_map_info("i", "you", 1),
    mock_map_info("you", "i", 1),
    mock_map_info("you", "like", 1)
  ))
  expect_equal(test_scenario(doc3), output3)
  
  doc4 <- c(".", "!", ".", "i", "like", "you", ".", ".", ".", ".")
  output4 <- sort(c(
    mock_map_info("i", "like", 1),
    mock_map_info("like", "i", 1),
    mock_map_info("like", "you", 1),
    mock_map_info("i", "you", 1),
    mock_map_info("you", "i", 1),
    mock_map_info("you", "like", 1)
  ))
  expect_equal(test_scenario(doc4), output4)
  
  reload_cooccurrences()
})

test_that("str_extract_cpp works", {
  reload_cooccurrences()
  na_to_empty <- function(x) if(is.na(x)) "" else x
  str1 = "abc123abc";
  regex1 = "(abc)"
  expect_equal(na_to_empty(str_extract(str1, regex1)),
               str_extract_cpp(str1, regex1))
  regex2 = "\\d"
  expect_equal(na_to_empty(str_extract(str1, regex2)),
               str_extract_cpp(str1, regex2))
  regex3 = "#"
  expect_equal(na_to_empty(str_extract(str1, regex3)),
               str_extract_cpp(str1, regex2))
  
  
})
# 
# test_that("pull_table works", {
#   test_scenario <- function(doc) {
#     reload_cooccurrences()
#     set_sepstring(occurrences_sepstr)
#     load_string_sentence(doc)
#     pull_table()
#   }
#   output_to_df <- function(output) {
#     target <- str_extract(output, paste0(".+", sepstr_regex))
#     target <- str_sub(target, end=str_length(target)-str_length(occurrences_sepstr))
#     context <- str_extract(output, paste0(sepstr_regex, ".+", " - "))
#     context <- str_sub(context, start=str_length(occurrences_sepstr)+1, 
#                        end=str_length(context) - 3)
#     freq <- str_sub(str_extract(output, " - \\d+"), start=4)
#     data.frame(target, context, freq)
#   }
#   doc1 <- c("i", "like", "you", ".")
#   output1 <- sort(c(
#     mock_map_info("i", "like", 1),
#     mock_map_info("like", "i", 1),
#     mock_map_info("like", "you", 1),
#     mock_map_info("i", "you", 1),
#     mock_map_info("you", "i", 1),
#     mock_map_info("you", "like", 1)
#   ))
#   output_to_df(output1)
#   test_scenario(doc1)
#   
# })