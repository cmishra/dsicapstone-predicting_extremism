#include <Rcpp.h>
#include <string>
#include <map>
#include <vector>
#include <regex>

class cooccurrences_counter
{
public:
  std::map<std::string,int> map;
  std::string map_string = " ";
  
} counter;

void load_string_k(std::vector< std::string > content, int k);
void load_string_sentence(std::vector< std::string > content);
void log_context(std::string & target, std::vector<int> & context_indexes, std::vector<std::string> & content);
bool is_end_of_sentence(std::string & str);
void add_or_increment(std::string & key);
std::string to_str(std::vector<int> vec);

// [[Rcpp::export]]
std::vector<std::string> map_info() {
  std::vector<std::string> info; 
  // return counter.map.size();
  for (auto& p : counter.map) {
    info.push_back(p.first + " - " + std::to_string(p.second));
  }
  return info;
}


// [[Rcpp::export]]
void set_sepstring(std::vector<std::string> str) {
  counter.map_string = str[0];
}

void check_sepstring() {
  if (counter.map_string == " ")
    Rcpp::stop("key joining string not set!");
}

// [[Rcpp::export]]
void load_string_k(std::vector< std::string > content, int k)
{
  check_sepstring();
  int num_words_added, j; // j is the context offset
  for (int i = 0; i < content.size(); i++)
  {
    if (is_end_of_sentence(content[i]))
      continue;
    std::vector<int> context_indexes;
    j = -1;
    int tot_words_in_context;
    if (i < k)
      tot_words_in_context = i + k;
    else 
      tot_words_in_context = 2*k;
    while (context_indexes.size() < k && i+j >= 0)
    {
      if (!is_end_of_sentence(content[i+j]))
      {
        context_indexes.push_back(i+j);
      }
      j--;
    }
    j = 1;
    while(context_indexes.size() < tot_words_in_context && i+j < content.size())
    {
      if (!is_end_of_sentence(content[i+j]))
      {
        context_indexes.push_back(i+j);
      }
      j++;
    }
    log_context(content[i], context_indexes, content);
  }
}

// [[Rcpp::export]]
void load_string_sentence(std::vector< std::string > content)
{
  check_sepstring();
  int first_word = 0, end_of_sentence = 0;
  while(first_word < content.size())
  {
    while (end_of_sentence < content.size() &&
           !is_end_of_sentence(content[end_of_sentence])) 
    {
      end_of_sentence++;
    }
    
    std::vector<int> context_indexes;
    for (int i = first_word+1; i < end_of_sentence; i++)
    {
      context_indexes.push_back(i);
    }
    int target_index = first_word, j = 0;
    while (target_index < end_of_sentence)
    {
      log_context(content[target_index], context_indexes, content);
      if (j < context_indexes.size() && context_indexes[j] != target_index +1)
      {
        Rcpp::stop("first context word isn't as expected " + 
          std::to_string(context_indexes[j]) + " isn't " + 
          std::to_string(target_index+1) + " with params" + std::to_string(j) +
          "," + std::to_string(end_of_sentence));
      }
      if (j < context_indexes.size()) {
        context_indexes[j] = target_index;
      }
      target_index++;
      j++;
    }
    end_of_sentence += 1;
    first_word = end_of_sentence;
  }
}

std::string to_str(std::vector<int> vec) {
  std::string str = "";
  for (auto element : vec) {
    str += std::to_string(element) + ",";
  }
  return str;
}

// [[Rcpp::export]]
Rcpp::CharacterVector test_str_param(Rcpp::CharacterVector context)
{
  return Rcpp::CharacterVector::create(context.begin()[0]);
}
// EOS = end of sentence
bool is_end_of_sentence(std::string & str)
{
  return std::regex_match(str, std::regex("((\\.|!)|?)+"));
}


void log_context(std::string & target, std::vector<int> & context_indexes, std::vector<std::string> & content)
{
  for (int i = 0; i < context_indexes.size(); i++)
  {
    std::string key = target + counter.map_string + content[context_indexes[i]];
    add_or_increment(key);
  }
}

void add_or_increment(std::string &  key)
{
  std::pair<std::map<std::string,int>::iterator,bool> ret =
    counter.map.emplace(key, 1);
  if (!ret.second)
  {
    counter.map[key]++;
  }
}

// Abandoned bc regex was posing very perplexing problems.
// std::vector<std::string> str_extract_cpp(std::string str, std::string r_str) {
//   std::smatch str_match;
//   std::regex regex_str(r_str);
//   bool match = regex_match(str, str_match, regex_str);
//   std::vector<std::string> vec;
//   for (std::string str : str_match) {
//     vec.push_back(str);
//   }
//   return vec;
// }

// Abandoned bc regex was posing very perplexing problems.
// Rcpp::DataFrame pull_table()
// {
//   std::vector<std::string> target(counter.map.size());
//   std::vector<std::string> context(counter.map.size());
//   std::vector<int> freq(counter.map.size());
//   
//   std::smatch str_match;
//   std::map<std::string, int>::iterator itr = counter.map.begin();
//   std::regex comp_key("(.+)" + counter.map_string + "(.+)");
//   for (itr = counter.map.begin(); itr != counter.map.end(); ++itr)
//   {
//     regex_match(itr -> first, str_match, comp_key);
//     target.push_back(str_match[0]);
//     context.push_back(str_match[1]);
//     freq.push_back(itr -> second);
//   }
//   return Rcpp::DataFrame::create(Rcpp::Named("target")=target,
//                                  Rcpp::Named("context")=context,
//                                  Rcpp::Named("freq")=freq);
// }
// 
