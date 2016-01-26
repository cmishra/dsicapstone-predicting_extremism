#include <Rcpp.h>
#include <string>
#include <map>
#include <vector>


class cooccurrences_counter {
public:
  std::map<std::string,int>* map;
  std::string map_string = "_%_!_";
  
} counter;


void counter_init(); 
void load_string_k(std::string content[], int k);
void load_string_sentence(std::string content[]);
void load_string_sentence(std::string content[]);
void add_or_increment(std::string key);
void counter_reset();
