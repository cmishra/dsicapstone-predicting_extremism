# -*- coding: utf-8 -*-
"""
Document to dataframe analysis
Created on Thu Sep 17 13:12:55 2015

@author: nmvenuti
"""

from nltk import tokenize
import re
import sys
from numpy import *
import pandas as pd


import nltk
nltk.download('all')

#set value list

value_terms = ["beautiful", "path of the prophet", "love", "eros", "ignorance", "evil", "justice", "disbelief", "apostriphy", "ungratefulness", "jewish", "fidelity", "loyalty", "treatery"]

#Load tokenizer
tokenizer = nltk.data.load('tokenizers/punkt/english.pickle')

#Load sample dataset
fp = open("C:/Users/nmvenuti/Desktop/capstone_data/sample_text.txt")
data = fp.read()



#break dataset into sentences
data_list = tokenizer.tokenize(data)

value_count={}
i=0
for sentence in data_list:
    i+=1
    value_count[i]=0
    words=sentence.strip()
    # Create regular expression to keep alphanumeric characters as well as @ and #. 
    punct = re.compile(r'([^A-Za-z0-9#@ ])')
    # Clean the tweet with this regular expression
    words = punct.sub("", words)
    # Deal with case-sensitivity by converting to lower-case
    words = words.lower()
    word = words.split()
    
    #check if word is value term        
    value_count[i]= len([w for w in word if w in value_terms])
    
print(value_count)

#Convert to dataframe
value_output=pd.DataFrame.from_dict(value_count, orient='index')
print(value_output)

print(value_output[value_output['Count'] > 0])
