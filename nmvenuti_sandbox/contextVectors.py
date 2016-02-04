# -*- coding: utf-8 -*-
"""
Created on Wed Feb  3 14:08:42 2016

@author: nmvenuti
"""

import numpy as np
import re, math, random
from nltk.corpus import stopwords
from collections import Counter
WORD = re.compile(r'\w+')
stopWords = stopwords.words("english")
##########################
#####Define Functions#####
##########################


#Define context vector function
def get_context(i,word_list,window):
    index_start=i-window
    index_end=i+window+1
    if index_start<0:
        index_start=0
    if index_end>len(word_list):
        index_end=len(word_list)
    context_vector=(word_list[index_start:index_end])
    context_vector.remove(word_list[i])
    context_vector=' '.join(context_vector)
    context_vector=str(word_list[i])+'-'+context_vector
    return context_vector

#Define data rip
def get_data(file,window=15,specific_context=None):
    #Normalize data
    try:
        text = open(file).read()
    except:
        text = open(file, encoding="latin1").read()
    text=text.lower()
    text_list=text.split()
    text_list=[''.join([w for w in text if not w.isdigit()]) for text in text_list]#remove digits
    text_list=[''.join([w for w in text if w.isalpha() ]) for text in text_list] #remove puntuation
    text_list=[word for word in text_list if word not in stopWords]
    
    #Get context vectors
    if specific_context==None:
        context_vectors=[get_context(x,text_list,window) for x in range(len(text_list))]
    else:
        context_vectors=[get_context(x,text_list,window) for x in range(len(text_list)) if text_list[x]==specific_context]
    unique_words = set(text_list)
    
    return unique_words,context_vectors

#Cosine similarity functions

def text_to_vector(text):
     words = WORD.findall(text)
     return Counter(words)

def get_cosine(str1,str2):
    vec1 = text_to_vector(str1)
    vec2 = text_to_vector(str2)
    intersection = set(vec1.keys()) & set(vec2.keys())
    numerator = sum([vec1[x] * vec2[x] for x in intersection])
    
    sum1 = sum([vec1[x]**2 for x in vec1.keys()])
    sum2 = sum([vec2[x]**2 for x in vec2.keys()])
    denominator = math.sqrt(sum1) * math.sqrt(sum2)
    return float(numerator/denominator)

def search_context_vectors(search,cv_list):
    x=[cv.replace(search+'-','') for cv in cv_list if search+'-' in cv.split()]
    return x

def average_consine_sim(search,context_vectors,sim=10000):
    sub_cv=search_context_vectors(search,context_vectors)
    
    if len(sub_cv)>0:
        consine_sim=np.zeros(sim)
        for i in range(sim):
            x=random.randrange(0, len(sub_cv))
            y=random.randrange(0, len(sub_cv))
            
            consine_sim[i]=get_cosine(sub_cv[x],sub_cv[y])
        approx_avg_cosine=np.average(consine_sim)
    else:
        approx_avg_cosine=-1
    return approx_avg_cosine
