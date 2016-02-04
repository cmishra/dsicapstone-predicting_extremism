# -*- coding: utf-8 -*-
"""
Created on Wed Feb  3 14:08:42 2016

@author: nmvenuti
"""

import numpy as np
import pandas as pd
import numba as nb
from multiprocessing import Pool
import os, glob, nltk, re, string, time, math, random
from nltk.corpus import stopwords
from itertools import combinations
from collections import Counter
WORD = re.compile(r'\w+')
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
    context_vector=' '.join(context_vector)
    return context_vector

#Define data rip
def get_data(file):
    #Normalize data
    text = open(file).read()
    text=text.lower()
    text_list=text.split()
    text_list=[''.join([w for w in text if not w.isdigit()]) for text in text_list]#remove digits
    text_list=[''.join([w for w in text if w.isalpha() ]) for text in text_list] #remove puntuation
    text_list=[word for word in text_list if word not in stopWords]
    
    #Get context vectors
    context_vectors=[get_context(x,text_list,15) for x in range(len(text_list))]
    
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
    x=[cv for cv in cv_list if search in cv.split()]
    return x

def average_consine_sim(search,context_vectors,sim=10000):
    sub_cv=search_context_vectors(search,context_vectors)
    
    consine_sim=np.zeros(sim)
    for i in range(sim):
        x=random.randrange(0, len(sub_cv))
        y=random.randrange(0, len(sub_cv))
        
        consine_sim[i]=get_cosine(sub_cv[x],sub_cv[y])
    
    approx_avg_cosine=np.average(consine_sim)
    return approx_avg_cosine

#Get file list
path = 'C:/Users/nmvenuti/Desktop/webscraping sermoncentral/john_piper/'
file_list=[infile for infile in glob.glob( os.path.join(path, '*.*') )]



###################################
######Set up inital parameters#####
###################################
context_vectors=[]
unique_words=set([])
stopWords = stopwords.words("english")

##########################################################
#####Get all unique values, context vectors for files#####
##########################################################
#For each file extract context vectors
for file in file_list:
    x,y=get_data(file)
    context_vectors=context_vectors+y
    unique_words=x|unique_words

#Test get cosine
get_cosine(context_vectors[0],context_vectors[1])

#Test search context vectors
test=search_context_vectors('god',context_vectors)

#Get estimation of cosine similarity through monte carlo simulations
#Test
sim=100000
consine_sim=np.zeros(sim)
for i in range(sim):
    x=random.randrange(0, len(test))
    y=random.randrange(0, len(test))
    
    consine_sim[i]=get_cosine(test[x],test[y])

approx_avg_cosine=np.average(consine_sim)

#1000 sims 0.10165190912950399
#10000 sims 0.10029508468153125
#100000 sims 0.10044157631740482

average_consine_sim('god',context_vectors,sim=100000)
#0.099783710262092554

###Essentially getting same values from 1000 to 100,000 simulations
###Will use this to pipe into parallelized system to run whole corpus