# -*- coding: utf-8 -*-
"""
Created on Wed Feb  3 14:08:42 2016

@author: nmvenuti
"""

import numpy as np
import pandas as pd
import numba as nb
import multiprocessing as mp
import os, glob, nltk, re, string
from nltk.corpus import stopwords
from itertools import combinations
##########################
#####Define Functions#####
##########################

#Define merge sort
def mergeSort(alist):
    if len(alist)>1:
        mid = len(alist)//2
        lefthalf = alist[:mid]
        righthalf = alist[mid:]
        mergeSort(lefthalf)
        mergeSort(righthalf)

        i=0
        j=0
        k=0
        while i < len(lefthalf) and j < len(righthalf):
            if lefthalf[i] < righthalf[j]:
                alist[k]=lefthalf[i]
                i=i+1
            else:
                alist[k]=righthalf[j]
                j=j+1
            k=k+1

        while i < len(lefthalf):
            alist[k]=lefthalf[i]
            i=i+1
            k=k+1

        while j < len(righthalf):
            alist[k]=righthalf[j]
            j=j+1
            k=k+1

#Define binary search
def binarySearch(alist, item):
    if len(alist) == 0:
        return False
    else:
        midpoint = len(alist)//2
        if alist[midpoint]==item:
            return True
        else:
            if item<alist[midpoint]:
                return binarySearch(alist[:midpoint],item)
            else:
                return binarySearch(alist[midpoint+1:],item)

#Define context vector function
def get_context(i,word_list,window):
    index_start=i-window
    index_end=i+window
    if index_start<0:
        index_start=0
    if index_end>len(word_list):
        index_end=len(word_list)
    context_vector=(word_list[index_start:index_end])
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

#Create dictionary of Co-occurances
def dict_co_occurance(x):
    co_occurance=list(combinations(x,2))
    co_occurance_dict = {i:1 for i in co_occurance}
    return co_occurance_dict

#Create context matrix function
def get_context_matrix(unique_words,context_vectors):
    context_matrix=np.zeros([len(unique_words),len(unique_words),len(context_vectors)])
    
    #Add 1 for co-occurance of word in context vectors
    for x in range(len(unique_words)):
        for y in range(len(unique_words)):
            for z in range(len(context_vectors)):
                if unique_words[x] in context_vectors[z] and unique_words[y] in context_vectors[z]:
                    context_matrix[x,y,z]=1

#Set up numb context matrix function
get_context_matrix_nb=nb.jit(get_context_matrix)


#Get file list
path = 'C:/Users/nmvenuti/Desktop/webscraping sermoncentral/john_piper/'
file_list=[infile for infile in glob.glob( os.path.join(path, '*.*') )]



###################################
######Set up inital parameters#####
###################################
context_vectors=[]
unique_words=set([])
stopWords = stopwords.words("english")
processors=mp.cpu_count()
pool=mp.Pool(processes=processors)

##########################################################
#####Get all unique values, context vectors for files#####
##########################################################

#For each file extract context vectors
for file in file_list:
    x,y=get_data(file)
    context_vectors=context_vectors+y
    unique_words=x|unique_words

#Turn unique words into list and sort
mergeSort(list(unique_words))

len(unique_words)
len(context_vectors)

#psuedo sparse matrix for context vectors
cv_length_range=range(len(context_vectors))

#Test on ten to ensure accuracy
#cv_sparse=[(i,dict_co_occurance(context_vectors(i))) for i in cv_length_range]
cv_sparse=[(i,dict_co_occurance(context_vectors[i])) for i in range(10)]

df=pd.DataFrame(cv_sparse)

df.to_csv("C:/Users/nmvenuti/Desktop/test_context_vectors.csv")