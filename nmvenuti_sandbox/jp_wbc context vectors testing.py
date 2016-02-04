# -*- coding: utf-8 -*-
"""
Created on Thu Feb  4 15:44:30 2016

@author: nmvenuti
context Vectors Testing for wbc/jp
"""
import os, glob, time
import sys
sys.path.append('C:/Users/nmvenuti/Desktop/Context Vector Testing')
import contextVectors as cv


#Get file list
jp_path = 'C:/Users/nmvenuti/Desktop/webscraping sermoncentral/john_piper/'
jp_file_list=[infile for infile in glob.glob( os.path.join(jp_path, '*.*') )]

wb_path = 'C:/Users/nmvenuti/Desktop/webscraping westboro/sermons/'
wb_file_list=[infile for infile in glob.glob( os.path.join(wb_path, '*.*') )]

###################################
######Set up inital parameters#####
###################################
jp_context_vectors=[]
wb_context_vectors=[]

##########################################################
#####Get all unique values, context vectors for files#####
##########################################################
#For each file extract context vectors
for file in jp_file_list:
    x,y=cv.get_data(file)
    jp_context_vectors=jp_context_vectors+y

for file in wb_file_list:
    x,y=cv.get_data(file)
    wb_context_vectors=wb_context_vectors+y

#Test search context vectors
test=cv.search_context_vectors('god',jp_context_vectors)

len(test)
#962
test[0]
#' made even clearly jesus paul new testament jesus marriage godjesus makes point clearly marriage mark  
#beginning creation god made male female genesis  therefore man shall leave father'

test=cv.search_context_vectors('god',wb_context_vectors)

len(test)
#577
test[0]
#' cities  pickets documented warning america sodomite doom america  
#land sodomite damned beloved counted us faithful putting us ministry next sunday begin th year blessed service  love'


#Test sver cosine approx on both corpuses
cv.average_consine_sim('god',jp_context_vectors,sim=1000)
#0.074665812350007799

cv.average_consine_sim('god',wb_context_vectors,sim=1000)
#0.046074115924377115

#Create quick and dirty dataframe of value words and average cosine distance for each corpus
value_words=['love','hate','god','sin',"beautiful","ignorance", "evil", 
"justice", "disbelief", "ungratefulness", "jewish", "fidelity", "loyalty", "treachery"]

import pandas as pd

valueWords_df=pd.DataFrame(value_words)
valueWords_df.columns=['valueWords']

jp_avg_cosine=[cv.average_consine_sim(i,jp_context_vectors,sim=1000) for i in value_words]

valueWords_df['JP']= jp_avg_cosine

wb_avg_cosine=[cv.average_consine_sim(i,wb_context_vectors,sim=1000) for i in value_words]

valueWords_df['WB']= wb_avg_cosine

print(valueWords_df)


# -1 indicates no context vectors for these words were found
#        valueWords        JP        WB
#0             love  0.111886  0.067383
#1             hate  0.221584  0.106563
#2              god  0.079286  0.046347
#3              sin  0.076030  0.065646
#4        beautiful  0.363554  0.202709
#5        ignorance  0.536954  0.226163
#6             evil  0.087253  0.053713
#7          justice  0.184184  0.164967
#8        disbelief -1.000000 -1.000000
#9   ungratefulness -1.000000 -1.000000
#10          jewish  0.125975  0.122661
#11        fidelity -1.000000  1.000000
#12         loyalty  1.000000 -1.000000
#13       treachery -1.000000 -1.000000

#JP seems to have greater cosine similarity to one another (vectors are closer to one another)
#Ran based on windows of 15

#Perform same analysis with window of 3
jp_context_vectors=[]
wb_context_vectors=[]

#For each file extract context vectors
for file in jp_file_list:
    x,y=cv.get_data(file, window=3)
    jp_context_vectors=jp_context_vectors+y

for file in wb_file_list:
    x,y=cv.get_data(file, window=3)
    wb_context_vectors=wb_context_vectors+y
    
valueWords_df=pd.DataFrame(value_words)
valueWords_df.columns=['valueWords']

jp_avg_cosine=[cv.average_consine_sim(i,jp_context_vectors,sim=1000) for i in value_words]

valueWords_df['JP']= jp_avg_cosine

wb_avg_cosine=[cv.average_consine_sim(i,wb_context_vectors,sim=1000) for i in value_words]

valueWords_df['WB']= wb_avg_cosine

print(valueWords_df)

#        valueWords        JP        WB
#0             love  0.039931  0.079517
#1             hate  0.153000  0.128872
#2              god  0.014084  0.011944
#3              sin  0.033390  0.037303
#4        beautiful  0.169200  0.077757
#5        ignorance  0.532000  0.192957
#6             evil  0.079804  0.044437
#7          justice  0.119626  0.173200
#8        disbelief -1.000000 -1.000000
#9   ungratefulness -1.000000 -1.000000
#10          jewish  0.135637  0.150000
#11        fidelity -1.000000  1.000000
#12         loyalty -1.000000 -1.000000
#13       treachery -1.000000 -1.000000

#Becomes more rigid with tighter window
#May also be a good idea to try with parts of speech tagging/stemming