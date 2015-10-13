# -*- coding: utf-8 -*-
"""
Created on Tue Oct  6 19:21:08 2015

@author: nmvenuti
"""

from nltk import tokenize
import re
import sys
from numpy import *
import pandas as pd


import nltk
nltk.download('all')

from nltk.corpus import stopwords

from nltk.stem import *

cachedStopWords = stopwords.words("english")

stemmer = SnowballStemmer("english", ignore_stopwords=True)



import os, fnmatch

import re
import collections

#find files function
def findFiles (path, filter):
    for root, dirs, files in os.walk(path):
        for file in fnmatch.filter(files, filter):
            yield os.path.join(root, file)


            
#create file for lda
df=[]
for textFile in findFiles(r'C:\Users\nmvenuti\Desktop\UVA MSDS\Fall\DS 6001\Data Sources\westboro_sermons', '*.txt'):
    words = re.findall('\w+', open(textFile).read().lower())
    words = [stemmer.stem(word) for word in words]
    words = [word for word in words if word not in cachedStopWords]
    words = [word for word in words if word[0].isdigit() == False]
#    words = [word for word in words if not isinstance(word, int)]
    data={}
    X = dict(collections.Counter(words).items())
    X['file_path']=textFile
    df.append(X)
dfX = pd.DataFrame(df)
dfX = dfX.set_index(['file_path'])

len(dfX.columns)

dfX.to_csv("C:/Users/nmvenuti/Desktop/UVA MSDS/Fall/DS 6001/output_3.csv")

#still needs some cleaning to get right format, works for now