import numpy as np
import lda
import lda.datasets

import pandas as pd
from pandas import Series
import numpy as np
from numpy import genfromtxt

# the vocab
#vocab = lda.datasets.load_reuters_vocab()
x = pd.read_csv("C:/Users/nmvenuti/Desktop/UVA MSDS/Fall/DS 6001/output_3.csv", encoding='mac_roman')
vocab = list(x.columns.values)
col = list(x.columns.values)
vocab = tuple(vocab)
print(col[1:len(col)])

print("type(vocab): {}".format(type(vocab)))
print("len(vocab): {}\n".format(len(vocab)))

# titles for each story
#titles = lda.datasets.load_reuters_titles()
titles = x['file_path']
titles = tuple(titles)
print("type(titles): {}".format(type(titles)))
print("len(titles): {}\n".format(len(titles)))

print(col)
# document-term matrix
y = x.as_matrix(columns=col[1:len(col)])
print(y[0:5,])
X = np.nan_to_num(y)
print(X[0:5,])
X = X.astype(int)

#dfX = pd.DataFrame(X)
#dfX.to_csv("C:/Users/nmvenuti/Desktop/UVA MSDS/Fall/DS 6001/X_gonna_give_it_to_you.csv")
#print(X[len(X)-5:len(X),])

print(X[0:5,])
print(X.dtype.names)

#X = x.to_records()
#y = lda.datasets.load_reuters()



print(y.dtype.names)
print("type(X): {}".format(type(X)))
print("shape: {}\n".format(X.shape))


#fit the model
model = lda.LDA(n_topics=20, n_iter=500, random_state=1)
model.fit(X)

#output topic-word probabilities
topic_word = model.topic_word_
print("type(topic_word): {}".format(type(topic_word)))
print("shape: {}".format(topic_word.shape))

#get top 5 words for each topic
n = 5
for i, topic_dist in enumerate(topic_word):
    topic_words = np.array(vocab)[np.argsort(topic_dist)][:-(n+1):-1]
    print('*Topic {}\n- {}'.format(i, ' '.join(topic_words)))
    
    
#output document-topic probabilities
doc_topic = model.doc_topic_
print("type(doc_topic): {}".format(type(doc_topic)))
print("shape: {}".format(doc_topic.shape))

#get most probable topic from title of news story
for n in range(50):
    topic_most_pr = doc_topic[n].argmax()
    print("doc: {} topic: {}\n{}...".format(n,
                                            topic_most_pr,
                                            titles[n][:50]))