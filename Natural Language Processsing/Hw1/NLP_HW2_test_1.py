import numpy as np
import pandas as pd
import string
from nltk import sent_tokenize, word_tokenize

data1 = '/Users/marctheshark/Documents/NLP/HW2/data/amazon_cells_labelled.txt'
data2 = '/Users/marctheshark/Documents/NLP/HW2/data/imdb_labelled.txt'
data3 = '/Users/marctheshark/Documents/NLP/HW2/data/yelp_labelled.txt'


def getfile(address):
    file = open(address, 'rt')

    data = file.readlines()
    file.close()

    return data

def preprocessing(your_data):
    data = getfile(your_data)
    sentiment = []
    corpus = []
    for i in range(len(data)):
        index = data[i]
        # print(index, 'i')

        for j in reversed(range(len(index))):
            single_index = index[j]
            try:
                if int(single_index) <= 1:
                    sentiment.append(int(single_index))
                    break
            except ValueError:
                w = 0
##Building sentences from the data and removing punctuation and lowering captilizations.
        words = word_tokenize(index)
        #print(words)

        words = [word.lower() for word in words]
        # removing any punctuation
        matrix = str.maketrans('', '', string.punctuation)
        removed = [word.translate(matrix) for word in words]
        filter_words = [word for word in removed if word.isalpha()]

        corpus.append(filter_words)
#building the unique word bag

        token=[]

        for words in corpus:
            token.extend(words)
        unique_token = list(set(token))

    return sentiment, corpus, unique_token

def n_gram(data, n):
    nothing = preprocessing(data)
    labels, corpus, tokens = nothing
    corpus_ngram =[]
    for i in range(len(corpus)):
        sentences = corpus[i]
        sentence_ngram =[]
        for word in range(len(sentences)):

            sentence_ngram.append(sentences[word:word+n])
        corpus_ngram.append(sentence_ngram)
    return corpus_ngram , labels


#print(n_gram(data1,3)[0])

def build_dataframe(data):

    ng_all= []
    for i in range(1, 6):
        ng_all.append(n_gram(data,i)[0])
    labels = n_gram(data,1)[1]
    return ng_all , labels

#references Links :https://realpython.com/python-range/ reversed
#http://www.albertauyeung.com/post/generating-ngrams-python/
#https://stackoverflow.com/questions/13423919/computing-n-grams-using-python