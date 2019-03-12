import numpy as np
import pandas as pd
import string
from nltk import sent_tokenize, word_tokenize
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.metrics import confusion_matrix
import imp

#importing Data
data1 = '/Users/marctheshark/Documents/NLP/HW2/data/amazon_cells_labelled.txt'
data2 = '/Users/marctheshark/Documents/NLP/HW2/data/imdb_labelled.txt'
data3 = '/Users/marctheshark/Documents/NLP/HW2/data/yelp_labelled.txt'


#gathering the data and closing the file
def getfile(address):
    file = open(address, 'rt')

    data = file.readlines()
    file.close()

    return data

print(getfile(data1))

#processing the data from the txt file to get the sentiment labels, corpus, and the unique ngram tokens
def preprocessing(your_data):
    data = getfile(your_data)
    sentiment = []
    corpus = []
    #looping over the length of the data
    for i in range(len(data)):
        index = data[i]
        # print(index, 'i')

        #reverse looping through each index to find the label 0 or 1
        for j in reversed(range(len(index))):
            single_index = index[j]


            try:

                #if the single_index is 0 or 1 lets store it
                if int(single_index) <= 1:
                    sentiment.append(int(single_index))
                    #stop this from looping through the rest of the index
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
        #findthing the unique ngram token for the given corpus
        for words in corpus:
            token.extend(words)
        unique_token = list(set(token))

    return sentiment, corpus, unique_token

#creating the ngram to compare with the ngram dictionary
def n_gram_GetTraining(data, n):

    nothing = preprocessing(data)
    labels, corpus, tokens = nothing
    corpus_ngram =[]

    #looking through each sentence in the corpus
    for i in range(len(corpus)):
        sentences = corpus[i]
        sentence_ngram =[]

        #looking at each word of the sentence
        for word in range(len(sentences)):
            #storing the ngram
            sentence_ngram.append(sentences[word:word+n])
        #storing all ngrams in their respective sentences
        corpus_ngram.append(sentence_ngram)


    #splitting into unique ngram tokens
    tokens = []
    for j in range(len(corpus_ngram)):
        index = corpus_ngram[j]
        for w in range(len(index)):
            next_index = index[w]

            if next_index not in tokens:
                tokens.append(next_index)
    tokens.sort()
    #print(tokens)
    #creating the training data for this selection of n
    #looping over each unqiue token and then over each ngram with respective of its index.
    encoded_data = np.zeros([len(corpus_ngram),len(tokens)])
    count = 0
    for z in range(len(tokens)):
        unique_token = tokens[z]
        for e in range(len(corpus_ngram)):
            if unique_token in corpus_ngram[e]:
                count += 1
                encoded_data[e,z] = count

    return encoded_data , labels


#             ---------------------------<<<<< Classification  >>>>>----------------------------
#splitting the data so they can be equally distributed to the testing and training
def split_data(encoded_data, labels):

    zero =[]
    one = []
    zero_data = []
    one_data = []
    #looping through the encoded date

    #looking through the lendth of the labels
    for i in range(len(labels)):
        #if the label is 0 store it in its respective element
        if labels[i] == 0:
            zero.append(labels[i])
            zero_data.append(encoded_data[i])
        #if the label is 1 store it in its respective element
        elif labels[i] == 1:
            one.append(labels[i])
            one_data.append(encoded_data[i])

    return zero , zero_data , one , one_data

#classifying the data with the labels
def classifcation(encoded_data, labels , k,s_or_c,n_gram):
    zero, zero_data, one, one_data = split_data(encoded_data, labels)
   #initializing the 7 stats needed for assessment
    accuracy = []
    true_p = []
    true_n =[]
    false_p =[]
    false_n = []
    length = int(len(zero))
    folds =  int(length / (k))
    num_folds = int(length / folds)
    #print(folds)

    #looping through the kfolds for classifying
    for i in range(k):
        zero, zero_data, one, one_data = split_data(encoded_data, labels)
        #creating the testing data for the labels and observations
        test_zero = zero_data[folds* i: folds*(i)+folds]
        test_zero_l = zero[folds* i: folds*(i)+folds]
        test_one = one_data[folds* i: folds*(i)+folds]
        test_one_l = one[folds* i: folds*(i)+folds]
        #checking size
        ''' 
        print(test_zero)
        print(len(test_zero), 'zd')
        print(len(test_one), 'od')
        print(len(test_zero_l), 'zd1')
        print(len(test_one_l), 'od1')
        print(len(zero_data[folds*(i-1): folds*(i-1)+folds]))
        
         '''
        #adding the data together
        testing_data = np.concatenate((test_zero,test_one))
        testing_labels = np.concatenate((test_zero_l,test_one_l))
        #deteting the data that the was stored for the test data
        del zero_data[folds* i: folds*(i)+folds]
        del one_data[folds* i: folds*(i)+folds]
        del zero[folds* i: folds*(i)+folds]
        del one[folds* i: folds*(i)+folds]
        #storing the rest of the data used into the new training variables
        training_data = np.concatenate((zero_data,one_data))
        training_labels = np.concatenate((zero, one))

        #testing size
        '''
        
        print(testing_data.shape, 'testing shape')
        print(testing_labels.shape, ' testing label shape')

        print(training_data.shape, 'traing data')
        print(training_labels.shape , 'train label')
         '''
        #developing the classifier
        classifier = LogisticRegression(solver='newton-cg',n_jobs=-1, max_iter=1000)
        #calling the fit to train the model
        classifier.fit(training_data,training_labels)
        #predicting based on the model
        prediction = classifier.predict(testing_data)
        #print(prediction)
        #print(testing_labels)

        #variables to use as counters
        everything = 0
        count = 0
        yes0 = 0
        yes1 = 0
        no0 = 0
        no1 = 0
        # true will be 0 positive will be 1 negative
        for i in range(len(prediction)):
            everything += 1
            if prediction[i] == testing_labels[i]:

                #predicted a 0 correctly true positive
                if prediction[i] == 0:
                    yes0 +=1
                    count += 1
                #predicted a 1 correct false positive
                else:
                    yes1 +=1
                    count += 1

            else:
                #predicted 0 instead of 1 true negative
                if prediction[i] == 0:

                    no1 +=1
                #predict 1 instead of a 0 false negative
                else:
                    no0 +=1
        #storing values for stats
        tp = yes0
        fp = yes1
        tn = no1
        fn = no0

        #need to average up the tptnfn and so on and all the other stats per 1 run of this function
        accuracy1 = (tp + tn) / (tp + tn + fp + fn)

        #storing the values to be averaged later
        true_p.append(tp)
        true_n.append(tn)
        false_p.append(fp)
        false_n.append(fn)
        accuracy.append(count/everything)

    #averaging values
    tp = (np.mean(true_p))
    tn = (np.mean(true_n))
    fp = (np.mean(false_p))
    fn = ((np.mean(false_n)))
    acc = np.mean(accuracy)

    #acc precision recall calculation
    precision = round((tp /(tp+fp)),5)
    accuracy = round(((tp+tn)/(tp+tn+fp +fn)),5)
    recall = round((tp / (tp + fn)),5)

    #formating for printing output
    if s_or_c == 's':
        if n_gram == 1:

            print("Results for uni-gram:" )
            print('TP', tp, 'TN', tn, 'FP', fp, 'FN', fn, 'accuracy', accuracy, 'recall', recall, 'precision',
                  precision)
        elif n_gram == 2:
            print("Results for bi-gram:")
            print('TP', tp, 'TN', tn, 'FP', fp, 'FN', fn, 'accuracy', accuracy, 'recall', recall, 'precision',
                  precision)
        elif n_gram == 3:
            print("Results for tri-gram:")
            print('TP', tp, 'TN', tn, 'FP', fp, 'FN', fn, 'accuracy', accuracy, 'recall', recall, 'precision',
                  precision)
        elif n_gram == 4:
            print("Results for quad-gram:")
            print('TP', tp, 'TN', tn, 'FP', fp, 'FN', fn, 'accuracy', accuracy, 'recall', recall, 'precision',
                  precision)
        elif n_gram == 5:
            print("Results for penta-gram:")
            print('TP', tp, 'TN', tn, 'FP', fp, 'FN', fn, 'accuracy', accuracy, 'recall', recall, 'precision',
                  precision)
    else:
        if n_gram ==1:
            print("Results for uni-gram & bi-gram::")
            print('TP', tp, 'TN', tn, 'FP', fp, 'FN', fn, 'accuracy', accuracy, 'recall', recall, 'precision',
                  precision)
        elif n_gram == 2:
            print("Results for uni-gram & bi-gram & tri-gram:")
            print('TP', tp, 'TN', tn, 'FP', fp, 'FN', fn, 'accuracy', accuracy, 'recall', recall, 'precision',
                  precision)
        elif n_gram == 3:
            print("Results for uni-gram & bi-gram & tri-gram & quad-gram:")
            print('TP', tp, 'TN', tn, 'FP', fp, 'FN', fn, 'accuracy', accuracy, 'recall', recall, 'precision',
                  precision)
        elif n_gram == 4:
            print("Results for uni-gram & bi-gram & tri-gram & quad-gram & penta-gram:")
            print('TP', tp, 'TN', tn, 'FP', fp, 'FN', fn, 'accuracy', accuracy, 'recall', recall, 'precision',
                  precision)
    print("")


    return tp,tn,fp,fn,accuracy,recall,precision


#             ---------------------------<<<<<Gathering the data for the necessary experiements >>>>>----------------------------

#a function use to run all of the experiments
def get_results(data,number):
    #Generating printing outputs per dataset
    if number == 1:
        print("Generating results for Amazon dataset")
    elif number == 2:
        print("Generating results for IMDB dataset")
    else:
        print("Generating results for YELP dataset")
    #             --------------<<<<<Seperate >>>>>------------------
    encode1, l1 = n_gram_GetTraining(data, 1)
    encode2, l2 = n_gram_GetTraining(data, 2)
    encode3, l3 = n_gram_GetTraining(data, 3)
    encode4, l4 = n_gram_GetTraining(data, 4)
    encode5, l5 = n_gram_GetTraining(data, 5)

    #             --------------<<<<<Combined >>>>>------------------
    c1 = np.hstack((encode1, encode2))
    c2 = np.hstack((c1, encode3))
    c3 = np.hstack((c2, encode4))
    c4 = np.hstack((c3, encode5))
    #calling the classification functions with respect to the data and ngrams
    classifcation(encode1, l1, 10, 's', 1)
    classifcation(encode2, l1, 10, 's', 2)
    classifcation(encode3, l1, 10, 's', 3)
    classifcation(encode4, l1, 10, 's', 4)
    classifcation(encode5, l1, 10, 's', 5)

    classifcation(c1, l1, 10, 'c', 1)
    classifcation(c2, l1, 10, 'c', 2)
    classifcation(c3, l1, 10, 'c', 3)
    classifcation(c4, l1, 10, 'c', 4)
    return

#             ---------------------------<<<<<Getting the results>>>>>----------------------------
#d1 = get_results(data1,1)
#d2 = get_results(data2,2)
#d3 = get_results(data3,3)

data = data2

encode1, l1 = n_gram_GetTraining(data, 1)
encode2, l2 = n_gram_GetTraining(data, 2)
encode3, l3 = n_gram_GetTraining(data, 3)
encode4, l4 = n_gram_GetTraining(data, 4)
encode5, l5 = n_gram_GetTraining(data, 5)

#             --------------<<<<<Combined >>>>>------------------
c1 = np.hstack((encode1, encode2))
c2 = np.hstack((c1, encode3))
c3 = np.hstack((c2, encode4))
c4 = np.hstack((c3, encode5))
classifcation(c4, l1, 10, 'c', 4)
#references Links :https://realpython.com/python-range/ reversed
#http://www.albertauyeung.com/post/generating-ngrams-python/
#https://stackoverflow.com/questions/13423919/computing-n-grams-using-python
#https://docs.scipy.org/doc/numpy-1.10.1/reference/generated/numpy.hstack.html
#https://www.afternerd.com/blog/python-sort-list/
#https://stackoverflow.com/questions/15943769/how-do-i-get-the-row-count-of-a-pandas-dataframe