import numpy as np
import pandas as pd
from sklearn.preprocessing import LabelEncoder, StandardScaler
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier
from sklearn.model_selection import train_test_split, RandomizedSearchCV,cross_val_score
from sklearn.metrics import  confusion_matrix , classification_report, roc_auc_score, roc_curve, auc,accuracy_score
import matplotlib.pyplot as plt
from pandas.plotting import scatter_matrix
from sklearn.metrics import accuracy_score as acc
from mlxtend.feature_selection import SequentialFeatureSelector as sfs
from sklearn.neural_network import MLPClassifier
#from sklearn.grid_search import RandomizedSearchCV

from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import cross_val_score
from sklearn import metrics
from sklearn.model_selection import GridSearchCV
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import RandomizedSearchCV
def pre_processing():

    shroom = pd.read_csv("/Users/marctheshark/Documents/MODELING AND SIMULATION/Machine Learning/Final Project/Data/mushrooms.csv")


    #print(shroom.head(5))

    #checking to see if there are any null values
    nu = shroom.isnull().sum()

    #separating the classifcation variable either poisnious or edible
    shroom['class'].unique()
    #normalizing labels
    label_norm = LabelEncoder()
    for i in shroom.columns:
        shroom[i] = label_norm.fit_transform(shroom[i])

    X = shroom.iloc[:,1:]
    Y = shroom.iloc[:,0]
    #print (X.shape) #22 predictors


    #looking at the correlations between variables
    #print(shroom.corr())


    normalizing = StandardScaler()
    X=normalizing.fit_transform(X)



    shroom_split1 = shroom.iloc[:,:6]
    shroom_split2 = shroom.iloc[:,6 :12]
    shroom_split3 = shroom.iloc[:, 12:18]
    shroom_split4 = shroom.iloc[:, 18:22]
    
    shroom_split1.hist()
    shroom_split2.hist()
    shroom_split3.hist()
    shroom_split4.hist()
    #plt.show()
    #print(list(shroom))

    correlations = shroom.corr()
    # plot correlation matrix
    fig = plt.figure()
    ax = fig.add_subplot(111)
    cax = ax.matshow(correlations, vmin=-1, vmax=1)
    fig.colorbar(cax)
    #ticks = np.arange(0, 9, 1)
    #ax.set_xticks(ticks)
    #ax.set_yticks(ticks)
    ax.set_xticklabels(list(shroom))
    ax.set_yticklabels(list(shroom))
    #plt.show()

    scatter_matrix(shroom)
    #plt.show()

    print('Pre-Processing Done')


    return X, Y

#Splitting test and train split
def training_test(process):
    X, Y = process
    X_obs, Y_obs, X_labels, Y_labels = train_test_split(X, Y, test_size=.2, random_state=333, shuffle=True)


    return X_obs, Y_obs, X_labels, Y_labels

#training_test(pre_processing())

def ada():
    print('starting ADA')
    train, test, xlab, ylab = training_test(pre_processing())
    #parameters = {'n_estimators': range(10, 100, 10), 'algorithm': {'SAMME', 'SAMME.R'}}
    ada_boost = AdaBoostClassifier(n_estimators= 100, random_state= 333)
    #cv = cross_val_score(ada_boost,train,test,cv = 10 , n_jobs= -1 )
    ada_boost.fit(train,xlab)
    print ( ada_boost.score(test,ylab))
    ypred = ada_boost.predict(test)
    cm = confusion_matrix(ylab,ypred)
    print(cm)
    print('Accuracy for AdaBoost: ' + str(accuracy_score(ylab, ypred)))



def LDA():
    print('starting LDA')
    from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
    train, test, xlab, ylab = training_test(pre_processing())

    print('training')
    classifier = LinearDiscriminantAnalysis()
    classifier.fit(train,xlab)

    ypred = classifier.predict(test)
    cm = confusion_matrix(ylab, ypred)
    print(cm)
    print('Accuracy for LDA: ' + str(accuracy_score(ylab, ypred)))


    return

def LR():

    tuned_parameters = {'C': [0.001, 0.01, 0.1, 1, 10, 100, 1000],
                        'penalty': ['l1', 'l2']
                        }
    lr = LogisticRegression()
    logreg = GridSearchCV(lr, tuned_parameters, cv=10)



    train, test, xlab, ylab = training_test(pre_processing())

    logreg.fit(train,xlab)
    print(logreg.best_score_, 'Best Score for 10-fold CV LR ')
    print(logreg.best_params_, 'Best Params for LR')
    y_prob = logreg.predict_proba(test)[:, 1]  # This will give you positive class prediction probabilities
    y_pred = np.where(y_prob > 0.5, 1, 0)  # This will threshold the probabilities to give class predictions.
    logreg.score(test, y_pred)

    confusion_matrix = metrics.confusion_matrix(ylab, y_pred)
    print(confusion_matrix, "LR")



    return


def RF():

    train, test, xlab, ylab = training_test(pre_processing())

    rf = RandomForestClassifier()

    tuned_parameters = {'min_samples_leaf': range(10, 100, 10), 'n_estimators': range(10, 100, 10),
                        'max_features': ['auto', 'sqrt', 'log2']
                        }
    randfor = RandomizedSearchCV(rf, tuned_parameters, cv=10, scoring='accuracy', n_iter=20, n_jobs=-1)

    randfor.fit(train,xlab)
    print(randfor.best_score_, 'Best Score for 10-fold CV RF ')
    print(randfor.best_params_, 'Best Params for RF')
    y_prob = randfor.predict_proba(test)[:, 1]
    y_pred = np.where(y_prob > 0.5, 1, 0)
    randfor.score(test, y_pred)

    confusion_matrix = metrics.confusion_matrix(ylab, y_pred)
    print(confusion_matrix, 'RF')



print('running LR')
LR()
RF()
