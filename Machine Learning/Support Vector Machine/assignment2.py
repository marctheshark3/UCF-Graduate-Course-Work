from __future__ import division
import time
import pandas as p
from sklearn import svm
from sklearn.model_selection import train_test_split , GridSearchCV , RepeatedKFold
from sklearn.metrics import confusion_matrix

data = 'https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data'

glass = p.read_csv(data, sep = ',', names = ['Id number', 'RI' , 'Na ', 'Mg', 'Al', 'Si' , 'K','Ca' , 'Ba' ,'Fe' , 'Classifier'])
#splitting predictors and observations
predictors= glass.Classifier
observation = glass.drop(['Classifier','Id number'], axis= 1)

train_obs, test_obs, train_predictors, test_predictors = train_test_split( observation, predictors,test_size=0.2,random_state=333, stratify=predictors)
cross_val = RepeatedKFold(n_splits=5, n_repeats=5, random_state=333)

hyperparameters_part1 = [{'kernel': ['rbf'], 'gamma' : [1, .1, .01, 0.001, 0.0001],  'C': [1, 10, 100, 1000]},
                   {'kernel': ['linear'], 'gamma' : [1, .1, .01, 0.001, 0.0001] ,  'C': [1, 10, 100, 1000]},
                   {'kernel': ['poly'], 'gamma' : [1, .1, .01, 0.001, 0.0001],  'C': [1, 10, 100, 1000]},
                   {'kernel': ['sigmoid'], 'gamma' : [1, .1, .01, 0.001, 0.0001],  'C': [1, 10, 100, 1000]}]


hyperparameters_part2 = [{'kernel': ['rbf'], 'gamma' : [1, .1, .01, 0.001, 0.0001],   'C': [1, 10, 100, 1000], 'decision_function_shape': ['ovr']},#,'class_weight' :['balanced' , None]},
                   {'kernel': ['linear'], 'gamma' : [1, .1, .01,0.001, 0.0001] , 'C': [1, 10, 100, 1000], 'decision_function_shape': ['ovr']},#,'class_weight' :['balanced' , None]},
                   {'kernel': ['poly'], 'gamma' : [1, .1, .01, 0.001, 0.0001] , 'C': [1, 10, 100, 1000], 'decision_function_shape': ['ovr']},#,'class_weight' :['balanced' , None]},
                   {'kernel': ['sigmoid'], 'gamma' : [1, .1, .01, 0.001, 0.0001] , 'C': [1, 10, 100, 1000], 'decision_function_shape': ['ovr']}]#,'class_weight' :['balanced' , None]}]

hyperparameters_part3 = [{'kernel': ['rbf'], 'gamma' : [1, .1, .01, 0.001, 0.0001],   'C': [1, 10, 100, 1000],'class_weight' :['balanced']},
                   {'kernel': ['linear'], 'gamma' : [1, .1, .01,0.001, 0.0001] , 'C': [1, 10, 100, 1000],'class_weight' :['balanced']},
                   {'kernel': ['poly'], 'gamma' : [1, .1, .01, 0.001, 0.0001] , 'C': [1, 10, 100, 1000] ,'class_weight' :['balanced']},
                   {'kernel': ['sigmoid'], 'gamma' : [1, .1, .01, 0.001, 0.0001] , 'C': [1, 10, 100, 1000],'class_weight' :['balanced']}]

def function(m):
    start = time.time()
    if m == 1:

        # starting the gridsearch with the crossvalidation
        classifier = GridSearchCV(estimator=svm.SVC(),
                                  param_grid=hyperparameters_part1, cv=cross_val)

        classifier.fit(train_obs, train_predictors)

        print "generated results for the data"
        means = classifier.cv_results_['mean_test_score']
        stds = classifier.cv_results_['std_test_score']
        for mean, std, params in zip(means, stds, classifier.cv_results_['params']):
            print("%0.4f (+/-%0.03f) for %r" % (mean, std * 2, params))
        print ''
        print "The Best Training Score was:", classifier.best_score_
        print ''
        print "The Best Parameters were: ", classifier.best_params_
        print ' '

        # setting the optimal values to variables
        kern = classifier.best_estimator_.kernel
        c = classifier.best_estimator_.C
        g = classifier.best_estimator_.gamma
        d = classifier.best_estimator_.decision_function_shape

        optimalrun = svm.SVC(kernel=kern, C=c, gamma=g, decision_function_shape= d , random_state= 333).fit(train_obs, train_predictors)
        pred = optimalrun.predict(test_obs)

        # model accuracy creating confusion matrix
        accuracy = optimalrun.score(test_obs, test_predictors)
        cm = confusion_matrix(test_predictors, pred)

        print accuracy, 'is the models test accuracy part 1'

        end = time.time()
        print 'Completion Time:',(end - start)
    elif m==2:
        # starting the gridsearch with the crossvalidation
        classifier = GridSearchCV(estimator=svm.SVC(),
                                  param_grid=hyperparameters_part2, cv=cross_val)
        # only need balanced for the last questions
        # not sure why the accuacryies arent changing in the part classifier
        classifier.fit(train_obs, train_predictors)

        print "generated results for the data"
        means = classifier.cv_results_['mean_test_score']
        stds = classifier.cv_results_['std_test_score']
        for mean, std, params in zip(means, stds, classifier.cv_results_['params']):
            print("%0.4f (+/-%0.03f) for %r" % (mean, std * 2, params))
        print ''
        print "The Best Training Score was:", classifier.best_score_
        print ''
        print "The Best Parameters were: ", classifier.best_params_
        print ' '

        # setting the optimal values to variables
        kern = classifier.best_estimator_.kernel
        c = classifier.best_estimator_.C
        g = classifier.best_estimator_.gamma
        d = classifier.best_estimator_.decision_function_shape

        optimalrun = svm.SVC(kernel=kern, C=c, gamma=g, decision_function_shape=d, random_state=333).fit(train_obs,
                                                                                                         train_predictors)
        pred = optimalrun.predict(test_obs)

        # model accuracy creating confusion matrix
        accuracy = optimalrun.score(test_obs, test_predictors)
        cm = confusion_matrix(test_predictors, pred)

        print accuracy, 'is the models test accuracy part 2'

        print 'One Vs rest'
        end = time.time()
        print 'completion time:' , (end - start)

    elif m == 3:
        # starting the gridsearch with the crossvalidation
        classifier = GridSearchCV(estimator=svm.SVC(),
                                  param_grid=hyperparameters_part3, cv=cross_val)

        classifier.fit(train_obs, train_predictors)

        print "generated results for the data"
        means = classifier.cv_results_['mean_test_score']
        stds = classifier.cv_results_['std_test_score']
        for mean, std, params in zip(means, stds, classifier.cv_results_['params']):
            print("%0.4f (+/-%0.03f) for %r" % (mean, std * 2, params))
        print ''
        print "The Best Training Score was:", classifier.best_score_
        print ''
        print "The Best Parameters were: ", classifier.best_params_
        print ' '

        # setting the optimal values to variables
        kern = classifier.best_estimator_.kernel
        c = classifier.best_estimator_.C
        g = classifier.best_estimator_.gamma
        #d = classifier.best_estimator_.decision_function_shape
        cw = classifier.best_estimator_.class_weight

        optimalrun = svm.SVC(kernel=kern, C=c, gamma=g, random_state=333 , class_weight= cw).fit(train_obs,
                                                                                                         train_predictors)
        pred = optimalrun.predict(test_obs)

        # model accuracy creating confusion matrix
        accuracy = optimalrun.score(test_obs, test_predictors)
        cm = confusion_matrix(test_predictors, pred)

        print accuracy, 'is the models test accuracy part 3'

        end = time.time()
        print 'Completion Time:', (end - start)

#function(1)



#function(2)
function(3)
