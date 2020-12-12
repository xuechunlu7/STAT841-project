#Import Libraries
import numpy as np
from numpy import array
from numpy import argmax
import pandas as pd
from csv import reader

import sklearn
import sklearn.datasets
from sklearn import metrics
from sklearn import linear_model
from sklearn.metrics import accuracy_score
from sklearn.metrics import roc_auc_score
from sklearn.model_selection import train_test_split

from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import RepeatedStratifiedKFold
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import mean_absolute_error
from sklearn.metrics import mean_squared_error

#Read a CSV file and create a list containing the loaded dataset
def load_csv(filename):
	dataset = list()
	with open(filename, 'r') as file:
		csv_reader = reader(file)
		for row in csv_reader:
			if not row:
				continue
			dataset.append(row)
	return dataset

# Convert string column to float
def str_column_to_float(dataset, column):
	for row in dataset:
		row[column] = float(row[column].strip())

pca_train=pd.read_csv("pca_20_attr_training_data.csv")
pca_test=pd.read_csv("pcr_20_attr_test_data.csv")

model = LogisticRegression()
solvers = ['newton-cg', 'lbfgs', 'liblinear']
penalty = ['l2']
c_values = [100, 10, 1.0, 0.1, 0.01]

# define grid search
grid = dict(solver=solvers,penalty=penalty,C=c_values)
cv = RepeatedStratifiedKFold(n_splits=10, n_repeats=3, random_state=1)
grid_search = GridSearchCV(estimator=model, param_grid=grid, n_jobs=-1, cv=cv, 
                           scoring='accuracy',error_score=0)
grid_result = grid_search.fit(X, y)

print("Best: %f using %s" % (grid_result.best_score_, grid_result.best_params_))
means = grid_result.cv_results_['mean_test_score']
stds = grid_result.cv_results_['std_test_score']
params = grid_result.cv_results_['params']
for mean, stdev, param in zip(means, stds, params):
    print("%f (%f) with: %r" % (mean, stdev, param))


logisticRegr = LogisticRegression(C=100, class_weight=None, dual=False, fit_intercept=True,
                   intercept_scaling=1, l1_ratio=None, max_iter=100,
                   multi_class='ovr', n_jobs=None, penalty='l2', random_state=0,
                   solver='newton-cg', tol=0.0001, verbose=0, warm_start=False)

logisticRegr.fit(X, y)
training_score = logisticRegr.score(X,y)
test_score = logisticRegr.score(X_test,y_test)
y_test = test[['Response']].copy()
X_test = test.drop('Response', axis=1)
X_test = X_test.drop('Id', axis=1)
cfs_predictions = logisticRegr.predict(X_test)
mean_absolute_error(y_test,cfs_predictions)
mean_squared_error(y_test,cfs_predictions)

#X=X.drop('Id',axis=1)
y = train[['Response']].copy()

pca_y_train= pca_train[['Response']].copy()
pca_y_test= pca_test[['Response']].copy()
pca_X_train=pca_train.drop('Id',axis=1)
pca_X_train=pca_X_train.drop('Response',axis=1)
pca_X_train=pca_X_train.drop('Unnamed: 22',axis=1)
pca_X_train=pca_X_train.drop('Unnamed: 23',axis=1)
pca_X_train=pca_X_train.drop('Unnamed: 24',axis=1)
pca_X_train=pca_X_train.drop('Unnamed: 25',axis=1)
pca_X_train=pca_X_train.drop('Unnamed: 26',axis=1)

pca_X_test=pca_test.drop('Id',axis=1)
pca_X_test=pca_X_test.drop('Response',axis=1)

logisticRegr.fit(pca_X_train, pca_y_train)
training_score = logisticRegr.score(pca_X_train, pca_y_train)
test_score = logisticRegr.score(pca_X_test,pca_y_test)

pca_predictions = logisticRegr.predict(pca_X_test)
mean_absolute_error(y_test,pca_predictions)
mean_squared_error(y_test,pca_predictions)



    



