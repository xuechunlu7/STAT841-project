# Data Cleaning

This part is mainly about dealing with missing data.

- Check for attributes that have more than 30% Missing data: 

The attributes that are showing more than 30% missing data would be dropped from the analysis
Therefore, attributes, Employment_Info_1, Employment_Info_4, Employment_Info_6, and Medical_History_1 are the only features, which are retained for further analysis. These four attributes will need to be treated to impute their missing values.

- Test for MCAR:

The data were tested for MCAR using the Little’s test (Little’s test revealed that the missing data are not entirely at random)

- Plot all variables based on the number of missing values that they have and see how the missing values are distributed.


- Missing Values Imputation using mice

Imputation, Analysis and Pooling:
The MICE (Multivariate Imputation via Chained Equations) package in R has been utilized to do the multiple imputations. The missing data were assumed to be MAR. The categorical variables were removed and only numeric attributes were used to do the imputation.
