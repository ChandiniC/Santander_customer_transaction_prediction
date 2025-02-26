# Santander_customer_transaction_prediction
1.1    Problem Statement

At Santander, mission is to help people and businesses prosper. We are always looking for ways to help our customers understand their financial health and identify which products and services might help them achieve their monetary goals. Our data science team is continually challenging our machine learning algorithms, working with the global data science community to make sure we can more accurately identify new ways to solve our most common challenge, binary classification problems such as: is a customer satisfied? Will a customer buy this product? Can a customer pay this loan? In this challenge, we need to identify which customers will make a specific transaction in the future, irrespective of the amount of money transacted.

1.2    Data

By looking at the problem statement, we can say that it is a binary classification problem. Here, our task is to build a classification model which will classify customers who will make transaction in future and who will not transact money based on the ‘target’ variable. 
The data provided here has the same structure as the real data they have available to solve this problem. Models are evaluated on area under the ROC curve between the predicted probability and the observed target. The dataset is given in two files labeled "train.csv" and "test.csv", 200k observation and 202 variables each. Variables are ID_code, target, and var_0 to var_199. The data in test.csv does not have target variable therefore cannot be used in testing the model. The data in train.csv file much be split (80/20) for training and testing the model. A sample of data sets used is given below:
