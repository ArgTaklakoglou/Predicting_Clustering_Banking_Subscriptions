# Predicting_Clustering_Banking_Subscriptions

In this project, I used a dataset containing bank customers’ data which was collected through phone calls. The data includes various variables about each customer and whether they Subscribed to a bank’s service or not. The dataset consist of around 38.000 records. The aim of this project is to try 4 different classification methods in order to predict whether a customer will subscribe or not, taking into consideration the data about them.
The classification methods I used are the following:

Naïve Bayes Classifier
K-Nearest Neighbor
Random Forest Classification

To compare each method, the F1 metric will be used to evaluate the predictions, and the method with the highest F1 score will be chosen.

In the end of the project, I also attempt to cluster the customers by taking into account specific variables about them.
I used  pam() functon with the matrix of Gower Distance to create the clusters and I evaluate the clusters' quality by plotting the silhouette value.


Dataset can be found here: https://www.kaggle.com/datasets/pankajbhowmik/bank-marketing-campaign-subscriptions
