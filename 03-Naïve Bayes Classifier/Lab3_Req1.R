# Install the e1071 package
# install.packages("e1071")

# Import the packge 
library("e1071")

#########################################################################
################################### 1 ###################################
#########################################################################

# First of all, start by cleaning the workspace and setting the working directory.
rm(list=ls())

# set working directory
setwd("/Users/taher/Desktop/Second Semester/Big Data/Assignments/Assignment 3")

#########################################################################
################################### 2 ###################################
#########################################################################

# Import the dataset nbtrain.csv into a data frame. 
dfm <- read.table("nbtrain.csv", header=TRUE, sep=",")

# What are the variables of this data ?
colnames(dfm)
# 1-> age , 2-> gender , 3-> educ , 4-> income

#########################################################################
################################### 3 ###################################
#########################################################################

# Divide the data into two data frames: a training set containing the first 9000 rows,
# and a test set containing the remaining rows.
train_dfm <- as.data.frame(dfm[1:9000,])
test_dfm <- as.data.frame(dfm[9001:nrow(dfm),])

# Why do we split data into training and test sets?
# We split data into training and test sets, so we use the training set to train the model
# and then use the test set to evaluate the model's performance and assess its ability to 
# generalize to new, unseen data.

#########################################################################
################################### 4 ###################################
#########################################################################

# Train a Naïve Bayes Classifier model with income as the target variable and all
# other variables as independent variables. Smooth the model with Laplace
# smoothing coefficient = 0.01
model <- naiveBayes(income~.,train_dfm,laplace=0.01)

# First of all, let's define what is Laplace smoothing first and what is the problem it solves
# Laplace smoothing is a technique used to smooth probability estimates
# If a feature is not present in the training set for a particular class, 
# the probability estimate will be zero, and the classifier will assign zero probability 
# to that class for any input that includes that feature. 
# This can lead to overfitting, especially when the number of training instances is small.

# What does Laplace smoothing coefficient mean?
# Laplace smoothing coefficient in Naïve Bayes classifiers controls 
# the amount of smoothing applied to the probability estimates for features 
# that are not present in the training set for a particular class

#########################################################################
################################## 5 ##################################
#########################################################################

# Display the resulting model.
model

#########################################################################
################################### 6 ###################################
#########################################################################

# Use the model to predict the income values of the test data
results <- predict (model,test_dfm)

#########################################################################
################################### 7 ###################################
#########################################################################

# Display a confusion matrix for the predict values of the test data versus the actual values. 

# Construct the confusion matrix
# predictions are on the rows
# true labels are on the columns
confusion_table <- table(results, test_dfm$income)

# Display the confusion matrix
confusion_table

# Investigate the results.
# 10-50K class is the most correctly classified class in the test data 
# then class GT 80K comes with very low correctness rate ,and finally
# class GT 80K with zero correctness rate.

# Explain the variation in the model’s classification power across income classes.
# class 10-50K is over-represented in the training set so it have a high A-priori probability
# which makes the model gives this class higher weight and classify it frequently
# The other 2 classes are under-represented, so we can see that their correctlt 
# classified points are low

#########################################################################
################################### 8 ###################################
#########################################################################

# Display the accuracy of the model. 
accuracy <- sum(diag(confusion_table)) / sum(confusion_table)
cat("Accuracy: ", round(accuracy, 5), "\n")

#Comment on the result.
# As the classes of the data are imbalanced , the accuracy value is high, although one the classes 
# is classified wrong with percentage 100% and one is 90% wrongly classified
# We may use another metric for the performance like f1-score

#########################################################################
################################### 9 ###################################
#########################################################################

# Display the overall 10-50K, 50-80K, GT 80K misclassification rates.
misclassification_rates <- c(1 - (sum(diag(confusion_table)[1]) / sum(confusion_table[,1])),
                             1 - (sum(diag(confusion_table)[2]) / sum(confusion_table[,2])),
                             1 - (sum(diag(confusion_table)[3]) / sum(confusion_table[,3])))

cat("Misclassification rate for 10-50K: ", round(misclassification_rates[1], 5) * 100, "% \n")
cat("Misclassification rate for 50-80K: ", round(misclassification_rates[2], 5) * 100, "% \n")
cat("Misclassification rate for GT 80K: ", round(misclassification_rates[3], 5) * 100, "% \n")

