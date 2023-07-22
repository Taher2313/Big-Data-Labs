# install.packages("rpart.plot")
# install.packages("ROCR")
library("rpart")
library("rpart.plot")
library("ROCR")

# First of all, start by cleaning the workspace and setting the working directory.
rm(list=ls())

# set working directory
setwd("/Users/taher/Desktop/Second Semester/Big Data/Assignments/Assignment 3")



#Read the data
play_decision <- read.table("DTdata.csv",header=TRUE,sep=",")
play_decision
summary(play_decision)

#Build the tree to "fit" the model
fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=2, maxdepth = 3),
             parms=list(split='information'))
# split='information' : means split on "information gain" 
#plot the tree
rpart.plot(fit, type = 4, extra = 1)

summary(fit)
#######################################################################################
# Q1: what is the default value for split?  
# the default of the split is gini (for using the Gini index which measures the disorder of a set of elements),
# the used value above (information) for using the information gain

# Q2: what are the meanings of these control parameters?  
#
#          1- "minsplit=2"
#
#               This parameter determines the minimum number of observations required in a node 
#               before the algorithm tries to split it further. For example, if the minimum split 
#               is set to 2, a node with only one observation will not be split.
#              
#          2- "maxdepth=3" 
#
#              This parameter specifies the maximum depth of the decision tree, which is the number 
#              of levels from the root node to the terminal nodes. By setting the maximum depth, 
#              we can control the complexity of the tree and avoid overfitting. The depth of the
#              root node is zero.
#
#          3- "minbucket=4" 
# 
#              This parameter determines the minimum number of observations required in a terminal node, 
#              which is a leaf node that cannot be split further. By setting a minimum number of observations, 
#              we can prevent the tree from being too specific to the training data and increase its generalizability.
#
# Support your answers with graphs for different values of these parameters.

# By increasing the minsplit to 4 it couldn't split further in the left sub tree as it have only 3 observations
rpart.plot(rpart(Play ~ Outlook + Temperature + Humidity + Wind,
                 method="class", 
                 data=play_decision,
                 control=rpart.control(minsplit=4, maxdepth = 3),
                 parms=list(split='information')), type = 4, extra = 1)

# By decreaing the maxdepth to one , the tree have only two levels 
rpart.plot(rpart(Play ~ Outlook + Temperature + Humidity + Wind,
                 method="class", 
                 data=play_decision,
                 control=rpart.control(minsplit=2, maxdepth = 1),
                 parms=list(split='information')), type = 4, extra = 1)

#  By choosing the minbucket=5 the tree become one level only, as the left sub-tree 
# will have 3 observations if the split occurs
rpart.plot(rpart(Play ~ Outlook + Temperature + Humidity + Wind,
                 method="class", 
                 data=play_decision,
                 control=rpart.control(minsplit=2, minbucket = 5),
                 parms=list(split='information')), type = 4, extra = 1)

#Q3: What will happen if only one of either minsplit or minbucket is specified
#    and not the other?
# The code either sets minsplit to minbucket*3 or minbucket to minsplit/3

#Q4: What does 'type' and 'extra' parameters mean in the plot function?
# the type parameter specifies the type of plot to be generated.
# type = 1: text-based box plot
# type = 4: compact plot with variable labels and split labels
# The extra parameter is used to add additional graphical elements to the plot.
# extra = 1: add a residual plot to each terminal node
# extra = 4: add a distribution histogram of the response variable to each terminal node
# both parameters have other values

#Q5: Plot the tree with propabilities instead of number of observations in each node.
######################################################################################
rpart.plot(fit, type = 4, extra = 4) 

# test case 1
newdata <- data.frame(Outlook="overcast",Temperature="mild",Humidity="high",Wind=FALSE)
newdata
predict(fit,newdata=newdata,type=c("class"))
# type can be class, prob or vector for classification trees.

# test case 2
#Predict if Play is possible for condition rainy, mild humidity, high temperature and no wind
newdata <- data.frame(Outlook="rainy",Temperature="hot",Humidity="normal",Wind=FALSE)
newdata
predict(fit,newdata=newdata,type=c("class"))

######################################################################################
#Q6: What is the predicted class for this test case?
# yes ==> play is possible ==> for the both test cases

#Q7: State the sequence of tree node checks to reach this class (label).
# test case 1
# go to the left sub-tree where the temperature is mild
# then go to the right-sub tree where the outlook is overcast

# test case 2
# go to the right sub-tree where the temperature is hot
# then go to the right-sub tree where the wind is false

## ================================= END ===================================== ##

