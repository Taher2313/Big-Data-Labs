#########################################################################
################################### 1 ###################################
#########################################################################

# First of all, start by cleaning the workspace and setting the working directory.
rm(list=ls())

# set working directory
setwd("/Users/taher/Desktop/Second Semester/Big Data/Assignments/Assignment 1")

#########################################################################
################################### 2 ###################################
#########################################################################

# Import the dataset titanic.csv into a data frame.
dfm <- read.csv("titanic.csv")

#########################################################################
################################### 3 ###################################
#########################################################################

# a.Show the dimensions of the data frame
dim(dfm)

# b.Show the structure of the data frame
str(dfm)

# c.Get more insight into data by exploring the first and the last TEN rows in the dataset
head(dfm, 10)
tail(dfm,10)

# d.Show summary of all variables in the data frame
summary(dfm)

#########################################################################
################################### 4 ###################################
#########################################################################

# a.Show a summary for the variable age only
summary(dfm$Age)

# b.What are the first and third quartile values for this variable? What do these values mean?
# we can get these values from the summery also but this is another way to get the quartiles only
quantile(dfm$Age, probs = c(0.25,0.75) , na.rm = TRUE )

# the first quartile is 20.125 
# the first quartile is the value under which 25% of data points are found when they are arranged in increasing order
# or we can say that is the value that falls between the smallest value of the dataset and the median

# the third quartile is 38
# the third quartile is the value under which 75% of data points are found when they are arranged in increasing order
# or we can say that is the value that falls between the largest value of the dataset and the median

# c.Are there any missing values in the variable age? (i.e. written as <NA>)?
# yes 
anyNA(dfm$Age)

# d.What is the type of the variable embarked? Show the levels of this variable. Is that what you were expecting?
# type   ==>> character 
# levels ==>> ""  "C" "Q" "S"
# not as we expected as there is an empty string in the levels
typeof(dfm$Embarked)
levels(factor(dfm$Embarked))

# e.Can you conclude what’s needed at this step in the data analysis cycle?
# data need preprocessing

#########################################################################
################################### 5 ###################################
#########################################################################

# a.Remove the rows containing <NA> in the age variable from the data frame.
dfm <- subset(dfm, !is.na(Age))  

# b.Remove the rows containing any unexpected value in the embarked variable from the dataset
dfm <- subset(dfm,  Embarked != "")

# c.Now, check that no NA values exist in the age variable. Also, factor the embarked variable and display its levels. Is that what you are expecting?
# use it is what i expected
anyNA(dfm$Age)
levels(factor(dfm$Embarked))

# d.Some variables are not very interesting and provide no real indicative value. Remove columns Cabin and Ticket from the dataset.
dfm <- subset(dfm, select = -c(Cabin,Ticket))

#########################################################################
################################### 6 ###################################
#########################################################################

# a.Show the number of males and females aboard the Titanic.
table1 = table(dfm$Gender)
print(table1)

# b.Plot a pie chart showing the number of males and females aboard the Titanic
pie(table1)

# c1.Indicate males with a blue color and females with a red color in the above plot.
pie(table1, col = c("red", "blue"))

# c2.Show the number of people who survived and didn’t survive from each gender
table2 = table(dfm$Gender , dfm$Survived)
print(table2)

# d.Plot a pie chart showing the number of males and females who survived only.
survived = dfm[dfm$Survived == 1 ,]
table3 = table(survived$Gender)
pie(table3)

# e.What do you conclude from that?
# it is obvious that the number of the survived females is larger , althought the number of males 
# is bigger the number of females in total

# f.how the relationship between social class and survival i.e. show how many people survived and how many people didn’t survive from each class.
table4 = table(dfm$Survived , dfm$Pclass)
print(table4)

# g.Plot this relationship as a stacked bar plot
barplot(table4)

# h.Indicate survived passengers with a blue color and un-survived passengers with a red color in the above plot
barplot(table4 , col = c("red" , "blue"))

# i.What do you conclude from that?
# the ratio of survivors inversly proportional to the social class
# lower social class have highest survivors ratio and higher social class have lowest survivors ratio

# j.Plot a box and whiskers plot for the variable age 
boxplot(dfm$Age)

# k.What does this plot mean?
# this graph show the upper and lower extremes values of the age
# it also show the 1st, 3rd quartiles and median
# it also show the outliers and their values (the points above the upper extreme)

# l.Plot a density distribution for the variable age
plot(density(dfm$Age))

#########################################################################
################################### 7 ###################################
#########################################################################

# Remove all columns but passenger name and whether they survived or not. Export the new dataset to a file named “titanic_preprocessed.csv”
dfm <- subset(dfm, select = c(Name,Survived))
write.csv(dfm, 'titanic_preprocessed.csv', row.names=TRUE)

