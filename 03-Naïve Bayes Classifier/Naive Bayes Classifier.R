# Let us get the appropriate libraries loaded for NB Classifier. 
#install.packages("e1071")
library("e1071")
## ================================= Part 1 ===================================== ##

# read the data into a table from the file
sample <- read.table("sample1.csv", header=TRUE, sep=",")

# we will now define the data frames to use the NB classifier
traindata <- as.data.frame(sample[1:14,])
testdata <- as.data.frame(sample[15,])

#Display data frames
traindata
testdata

# [1] Bulid the model manually -----------------------------
# Step 1: ... Prioi Probabilities

traindata$Enrolls <- factor(traindata$Enrolls)
tprior <- table(traindata$Enrolls) # table: to get no. of occurence (frequency)
tprior <- tprior/sum(tprior) # to get probability --> prior probability (probability of each class in data set)

# [Note]: 
#vector/vector        : divides each element with the corresponding element
#vector/number        : divides each element by this number
#vector of rows/vector: divides each row by the corresponding element
#                       and the row itself is a vector (so vector/element)

# Step 2: ... Conditional Probabilities
ageCounts <-table(traindata[,c("Enrolls", "Age")]) # count no. of occurence of each "Enrolls" value with each each "Age" value
ageCounts <- ageCounts/rowSums(ageCounts) # the result is conditional probability.

incomeCounts <- table(traindata[,c("Enrolls", "Income")])
incomeCounts <- incomeCounts/rowSums(incomeCounts)

jsCounts <- table(traindata[,c("Enrolls", "Jobsatisfaction")])
jsCounts<-jsCounts/rowSums(jsCounts)

desireCounts <- table(traindata[,c("Enrolls", "Desire")])
desireCounts <- desireCounts/rowSums(desireCounts)

# Step 3: ... Posterior Probability
# Predict - Compute the probabilities for Age<=30, Income = Medium,
# Jobsatisfaction = yes and Desire = Fair --> the 15th row in the file
pyes <- 
  ageCounts["Yes","<=30"]*
  incomeCounts["Yes","Medium"]*
  jsCounts["Yes","Yes"]*
  desireCounts["Yes","Fair"]*
  tprior["Yes"]
pno <- 
  ageCounts["No","<=30"]*
  incomeCounts["No","Medium"]*
  jsCounts["No","Yes"]*
  desireCounts["No","Fair"]*
  tprior["No"]
print (pyes)
print (pno)
print(max(pyes,pno)) # the class of the max(pyes, pno) is the predicted class - here is "yes"


# [2] use the NB classifier -------------------------------
model <- naiveBayes(Enrolls~.,traindata)
# display model
model
# predict with testdata
results <- predict (model, testdata)
# display results
results

# use the NB classifier with Laplace smoothing
model1 <- naiveBayes(Enrolls ~.,traindata,laplace=.01)
# display model
model1
# predict with testdata
results1 <- predict (model1,testdata)
# display results
results1

