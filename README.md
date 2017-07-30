---
title: "Practical Machine Learning"
author: "Vineet Jaiswal"
date: "July 30, 2017"
output: html_document
---

## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).


## Data

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv


The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

## What you should submit

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

## Detail Analysis 

### Getting the data and do primary analysis 

The very first thing is getting the data from specified location and do the primary analysis to be familiar with the records on which you are going to play

```{r}
trainingData <- read.csv("D:\\ML\\Assignment\\Practical-Machine-Learning\\pml-training.csv",na.strings = c("NA","", "#DIV/0!"))

testingData <- read.csv("D:\\ML\\Assignment\\Practical-Machine-Learning\\pml-testing.csv",na.strings = c("NA","", "#DIV/0!"))

dim(trainingData)
dim(testingData)

# Testing to training difference 
setdiff(colnames(testingData), colnames(trainingData))
# Trainging to testing difference
setdiff(colnames(trainingData), colnames(testingData))

```

With the analysis, we observed that training data has 19622 records and testing data has 20 records, both has 160 column but training has column "Classe" and testing has column "problem_id"

### Cleaning the data for better result

Clean the data like if row has more than 50% NA and also remove unwanted columns which is not useful for this analysis and it may impact our predication 

```{r}
# Remove initial 7 columns which is not useful for our analysis 
trainingData <- trainingData[, -c(1:7)]
testingData <- testingData[, -c(1:7)]

dim(trainingData)
dim(testingData)

# Remove the column which has only NA values 
trainingData<-trainingData[,colSums(is.na(trainingData)) == 0]
testingData <-testingData[,colSums(is.na(testingData)) == 0]

dim(trainingData)
dim(testingData)
```

### Partition training data and visualisation 

To perform validation test, we need to partition the training data(60:40) and visualize the data based on "Classe"

```{r}
library(caret)
set.seed(2000)

partTraining <- createDataPartition(y = trainingData$classe, p=0.6, list = F)

secTrain <- trainingData[partTraining,]
secTest <- trainingData[-partTraining,]

dim(secTrain)
dim(secTest)

ggplot(secTrain, aes(classe)) + geom_bar(colour = "red", fill="blue") + xlab("classe") + ylab("count") + ggtitle("Class vs Frequency for secTrain")

ggplot(secTest, aes(classe)) + geom_bar(colour = "red", fill="blue") + xlab("classe") + ylab("count") + ggtitle("Class vs Frequency for secTest")

```

### Implement first machine learning algorithm : decision tree

*Decision tree making and visualization

```{r}
library(rpart)
library(rattle)

dt <- rpart(classe~., data = secTrain, method="class")

# View descision tree in more user intractive way
fancyRpartPlot(dt)
  
```

* Prediction using classification decision tree 

```{r}
predictionDT <-  predict(dt, secTest, type = "class")

# Create confusion matrix to validate the records
confusionMatrix(predictionDT,secTest$classe)
```

### Implement second machine learning model : random forest 

* Creating random forest

```{r}
library(randomForest)
rf <- randomForest(classe~., data= secTrain, method = "class")

```

* Predicting on random forest classification
```{r}
predictionRF <- predict(rf, secTest, type="class")

confusionMatrix(predictionRF, secTest$classe)
```

### Which machile learning algorithm to choose 

As per both confusion matix result, Random forest gives 99% accuracy and decision tree gives 70%, so final analsis will be done on basis for random forest. With random forest, out of sample error is approximate 0.79%

## Final Prediction

final prediction based on random forest model 

```{r}
finalPrediction <-  predict(rf, testingData, type = "class")
finalPrediction
```
