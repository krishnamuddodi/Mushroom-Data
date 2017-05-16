#########################################################################################
# Name    : Krishna Chaitanya M                                                         #
# Project : Project - Mushroom Data                                                     #
# Date    : 11/04/2016                                                                  #
#                                                                                       #
#########################################################################################

##############################   PROJECT OVERVIEW   #####################################
#                                                                                       #
#                                                                                       #
#   1. About The Mushroom Data - Business Objective                                     #
#                                                                                       #
#   2. Project Setup - installing and loading packages                                  #
#                                                                                       #
#   3. Data Preprocessing                                                               #
#                                                                                       #
#   4. Data Visualization - Exploratory Analysis                                        #
#                                                                                       #
#   5. Data Modelling                                                                   #
#                                                                                       #
#   6. Findings                                                                         #
#                                                                                       #
#########################################################################################

##########################   1. About The Mushroom Data - Business Objective   ##########
#                                                                                       #
#   About the Dataset :                                                                 #
#                                                                                       #
#   The Dataset is related to mushrooms which comprises of 8124 records and 18attributes#
#   The Attributes are pertaining to different characteristics(Size, Shape and color)   #
#   of mushroom such as Cap,  Gill, Stalk, Ring along with some other features          #
#                                                                                       #
#   Business Objective :                                                                #
#                                                                                       #
#   The aim of this project is to build a predictive model and find out whether the     #
#   mushroom is of type P - Poisonous or E - Eatable, given the various other attributes#
#                                                                                       #
#########################################################################################

##########################   2. Project Setup      ######################################
#                                                                                       #
#   This is the initial phase which consists of cleaning up of the history in R Console.#
#   Also includes installing the required packages for this project and loading them    #
#   The dataset will be read and loaded into a dataframe for further analysis           #
#########################################################################################

# The below code cleans the R Global Variables             
rm(list = ls())

# Installing and loading Packages

install.packages("utils")
library(utils)

install.packages("ggplot2")
library(ggplot2)

install.packages("lattice")
library(lattice)

install.packages("gridExtra")
library(gridExtra)

install.packages("ROCR")
library(ROCR)

#For sample.split function i.e., splitting the data
install.packages("caTools")
library(caTools)

#Missing values check
install.packages("Amelia")
library(Amelia)

#For Bayes logistic model
install.packages("arm")
library(arm)

#Decision Tree packages
install.packages("rpart")
library(rpart)

#Decision Tree packages - plot for rpart
install.packages("rpart.plot")
library(rpart.plot)

#Random Forest Package
install.packages("randomForest")
library(randomForest)

#Caret package for k-fold validation
install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)


##########################   3. Data Preprocessing    ###################################
#                                                                                       #
#   After carefully examining the dataset, i came to know that the data is already clean#
#   meaning there are no missing values, no outliers, no derived fields and hence no    #
#   transformation of the variable or attribute is required.                            #
#                                                                                       #
#   But,                                                                                #
#   a. Incomplete data or the attribute having single factor needs to be removed as it  #
#      may cause following error message while modelling.                               #
#         Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) :           #
#         contrasts can be applied only to factors with 2 or more levels                #
#                                                                                       #
#   b. In order to check for the missing values, plotted a graph indicates the observed #
#   and missing values. Found that there were no missing values                         #
#                                                                                       #                                                                                       #
#   c. Since the data is categorical, its good practice to reduce the number of levels  #
#   of multilevel categorical attributes.                                               #
#                                                                                       #
#########################################################################################

#Change the file location to the folder containing the mushroom data
#Reading the file to a dataframe
mushroom <- read.csv("K:/Studies/03 Fall - 16/R Case/mushroom.csv")

#Graphical view of the mushroom dataset
View(mushroom)

#Records and level information
str(mushroom)

#Quick Summary of every attibute
summary(mushroom)

#a. Eliminating the variable(veil type) which has one level
mushroom$veil.type=NULL

#b. Graph of missing and observed values
#   We can see that there are no missing values in the dataset!! Perfect!!
missmap(mushroom, main = "Missing values vs observed")

#c. Eliminating the observations of attributes having least count
#From Summary we can observe that there are some attributes with levels having
#less observations which can be eliminated for better accuracy

summary(mushroom)

#Checking Mushroom cap shape levels
plot(mushroom$cap.shape,col="Blue",main ="Histogram of Mushroom Cap Shape" )

#Checking Mushroom cap surface levels
plot(mushroom$cap.surface,col="Blue",main ="Histogram of Mushroom Cap Surface" )

#Checking Mushroom veil color levels
plot(mushroom$veil.color,col="Blue",main ="Histogram of Mushroom veil color" )

#Conditionally Removing the rows, since this operation deletes 
# 13 rows which is even less than 5%(1.6% to be precise)
mushroom = mushroom[!( (mushroom$cap.shape=="c") | (mushroom$cap.surface=="g") 
                                                 | (mushroom$veil.color=="y") ),]

#Checking the number of records and levels
str(mushroom)


####################   4. Data Visualization - Exploratory Analysis   ###################
#                                                                                       #
#   Here we will see the different graphs and plots in order to explore the realtionship#
#   between the variables                                                               #
#                                                                                       #
#########################################################################################

# Exploring the Mushroom data.

#Histogram of attribute PE shows how the data distribution of mushrooms which are edible and poisonous
#We can see that almost half of the observations are poisonous and the other half are eatable
histogram(mushroom$PE,col="lightblue",xlab = "PE",ylab = "Frequency",main = "Mushroom : Histogram of PE")

#This plot is intended to know whether the color of the cap is affected by bruises
#We can observe from this plot that mushroom with color "b" is affected by bruises
xyplot(mushroom$cap.color~mushroom$bruises,xlab = "Bruises",ylab = "Cap Color",main = "Mushroom : Cap Color vs Bruises")

#Normal plot of Population vs Habitat exhibits different habitats across different population
plot(mushroom$population~mushroom$habitat,xlab = "Habitat",ylab = "Population", main = "Mushroom : Population vs Habitat")

#Trying to find whether the ring number varies across different habitat
#Assuming ring numbers n=none, o=one and t=two, found that mushrooms from habitats "l" and "u" have only one ring number and habitat "w" is having two ring numbers
xyplot(mushroom$ring.number~mushroom$habitat)

#Stalk shape of different population
xyplot(mushroom$population~mushroom$stalk.shape,xlab = "Population",ylab = "Stalk Shape", main = "Mushroom : Stalk shape of Population")

#Mosaic plot of Bruises and PE"
#We can observe that eatable mushrooms have more bruises.In contrast poisonous mushrooms have very less bruises
mp = table(mushroom$PE,mushroom$bruises)
mosaicplot(mp,xlab = "PE",ylab = "Bruises",color = "maroon",main = "Mushroom : PE vs Bruises")


##########################   5. Data Modelling   ########################################
#   Mushroom Dataset was divided into Training and Test datasets to check whether the   #
#   models fitted are more generalised rather than overfitting                          #
#   Several Models like Logistic, Decision Trees, Random Forest and crossfold validation#
#   were tried on the dataset starting from the baseline model.                         #
#   After the dataset is trained and tested,the models were assessed for the accuracy   #
#                                                                                       #
#########################################################################################

#Setting the seed so that the random number genaration reproduces the objects same all the time 
set.seed(50)

#Splitting the dataset with a split ratio of 75% Training and 25% Testing datasets
split = sample.split(mushroom$PE, SplitRatio = 0.75)

#Assigning the subsets after splitting
mushroomTrain = subset(mushroom, split == TRUE)
mushroomTest = subset(mushroom, split == FALSE)

#Checking the rows obtained
nrow(mushroomTrain)
nrow(mushroomTest)

#We can observe that 4208 out 8111 different observations are eatable 
acc = table(mushroom$PE)

#Baseline model - We can see that we have an accuracy of 51.88% from our baseline model

Accuracy = acc[1]/(acc[1]+acc[2])
Accuracy


#Attaching the dataset which helps while specifying the variables in modelling
attach(mushroomTrain)

##########################   A. Logistic Model   ########################################
#                                                                                       #
#    Started with simple logistic model to train and test the dataset and to find the   #
#    accuracy of the model.                                                             #
#    Some problems/warnings showed up as i started with the modelling.                  #
#    1: glm.fit: fitted probabilities numerically 0 or 1 occurred                       # 
#    2: glm.fit: algorithm did not converge                                             #
#                                                                                       #
#    The first warning is due to complete separation or quasi-complete separation       #
#    meaning complete separation happens when the outcome variable separates            # 
#    a predictor variable or a combination of predictor variables completely            #
#                                                                                       #
#    In Order to overcome this issue i preferred Bayesian approach.Some advantages      #
#    are given below.                                                                   #
#       Fit with a pseudo-data approach or modified EM algorithm                        #
#       Equivalent of the glm command in R                                              #
#       Restricted to t-distributions for priors                                        #
#                                                                                       #
#    The Model building approach and accuracy for the training and testing datasets are #
#    provided below                                                                     #
#########################################################################################


#Initially started with the all the attributes and variable selection was made based on the 
#AIC and significance of the attributes

mushroomLog = bayesglm(PE ~ odor+gill.size+stalk.surface.above.ring+spore.print.color+
                         ring.type+gill.spacing,
                       data = mushroomTrain, family = binomial)
summary(mushroomLog)

#Checking the Accuracy of Training model
predictTrain = predict(mushroomLog,type = "response")
summary(predictTrain)
tapply(predictTrain,mushroomTrain$PE,mean)
acc = table(mushroomTrain$PE,predictTrain>0.5)

AccuracyLogTrain = (acc[1]+acc[4])/(acc[1]+acc[2]+acc[3]+acc[4]);AccuracyLogTrain

#Assessing the Train dataset using ROC curve
ROCRpred = prediction(predictTrain, mushroomTrain$PE)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf,colorize= TRUE,print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

#Predicting Test Values
predictTest = predict(mushroomLog, type="response", newdata = mushroomTest)
summary(predictTest)

#Checking the Accuracy of Testing model
tapply(predictTest,mushroomTest$PE,mean)
acc = table(mushroomTest$PE,predictTest>0.5)

AccuracyLogTest = (acc[1]+acc[4])/(acc[1]+acc[2]+acc[3]+acc[4]);AccuracyLogTest

#Assessing the Test dataset using ROC curve
ROCRpred = prediction(predictTest, mushroomTest$PE)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf,colorize= TRUE,print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))


##########################    B.Decision Trees   ########################################
#                                                                                       #
#   Decision Trees are very helpful as it requires very minimal effort for data         #
#   preparation                                                                         #
#                                                                                       #
#   The Model building approach and accuracy for the training and testing datasets are  #
#   provided below                                                                      #
#########################################################################################

#Building Tree Model for Mushroom Train Dataset
#Providing the minbucket prevents the overfitting of the data
mushroomTree = rpart(PE ~ odor+gill.size+stalk.surface.above.ring+spore.print.color+
                       ring.type+gill.spacing,
                     data=mushroomTrain,method="class",minbucket = 25)

prp(mushroomTree)

#predicting test values
predictTree = predict(mushroomTree,newdata = mushroomTest,type = "class")

#Checking Accuracy of Test set
acc=table(mushroomTest$PE,predictTree)

#Accuracy of the test dataset
AccuracyTree = (acc[1]+acc[4])/(acc[1]+acc[2]+acc[3]+acc[4]);AccuracyTree

#Assessing the results using ROC Curve
predictROC = predict(mushroomTree,newdata = mushroomTest)
predictROC

pred = prediction(predictROC[,2],mushroomTest$PE)
perf = performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)


##########################    C.Random Forest    ########################################
#                                                                                       #
#   Random Forest are ensemble learning method for classification, regression and other #
#   models                                                                              #
#                                                                                       #
#   The Model building approach and accuracy for the training and testing datasets are  #
#   provided below                                                                      #
#########################################################################################


#Building Random Forest Model with nodes
mushroomForest = randomForest(PE ~ odor+gill.size+stalk.surface.above.ring+spore.print.color+
                                ring.type+gill.spacing,
                              data=mushroomTrain,nodeize=25,ntree=200)

#predicting test values
predictForest = predict(mushroomForest, newdata = mushroomTest)

#Checking Accuracy
acc = table(mushroomTest$PE,predictForest)

#Accuracy of the test dataset
AccuracyForest = (acc[1]+acc[4])/(acc[1]+acc[2]+acc[3]+acc[4]);AccuracyForest


##########################    D. K-fold Crossvalidation    ##############################
#                                                                                       #
#  Cross validation helps us make sure we are selecting a good parameter value,and      #
#  often this will significantly increase the accuracy.                                 #
#  If we had already happened to select a good parameter value,then the accuracy might  #
#  not of increased that much.                                                          #
#  But by using cross validation, we can be sure that we're selecting a smart parameter #
#  value                                                                                #
#                                                                                       #
#  The Model building approach and accuracy for the training and testing datasets are   #
#  provided below                                                                       #
#########################################################################################


#Setting the number of folds and grid preparation
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))

#output : First Parameter -> cp parameter tested
#Second Parameter -> cross validation accuracy for cp value

train(PE ~ odor+gill.size+stalk.surface.above.ring+spore.print.color+
        ring.type+gill.spacing,data = mushroomTrain, method = "rpart", trControl=numFolds, tuneGrid = cpGrid)

#Building Model
mushroomTreeCV = rpart(PE ~ odor+gill.size+stalk.surface.above.ring+spore.print.color+
                         ring.type+gill.spacing,data = mushroomTrain,method = "class",cp=0.01)
#Predicting Test Values
predictCV = predict(mushroomTreeCV,newdata=mushroomTest,type="class")

acc = table(mushroomTest$PE,predictCV)
AccuracyCross = (acc[1]+acc[4])/(acc[1]+acc[2]+acc[3]+acc[4]);AccuracyCross


##########################    6. Findings            ####################################
#                                                                                       #
#  The only reason why i stopped myself from applying other modelling techniques such as#
#  Gradient boosting and xgboost is that those modelling techniques uses complex        #
#  algorithms and generally used to improve the accuracy.                               #
#  In this case the accuracy is already achieved and even if we apply its futile        #
#                                                                                       #
#  This Section provides the comparison of different models and selection of better     #
#  model and recommendations based on the attributes obtained                           #
#                                                                                       #
#########################################################################################


#########################################################################################
#                                                                                       #
# Comparison of different Models based on accuracy                                      #
#                                                                                       #
# Logistic Regression       - 99.95%                                                    #
#                                                                                       #
# Decision Tree             - 99.6%                                                     #
#                                                                                       #
# Random Forest             - 99.95%                                                    #
#                                                                                       #
# Crossfold Validation      - 99.6%                                                     #
#                                                                                       #
#########################################################################################

#########################################################################################
#                                                                                       #
#   Why Logistic Regression is better ??                                                #
#     Even though the other model results are comparitively similar, Logistic regression#
#     has an edge over other models                                                     #
#       *  Better Accuracy in our model                                                 #  
#       *  Quicker to build                                                             #
#       *  Easy to Interpret                                                            #
#       *  More reilable                                                                #
#                                                                                       #
#   Models are usually trained/scored on different populations and used to make         #
#   predictions and decisions that again are far removed from what they were originally #
#   intended to do. In those situations, I eminently prefer having the ability to       #
#   understand what the model is saying and why, and then make the corrections based on #                                         #
#   real time experience and domain expertise                                           #
#########################################################################################

#########################################################################################
#                                                                                       #
#  Based on model results and some research i came to know how to pick the mushrooms    #
#  that are eatable.                                                                    #
#                                                                                       #
#  Lets look at the findings from our model                                             #
#  a. Odor - The estimate coefficients are against reference category "a".              #
#            We can infer that category "c","f","p" are significantly more likely to be #
#            poisonous compared to category "a".Also the category "n" is significantly  #
#            less likely to be poisonous compared to category "a"                       #
#                                                                                       #
#  b. Spore print color - The estimate coefficients are against reference category "b". #
#                         We can infer that category "r" and "w" are significantly more #
#                         likely to be poisonous compared to category "b"               #
#                                                                                       #
#  c. Stalk Surface above ring - The estimate coefficients are against reference        #
#                         category "f".We can infer that category "k" is significantly  #
#                          more likely to be poisonous compared to category "f"         #                                                                                       #
#                                                                                       #
#  d. Gill Size - The estimate coefficients are against reference category "b".         #
#                 We can infer that category "n" is significantly more likely to be     #
#                 poisonous compared to category "b"                                    #
#                                                                                       #
#  e. Ring Type - The estimate coefficients are against reference category "e".         #
#                 We can infer that category "p" is significantly more likely to be     #
#                 poisonous compared to category "e"                                    #
#                                                                                       #
#  f. Gill Spacing - The estimate coefficients are against reference category "c".      #
#                    We can infer that category "w" is significantly less likely        #
#                    to be poisonous compared to category "c"                           #
#                                                                                       #
#########################################################################################
