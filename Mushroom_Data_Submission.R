#################################### SYKES DATA CHALLENGE ####################################

#Name: Sai Ram Pramod Kotla
#Data analysis and Prediction of Mushroom edibility based on the characteristics of Mushroom##

#Load the dataset Mushroom
mushroom <- read.csv("H:/Sykes Data Challenge/mushroom.csv")

#Packages to install if they are not installed in the RStudio
if(!require(caret))
  install.packages("caret")
require(caret)

if(!require(rpart.plot))
  install.packages("rpart.plot")
require(rpart.plot)

if(!require(Metrics))
  install.packages("Metrics")
require(Metrics)

if(!require(randomForest))
  install.packages("randomForest")
require(randomForest)

if(!require(randomForest))
install.packages("rpart")
require(rpart)

########################################## Data Preparation ##################################

#Analyzed all the variables and found that veil.type has only 1 level "p" (zero variance) 
# so excluded that variable

sapply(mushroom, function(x) length(unique(x)))

data <- mushroom #Copy the mushroom data into dataframe called data
data$veil.type<-NULL #Removed the variable veil.type

########################################## Splitting Data into Test and Train##################

splitData = createDataPartition(y = data$PE,
                                p=0.7,
                                list = FALSE) # Data has been split into 70%-30% for train and test

train <- data[splitData,] #Train Data
test <- data[-splitData,] #Test Data

attach(train) # Attach train for easy access of data

######################################### 5-Fold Cross Validation ############################

#Created a variable to convert the data into 5-folds and validate the results

train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)

############################################# Decision Tree ###################################

treegrid<- expand.grid(cp=seq(0,0.1,0.01)) # Prameter tuning factor - Complexity Parameter

#Run the model decision tree on cross validation data to decide upon the complexity parameter
#"cp" and apply it on the main model.
model_dt<- train(PE~.-PE, data=train,
                 trControl=train_control, 
                 method="rpart", 
                 tuneGrid=treegrid,
                 maxdepth=3) #Run the model on cross validation data
print(model_dt) # Print the model to view the results

#"cp=0" gives the best output
#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was cp = 0.

#Run the final model on train data
final_tree<-rpart(PE~.-PE, data=train,
                  control = rpart.control(cp=0,maxdepth = 3))
#View the decision tree with the following command
prp(final_tree)
#Explanation of Tree: 
#If odor is not a,l,n then it is poisonous
#If spore.pr is not b,h,k,n,o,u,w,y then it is poisonous
#If stalk.co is not e,g,n,o,p,w then it is poisonous

summary(final_tree) # Summary of Final Tree

#Predict the values on train dataset and check the misclassification rate
pred_train_dt  <- predict(final_tree, newdata=train,type = "class") #Prediction command
#Build the confusion matrix
table(pred_train_dt,train$PE)
#pred_train_dt    e    p
#e              2946   18
#p                 0  2724

#Misclassification Rate of the train model
18/5688 # 0.003164557

#Predict the values on test dataset and check the misclassification rate
pred_test_dt  <- predict(final_tree, newdata=test,type = "class")
#Build the confusion matrix
table(pred_test_dt,test$PE)
#pred_test_dt    e    p
#e            1262    6
#p               0 1168

#Misclassification Rate of the test data
6/2436 #0.002463054

##############################################Random Forest##################################

rfgrid<- expand.grid(mtry = seq(15,21,1)) # Controlling mtry: mtry is the number of variables the model considers to predict the output.

#Run the model random forest on cross validation data to decide upon the parameter tunig factor mtry, ntree, nodesize, maxnodes
model_rf<- train(PE~.-PE, data=train, 
                 trControl=train_control, nodesize=500,
                 method="rf", ntree=100, maxnodes=5,tuneGrid=rfgrid,
                 numthreads=4) #Command for building the model

print (model_rf) # Print the results of model
#mtry = 18 gives the best accuracy.

#Execute the final model with above parameters
final_rf <- randomForest(PE~.-PE,data = train,
                         ntree=100,mtry=18, 
                         nodesize=500, maxnodes=5,
                         numthreads=4)
summary(final_rf)      #Gets the detailed description of every split
print(final_rf)       #Prints the model results

varImpPlot(final_rf)  #Gives the variable importance plot
#Odor is the most important variable in this dataset.

#Predict the values on train dataset and check the misclassification rate
pred_train_rf  <- predict(final_rf, newdata=train) # Predict the values using final model
#Build the confusion matrix
table(pred_train_rf,train$PE)
#pred_train_rf    e    p
#e             2946   14
#p                0 2728

#Misclassification Rate
14/5688 #0.002461322

#Predict the values on test dataset and check the misclassification rate
pred_test_rf  <- predict(final_rf, newdata=test)
#Build the confusion matrix
table(pred_test_rf,test$PE)
#pred_test_rf    e    p
#e            1262    6
#p               0 1168

#Misclassification Rate
6/2436 #0.002463054

#Conclusion: Both models seem pretty good but the Misclassification Rate of 
#randaom forest model looks good relatively on train dataset.
#As per the results I would go ahead with the Random Forest model which has 99.99% accuracy.

