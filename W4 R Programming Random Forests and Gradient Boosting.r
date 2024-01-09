##W4 R Programming: Random Forest / Gradient Boosting
#========================Step 1: Read in the Data=====================

#Load library
library( rpart )
library( rpart.plot )
library(ROCR)
library(randomForest)
library(gbm)

#Read the data into R
PATH 		= "/Users/chichi/Desktop/IS_5213/W3/HMEQ"
FILE_NAME 	= "HMEQ_Scrubbed.csv"
INFILE = paste(PATH, FILE_NAME, sep="/")
df = read.csv( INFILE )

#List the structure of the data (str)
str(df)

#Execute a summary of the data
summary(df)

#Print the first six records
head(df)

#===============step 2:Classification Models==================
#Using the code discussed in the lecture, split the data into training and testing data sets.
SEED = 1
set.seed(SEED)

df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

FLAG = sample( c(TRUE, FALSE), nrow(df_flag), replace = TRUE, prob = c(0.7, 0.3) )
df_train = df_flag[FLAG, ]
df_test = df_flag[!FLAG, ]
dim(df_train)
dim(df_test)
dim(df)

#Create a Decision Tree model using the rpart library to predict the variable TARGET_BAD_FLAG
#Plot the Decision Tree and list the important variables for the tree.
tr_set = rpart.control(maxdepth = 10)
tr_model = rpart(data = df_train, TARGET_BAD_FLAG~., control = tr_set, method = "class", parms = list(split = 'information'))
rpart.plot(tr_model)
tr_model$variable.importance

# Create a Random Forest model using the randomForest library to predict the variable TARGET_BAD_FLAG
#List the important variables for the Random Forest and include the variable importance plot.
rf_model = randomForest(data = df_train, TARGET_BAD_FLAG ~ ., ntree = 500, importance = TRUE)
importance(rf_model)
varImpPlot(rf_model)

#Create a Gradient Boosting model using the gbm library to predict the variable TARGET_BAD_FLAG 
gb_model = gbm( data = df_train, TARGET_BAD_FLAG~., n.tree = 500, distribution = "bernoulli")
summary.gbm( gb_model, cBars = 10)


#Using the testing data set, create a ROC curves for all models. They must all be on the same plot
#TREE
pt = predict(tr_model, df_test, type = "prob")
head(pt)
pt2 = prediction( pt[,2], df_test$TARGET_BAD_FLAG)
pt3 = performance(pt2, "tpr", "fpr")
head(pt[, 2])

# random forest
pr = predict(rf_model, df_test)
head(pr)
pr2 = prediction( pr, df_test$TARGET_BAD_FLAG)
pr3 = performance(pr2, "tpr", "fpr")

#gradient boosting
pg = predict(gb_model, df_test, type = "response")
head(pg)
pg2 = prediction( pg, df_test$TARGET_BAD_FLAG)
pg3 = performance(pg2, "tpr", "fpr")

# draw ROC curve
plot( pt3, col="green" )
abline(0,1,lty=2)
legend("bottomright",c("TREE"),col=c("green"), bty="y", lty=1 )

plot( pt3, col="green" )
plot( pr3, col="red", add=TRUE )
plot( pg3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TREE","RANDOM FOREST", "GRADIENT BOOSTING"),col=c("green","red","blue"), bty="y", lty=1 )

#Display the Area Under the ROC curve (AUC) for all models.
aucT = performance( pt2, "auc" )@y.values
aucR = performance( pr2, "auc" )@y.values
aucG = performance( pg2, "auc" )@y.values

print( paste("TREE AUC=", aucT) )
print( paste("RF AUC=", aucR) )
print( paste("GB AUC=", aucG) )

#Rerun with different training and testing data at least three times.
#rerun 1
SEED = 1
set.seed(SEED)

FLAG = sample( c(TRUE, FALSE), nrow(df_flag), replace = TRUE, prob = c(0.7, 0.3) )
df_train = df_flag[FLAG, ]
df_test = df_flag[!FLAG, ]
dim(df_train)

tr_model = rpart(data = df_train, TARGET_BAD_FLAG~., control = tr_set, method = "class", parms = list(split = 'information'))
rf_model = randomForest(data = df_train, TARGET_BAD_FLAG ~ ., ntree = 500, importance = TRUE)
gb_model = gbm( data = df_train, TARGET_BAD_FLAG~., n.tree = 500, distribution = "bernoulli")
pt = predict(tr_model, df_test, type = "prob")
pt2 = prediction( pt[,2], df_test$TARGET_BAD_FLAG)
pt3 = performance(pt2, "tpr", "fpr")

pr = predict(rf_model, df_test)
pr2 = prediction( pr, df_test$TARGET_BAD_FLAG)
pr3 = performance(pr2, "tpr", "fpr")

pg = predict(gb_model, df_test, type = "response")
pg2 = prediction( pg, df_test$TARGET_BAD_FLAG)
pg3 = performance(pg2, "tpr", "fpr")

plot( pt3, col="green" )
abline(0,1,lty=2)
legend("bottomright",c("TREE"),col=c("green"), bty="y", lty=1 )

plot( pt3, col="green" )
plot( pr3, col="red", add=TRUE )
plot( pg3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TREE","RANDOM FOREST", "GRADIENT BOOSTING"),col=c("green","red","blue"), bty="y", lty=1 )

aucT = performance( pt2, "auc" )@y.values
aucR = performance( pr2, "auc" )@y.values
aucG = performance( pg2, "auc" )@y.values

print( paste("TREE AUC=", aucT) )
print( paste("RF AUC=", aucR) )
print( paste("GB AUC=", aucG) )
 
#rerun 2
SEED = 1
set.seed(SEED)

FLAG = sample( c(TRUE, FALSE), nrow(df_flag), replace = TRUE, prob = c(0.65, 0.35) )
df_train = df_flag[FLAG, ]
df_test = df_flag[!FLAG, ]
dim(df_train)

tr_model = rpart(data = df_train, TARGET_BAD_FLAG~., control = tr_set, method = "class", parms = list(split = 'information'))
rf_model = randomForest(data = df_train, TARGET_BAD_FLAG ~ ., ntree = 500, importance = TRUE)
gb_model = gbm( data = df_train, TARGET_BAD_FLAG~., n.tree = 500, distribution = "bernoulli")

pt = predict(tr_model, df_test, type = "prob")
pt2 = prediction( pt[,2], df_test$TARGET_BAD_FLAG)
pt3 = performance(pt2, "tpr", "fpr")

pr = predict(rf_model, df_test)
pr2 = prediction( pr, df_test$TARGET_BAD_FLAG)
pr3 = performance(pr2, "tpr", "fpr")

pg = predict(gb_model, df_test, type = "response")
pg2 = prediction( pg, df_test$TARGET_BAD_FLAG)
pg3 = performance(pg2, "tpr", "fpr")

plot( pt3, col="green" )
abline(0,1,lty=2)
legend("bottomright",c("TREE"),col=c("green"), bty="y", lty=1 )
plot( pt3, col="green" )
plot( pr3, col="red", add=TRUE )
plot( pg3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TREE","RANDOM FOREST", "GRADIENT BOOSTING"),col=c("green","red","blue"), bty="y", lty=1 )

aucT = performance( pt2, "auc" )@y.values
aucR = performance( pr2, "auc" )@y.values
aucG = performance( pg2, "auc" )@y.values

print( paste("TREE AUC=", aucT) )
print( paste("RF AUC=", aucR) )
print( paste("GB AUC=", aucG) )

#rerun 3
SEED = 1
set.seed(SEED)

FLAG = sample( c(TRUE, FALSE), nrow(df_flag), replace = TRUE, prob = c(0.8, 0.2) )
df_train = df_flag[FLAG, ]
df_test = df_flag[!FLAG, ]
dim(df_train)

tr_model = rpart(data = df_train, TARGET_BAD_FLAG~., control = tr_set, method = "class", parms = list(split = 'information'))
rf_model = randomForest(data = df_train, TARGET_BAD_FLAG ~ ., ntree = 500, importance = TRUE)
gb_model = gbm( data = df_train, TARGET_BAD_FLAG~., n.tree = 500, distribution = "bernoulli")

pt = predict(tr_model, df_test, type = "prob")
pt2 = prediction( pt[,2], df_test$TARGET_BAD_FLAG)
pt3 = performance(pt2, "tpr", "fpr")

pr = predict(rf_model, df_test)
pr2 = prediction( pr, df_test$TARGET_BAD_FLAG)
pr3 = performance(pr2, "tpr", "fpr")

pg = predict(gb_model, df_test, type = "response")
pg2 = prediction( pg, df_test$TARGET_BAD_FLAG)
pg3 = performance(pg2, "tpr", "fpr")

plot( pt3, col="green" )
abline(0,1,lty=2)
legend("bottomright",c("TREE"),col=c("green"), bty="y", lty=1 )

plot( pt3, col="green" )
plot( pr3, col="red", add=TRUE )
plot( pg3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TREE","RANDOM FOREST", "GRADIENT BOOSTING"),col=c("green","red","blue"), bty="y", lty=1 )

aucT = performance( pt2, "auc" )@y.values
aucR = performance( pr2, "auc" )@y.values
aucG = performance( pg2, "auc" )@y.values

print( paste("TREE AUC=", aucT) )
print( paste("RF AUC=", aucR) )
print( paste("GB AUC=", aucG) )


 
#===============step3 Regression Decision Tree================
#Using the code discussed in the lecture, split the data into training and testing data sets.
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL

SEED = 1
set.seed(SEED)

FLAG = sample( c(TRUE, FALSE), nrow(df_amt), replace = TRUE, prob = c(0.7, 0.3) )
df_train = df_amt[FLAG, ]
df_test = df_amt[!FLAG, ]
dim(df_train)

#Create a Decision Tree model using the rpart library to predict the variable TARGET_LOSS_AMT
tr_model = rpart(data = df_train, TARGET_LOSS_AMT~., control = tr_set, method = "poisson")
#Plot the Decision Tree and list the important variables for the tree.
rpart.plot(tr_model)
rpart.plot(tr_model, digits = -3, extra = 100)
tr_model$variable.importance
#Using the testing data set, calculate the Root Mean Square Error (RMSE) for the model.
pt = predict(tr_model, df_test)
head(pt)
RMSEt = sqrt( mean( (df_test$TARGET_LOSS_AMT - pt)^2) )
RMSEt

# Create a Random Forest model using the randomForest library to predict the variable TARGET_LOSS_AMT
rf_model = randomForest( data=df_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
#List the important variables for the Random Forest and include the variable importance plot.
importance( rf_model )
varImpPlot( rf_model )
#Using the testing data set, calculate the Root Mean Square Error (RMSE) for the model.
pr = predict( rf_model, df_test )
head( pr )
RMSEr = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pr )^2 ) )
RMSEr

# Create a Gradient Boostingn model using the gbm library to predict the variable TARGET_LOSS_AMT
gb_model = gbm( data=df_train, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="poisson" )
#List the important variables for the Gradient Boosting model and include the variable importance plot.
summary.gbm( gb_model, cBars=10 )
#Using the testing data set, calculate the Root Mean Square Error (RMSE) for the model.
pg = predict( gb_model, df_test, type="response" )
head( pg )
RMSEg = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pg )^2 ) )
RMSEg

#Rerun with different training and testing data at least three times.
# rerun 1
SEED = 1
set.seed(SEED)

FLAG = sample( c(TRUE, FALSE), nrow(df_amt), replace = TRUE, prob = c(0.7, 0.3) )
df_train = df_amt[FLAG, ]
df_test = df_amt[!FLAG, ]
dim(df_train)

#Create a Decision Tree model using the rpart library to predict the variable TARGET_LOSS_AMT
tr_model = rpart(data = df_train, TARGET_LOSS_AMT~., control = tr_set, method = "poisson")
#Plot the Decision Tree and list the important variables for the tree.
rpart.plot(tr_model)
rpart.plot(tr_model, digits = -3, extra = 100)
tr_model$variable.importance
#Using the testing data set, calculate the Root Mean Square Error (RMSE) for the model.
pt = predict(tr_model, df_test)
head(pt)
RMSEt = sqrt( mean( (df_test$TARGET_LOSS_AMT - pt)^2) )
RMSEt

# Create a Random Forest model using the randomForest library to predict the variable TARGET_LOSS_AMT
rf_model = randomForest( data=df_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
#List the important variables for the Random Forest and include the variable importance plot.
importance( rf_model )
varImpPlot( rf_model )
#Using the testing data set, calculate the Root Mean Square Error (RMSE) for the model.
pr = predict( rf_model, df_test )
head( pr )
RMSEr = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pr )^2 ) )
RMSEr

# Create a Gradient Boostingn model using the gbm library to predict the variable TARGET_LOSS_AMT
gb_model = gbm( data=df_train, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="poisson" )
#List the important variables for the Gradient Boosting model and include the variable importance plot.
summary.gbm( gb_model, cBars=10 )
#Using the testing data set, calculate the Root Mean Square Error (RMSE) for the model.
pg = predict( gb_model, df_test, type="response" )
head( pg )
RMSEg = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pg )^2 ) )
RMSEg

# rerun 2
SEED = 1
set.seed(SEED)

FLAG = sample( c(TRUE, FALSE), nrow(df_amt), replace = TRUE, prob = c(0.75, 0.25) )
df_train = df_amt[FLAG, ]
df_test = df_amt[!FLAG, ]
dim(df_train)

#Create a Decision Tree model using the rpart library to predict the variable TARGET_LOSS_AMT
tr_model = rpart(data = df_train, TARGET_LOSS_AMT~., control = tr_set, method = "poisson")
#Plot the Decision Tree and list the important variables for the tree.
rpart.plot(tr_model)
rpart.plot(tr_model, digits = -3, extra = 100)
tr_model$variable.importance
#Using the testing data set, calculate the Root Mean Square Error (RMSE) for the model.
pt = predict(tr_model, df_test)
head(pt)
RMSEt = sqrt( mean( (df_test$TARGET_LOSS_AMT - pt)^2) )
RMSEt

# Create a Random Forest model using the randomForest library to predict the variable TARGET_LOSS_AMT
rf_model = randomForest( data=df_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
#List the important variables for the Random Forest and include the variable importance plot.
importance( rf_model )
varImpPlot( rf_model )
#Using the testing data set, calculate the Root Mean Square Error (RMSE) for the model.
pr = predict( rf_model, df_test )
head( pr )
RMSEr = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pr )^2 ) )
RMSEr

# Create a Gradient Boostingn model using the gbm library to predict the variable TARGET_LOSS_AMT
gb_model = gbm( data=df_train, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="poisson" )
#List the important variables for the Gradient Boosting model and include the variable importance plot.
summary.gbm( gb_model, cBars=10 )
#Using the testing data set, calculate the Root Mean Square Error (RMSE) for the model.
pg = predict( gb_model, df_test, type="response" )
head( pg )
RMSEg = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pg )^2 ) )
RMSEg

# rerun 3
SEED = 1
set.seed(SEED)

FLAG = sample( c(TRUE, FALSE), nrow(df_amt), replace = TRUE, prob = c(0.8, 0.2) )
df_train = df_amt[FLAG, ]
df_test = df_amt[!FLAG, ]
dim(df_train)

#Create a Decision Tree model using the rpart library to predict the variable TARGET_LOSS_AMT
tr_model = rpart(data = df_train, TARGET_LOSS_AMT~., control = tr_set, method = "poisson")
#Plot the Decision Tree and list the important variables for the tree.
rpart.plot(tr_model)
rpart.plot(tr_model, digits = -3, extra = 100)
tr_model$variable.importance
#Using the testing data set, calculate the Root Mean Square Error (RMSE) for the model.
pt = predict(tr_model, df_test)
head(pt)
RMSEt = sqrt( mean( (df_test$TARGET_LOSS_AMT - pt)^2) )
RMSEt

# Create a Random Forest model using the randomForest library to predict the variable TARGET_LOSS_AMT
rf_model = randomForest( data=df_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
#List the important variables for the Random Forest and include the variable importance plot.
importance( rf_model )
varImpPlot( rf_model )
#Using the testing data set, calculate the Root Mean Square Error (RMSE) for the model.
pr = predict( rf_model, df_test )
head( pr )
RMSEr = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pr )^2 ) )
RMSEr

# Create a Gradient Boostingn model using the gbm library to predict the variable TARGET_LOSS_AMT
gb_model = gbm( data=df_train, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="poisson" )
#List the important variables for the Gradient Boosting model and include the variable importance plot.
summary.gbm( gb_model, cBars=10 )
#Using the testing data set, calculate the Root Mean Square Error (RMSE) for the model.
pg = predict( gb_model, df_test, type="response" )
head( pg )
RMSEg = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pg )^2 ) )
RMSEg

#===============step4:Probability / Severity Model Decision Tree==================
#Using the code discussed in the lecture, split the data into training and testing data sets.
df_4 = df

SEED = 1
set.seed(SEED)

FLAG = sample( c(TRUE, FALSE), nrow(df_4), replace = TRUE, prob = c(0.7, 0.3) )
df_train = df_4[FLAG, ]
df_test = df_4[!FLAG, ]
dim(df_train)

#Use any model from Step 2 in order to predict the variable TARGET_BAD_FLAG
df_4_flag_train = df_train
df_4_flag_train$TARGET_LOSS_AMT = NULL
rf_model = randomForest(data = df_4_flag_train, TARGET_BAD_FLAG ~ ., ntree = 500, importance = TRUE)

#Develop three models: Decision Tree, Random Forest, and Gradient Boosting to predict the variable TARGET_LOSS_AMT using only records where TARGET_BAD_FLAG is 1.
df_4_amt_train = subset( df_train, TARGET_BAD_FLAG == 1)
df_4_amt_train$TARGET_BAD_FLAG = NULL
tr_model = rpart( data=df_4_amt_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rf_model = randomForest( data=df_4_amt_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
gb_model = gbm( data=df_4_amt_train, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="poisson" )

#Select one of the models to predict damage.
df_4_amt_test = df_test
df_4_amt_test$TARGET_BAD_FLAG = NULL
pr = predict( rf_model, df_4_amt_test )

#List the important variables for both models.
tr_model$variable.importance
importance( rf_model )
summary.gbm( gb_model, cBars=10 )

#Using your models, predict the probability of default and the loss given default.
rf_model_flag = randomForest(data = df_4_flag_train, TARGET_BAD_FLAG ~ ., ntree = 500, importance = TRUE)
df_4_flag_test = df_test
df_4_flag_test$TARGET_LOSS_AMT = NULL
pr_flag = predict(rf_model_flag, df_4_flag_test)

#Multiply the two values together for each record.
p = pr_flag * pr
head(p)

#Calculate the RMSE value for the Probability / Severity model.
RMSE = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p ) ^2 ) )
print(RMSE)

#Rerun at least three times to be assured that the model is optimal and not over fit or under fit.

#rerun 1======================
df_4 = df
SEED = 1
set.seed(SEED)

FLAG = sample( c(TRUE, FALSE), nrow(df_4), replace = TRUE, prob = c(0.7, 0.3) )
df_train = df_4[FLAG, ]
df_test = df_4[!FLAG, ]

df_4_flag_train = df_train
df_4_flag_train$TARGET_LOSS_AMT = NULL
df_4_flag_test = df_test
df_4_flag_test$TARGET_LOSS_AMT = NULL
rf_model_flag = randomForest(data = df_4_flag_train, TARGET_BAD_FLAG ~ ., ntree = 500, importance = TRUE)

df_4_amt_train = subset( df_train, TARGET_BAD_FLAG == 1)
df_4_amt_train$TARGET_BAD_FLAG = NULL
rf_model = randomForest( data=df_4_amt_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
pr_train_flag = predict(rf_model_flag, df_train)
pr_train_amt = predict( rf_model, df_train )
pr_train = pr_train_flag * pr_train_amt
RMSEtrain = sqrt( mean( ( df_train$TARGET_LOSS_AMT - pr_train ) ^2 ) )
print(RMSEtrain)

pr_test_flag = predict(rf_model_flag, df_test)
pr_test_amt = predict( rf_model, df_test )
pr_test = pr_test_flag * pr_test_amt
RMSEtest = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pr_test ) ^2 ) )
print(RMSEtest)

# rerun2==============================
df_4 = df
SEED = 1
set.seed(SEED)

FLAG = sample( c(TRUE, FALSE), nrow(df_4), replace = TRUE, prob = c(0.7, 0.3) )
df_train = df_4[FLAG, ]
df_test = df_4[!FLAG, ]

df_4_flag_train = df_train
df_4_flag_train$TARGET_LOSS_AMT = NULL
df_4_flag_test = df_test
df_4_flag_test$TARGET_LOSS_AMT = NULL
rf_model_flag = randomForest(data = df_4_flag_train, TARGET_BAD_FLAG ~ ., ntree = 500, importance = TRUE)

df_4_amt_train = subset( df_train, TARGET_BAD_FLAG == 1)
df_4_amt_train$TARGET_BAD_FLAG = NULL
rf_model = randomForest( data=df_4_amt_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
pr_train_flag = predict(rf_model_flag, df_train)
pr_train_amt = predict( rf_model, df_train )
pr_train = pr_train_flag * pr_train_amt
RMSEtrain = sqrt( mean( ( df_train$TARGET_LOSS_AMT - pr_train ) ^2 ) )
print(RMSEtrain)

pr_test_flag = predict(rf_model_flag, df_test)
pr_test_amt = predict( rf_model, df_test )
pr_test = pr_test_flag * pr_test_amt
RMSEtest = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pr_test ) ^2 ) )
print(RMSEtest)

# rerun 3============================
df_4 = df
SEED = 1
set.seed(SEED)

FLAG = sample( c(TRUE, FALSE), nrow(df_4), replace = TRUE, prob = c(0.75, 0.25) )
df_train = df_4[FLAG, ]
df_test = df_4[!FLAG, ]

df_4_flag_train = df_train
df_4_flag_train$TARGET_LOSS_AMT = NULL
df_4_flag_test = df_test
df_4_flag_test$TARGET_LOSS_AMT = NULL
rf_model_flag = randomForest(data = df_4_flag_train, TARGET_BAD_FLAG ~ ., ntree = 500, importance = TRUE)

df_4_amt_train = subset( df_train, TARGET_BAD_FLAG == 1)
df_4_amt_train$TARGET_BAD_FLAG = NULL
rf_model = randomForest( data=df_4_amt_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
pr_train_flag = predict(rf_model_flag, df_train)
pr_train_amt = predict( rf_model, df_train )
pr_train = pr_train_flag * pr_train_amt
RMSEtrain = sqrt( mean( ( df_train$TARGET_LOSS_AMT - pr_train ) ^2 ) )
print(RMSEtrain)

pr_test_flag = predict(rf_model_flag, df_test)
pr_test_amt = predict( rf_model, df_test )
pr_test = pr_test_flag * pr_test_amt
RMSEtest = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pr_test ) ^2 ) )
print(RMSEtest)
