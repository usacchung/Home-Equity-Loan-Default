#W6 R Programming: Logistic/Linear Regression

#========================Step 1: Use the Decision Tree / Random Forest / Decision Tree code 
#from Week 5 as a Starting Point=====================
#In this assignment, we will build off the models developed in Week 5. Now we will add Regression to the models.
#Load library
library( rpart )
library( rpart.plot )
library(ROCR)
library(randomForest)
library(gbm)
library( MASS )

#Read the data into R
PATH 		= "/Users/chichi/Desktop/IS_5213/W3/HMEQ"
FILE_NAME 	= "HMEQ_Scrubbed.csv"
INFILE = paste(PATH, FILE_NAME, sep="/")
df = read.csv( INFILE )

SEED = 1
set.seed(SEED)

df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

FLAG = sample( c(TRUE, FALSE), nrow(df_flag), replace = TRUE, prob = c(0.7, 0.3) )
df_train = df_flag[FLAG, ]
df_test = df_flag[!FLAG, ]

tr_set = rpart.control(maxdepth = 10)
tr_model = rpart(data = df_train, TARGET_BAD_FLAG~., control = tr_set, method = "class", parms = list(split = 'information'))

rf_model = randomForest(data = df_train, TARGET_BAD_FLAG ~ ., ntree = 500, importance = TRUE)

gb_model = gbm( data = df_train, TARGET_BAD_FLAG~., n.tree = 500, distribution = "bernoulli")

#========================Step 2: Classification Models=====================
#Using the code discussed in the lecture, split the data into training and testing data sets.
#Create a LOGISTIC REGRESSION model using ALL the variables to predict the variable TARGET_BAD_FLAG
theUpper_LR = glm( TARGET_BAD_FLAG ~ ., family = "binomial", data=df_train )
summary(theUpper_LR)

#Create a LOGISTIC REGRESSION model and using BACKWARD VARIABLE SELECTION.
theLower_LR = glm( TARGET_BAD_FLAG ~ 1, family = "binomial", data=df_train )
lr_model = stepAIC(theUpper_LR, direction="backward", scope=list(lower=theLower_LR, upper=theUpper_LR))
summary( lr_model )

#Create a LOGISTIC REGRESSION model and using a DECISION TREE and FORWARD STEPWISE SELECTION.
tr_set = rpart.control(maxdepth = 10)
tr_model = rpart(data = df_train, TARGET_BAD_FLAG~., control = tr_set, method = "class", parms = list(split = 'information'))
treeVars = tr_model$variable.importance
treeVars = names(treeVars)
treeVarsPlus = paste( treeVars, collapse="+")
F = as.formula( paste( "TARGET_BAD_FLAG ~", treeVarsPlus ))
tree_LR = glm( F, family = "binomial", data=df_train )
theLower_LR = glm( TARGET_BAD_FLAG ~ 1, family = "binomial", data=df_train )
lrt_model = stepAIC(tree_LR, direction="both", scope=list(lower=theLower_LR, upper=tree_LR))

#List the important variables from the Logistic Regression Variable Selections.
summary( lrt_model )

#Using the testing data set, create a ROC curves for all models. They must all be on the same plot.
# plot linear regression model
plr = predict( lr_model, df_test, type="response" )
plr2 = prediction( plr, df_test$TARGET_BAD_FLAG )
plr3 = performance( plr2, "tpr", "fpr" )

#plot tree model
pt = predict( tr_model, df_test, type="prob" )
pt2 = prediction( pt[,2], df_test$TARGET_BAD_FLAG )
pt3 = performance( pt2, "tpr", "fpr" )

#plot RF model
rf_model = randomForest( data=df_train, TARGET_BAD_FLAG ~ ., ntree=500, importance=TRUE )
pr = predict( rf_model, df_test )
pr2 = prediction( pr, df_test$TARGET_BAD_FLAG )
pr3 = performance( pr2, "tpr", "fpr" )

# plot GRADIENT BOOSTING model
gb_model = gbm( data=df_train, TARGET_BAD_FLAG ~ ., n.trees=500, distribution="bernoulli" )
pg = predict( gb_model, df_test, type="response" )
pg2 = prediction( pg, df_test$TARGET_BAD_FLAG )
pg3 = performance( pg2, "tpr", "fpr" )

# plot LR model
plrt = predict( lrt_model, df_test, type="response" )
plrt2 = prediction( plrt, df_test$TARGET_BAD_FLAG )
plrt3 = performance( plrt2, "tpr", "fpr" )

plot( pt3, col="green" )
plot( pr3, col="red", add=TRUE )
plot( pg3, col="blue", add=TRUE )
plot( plr3, col="gold", add=TRUE ) 
plot( plrt3, col="gray", add=TRUE ) 
abline(0,1,lty=2)
legend("bottomright",c("TREE","RANDOM FOREST", "GRADIENT BOOSTING", "LOGIT REG FWD", "LOGIT REG TREE"),col=c("green","red","blue","gold","gray"), bty="y", lty=1 )

aucT = performance( pt2, "auc" )@y.values
aucR = performance( pr2, "auc" )@y.values
aucG = performance( pg2, "auc" )@y.values
aucLR = performance( plr2, "auc")@y.values
aucLRT = performance( plrt2, "auc")@y.values
 
print( paste("TREE AUC=", aucT) )
print( paste("RF AUC=", aucR) )
print( paste("GB AUC=", aucG) )
print( paste("LR AUC=", aucLR) )
print( paste("LRT AUC=", aucLRT) )

#============STEP 3: Linear Regression===============
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL
FLAG = sample( c( TRUE, FALSE ), nrow(df_amt), replace=TRUE, prob=c(0.7,0.3))
df_train = df_amt[FLAG, ]
df_test = df_amt[! FLAG, ]

# Create a LINEAR REGRESSION model using ALL the variables to predict the variable TARGET_BAD_AMT
#Create a LINEAR REGRESSION model and using BACKWARD VARIABLE SELECTION.
theUpper_LR = lm( TARGET_LOSS_AMT ~ ., data=df_train )
theLower_LR = lm( TARGET_LOSS_AMT ~ 1, data=df_train )
lr_model = stepAIC(theUpper_LR, direction="backward", scope=list(lower=theLower_LR, upper=theUpper_LR))

#Create a LINEAR REGRESSION model and using a DECISION TREE and FORWARD STEPWISE SELECTION.
tr_set = rpart.control( maxdepth = 10 )
tr_model = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
treeVars = tr_model$variable.importance
treeVars = names(treeVars)
treeVarsPlus = paste( treeVars, collapse="+")
F = as.formula( paste( "TARGET_LOSS_AMT ~", treeVarsPlus ))

tree_LR = lm( F, data=df_train )
theLower_LR = lm( TARGET_LOSS_AMT ~ 1, data=df_train )
lrt_model = stepAIC(theLower_LR, direction="both", scope=list(lower=theLower_LR, upper=tree_LR))


#List the important variables from the Linear Regression Variable Selections.
summary( lrt_model )

#Compare the variables from the Linear Regression with those of the Random Forest and the Gradient Boosting.
#LR BACK model
plr = predict( lr_model, df_test )
head( plr )
RMSElr = sqrt( mean( ( df_test$TARGET_LOSS_AMT - plr )^2 ) )

#LR TREE model
plr_tree = predict( tree_LR, df_test )
head( plr_tree )
RMSElr_tree = sqrt( mean( ( df_test$TARGET_LOSS_AMT - plr_tree )^2 ) )

#LR TREE STEP model
plr_tree_step = predict( lrt_model, df_test )
head( plr_tree_step )
RMSElr_tree_step = sqrt( mean( ( df_test$TARGET_LOSS_AMT - plr_tree_step )^2 ) )

#tree model
pt = predict( tr_model, df_test )
RMSEt = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pt )^2 ) )

# RF model
rf_model = randomForest( data=df_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
pr = predict( rf_model, df_test )
RMSEr = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pr )^2 ) )

# GRADIENT BOOSTING model
gb_model = gbm( data=df_train, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="poisson" )
pg = predict( gb_model, df_test, type="response" )
RMSEg = sqrt( mean( ( df_test$TARGET_LOSS_AMT - pg )^2 ) )

print( paste("TREE RMSE=", RMSEt ))
print( paste("RF RMSE=", RMSEr ))
print( paste("GB RMSE=", RMSEg ))

print( paste("LR BACK RMSE=",  RMSElr ))
print( paste("LR TREE RMSE=",  RMSElr_tree ))
print( paste("LR TREE STEP RMSE=", RMSElr_tree_step ))

#================step4: Probability / Severity Model=====================
#Using the code discussed in the lecture, split the data into training and testing data sets.
df_4 = df
SEED = 1
set.seed(SEED)
FLAG = sample( c(TRUE, FALSE), nrow(df_4), replace = TRUE, prob = c(0.7, 0.3) )
df_train = df_4[FLAG, ]
df_test = df_4[!FLAG, ]

#Use any LOGISTIC model from Step 2 in order to predict the variable TARGET_BAD_FLAG
df_4_flag_train = df_train
df_4_flag_train$TARGET_LOSS_AMT = NULL
theUpper_LR = glm( TARGET_BAD_FLAG ~ ., family = "binomial", data=df_4_flag_train )
theLower_LR = glm( TARGET_BAD_FLAG ~ 1, family = "binomial", data=df_4_flag_train )
log_model = stepAIC(theUpper_LR, direction="backward", scope=list(lower=theLower_LR, upper=theUpper_LR))
plr_flag = predict(log_model, df_test, type = "response")
head(plr_flag)

#Use a LINEAR REGRESSION model to predict the variable TARGET_LOSS_AMT using only records where TARGET_BAD_FLAG is 1.
df_4_amt_train = subset( df_train, TARGET_BAD_FLAG == 1)
df_4_amt_train$TARGET_BAD_FLAG = NULL
theUpper_LR = lm( TARGET_LOSS_AMT ~ ., data=df_4_amt_train )
theLower_LR = lm( TARGET_LOSS_AMT ~ 1, data=df_4_amt_train )
lr_model = stepAIC(theUpper_LR, direction="backward", scope=list(lower=theLower_LR, upper=theUpper_LR))

head(plr)
plr_amount = predict(lr_model, df_test)
head(plr_amount)

#List the important variables for both models. 
summary(log_model)
summary(lr_model)

#Using your models, predict the probability of default and the loss given default.
plr_flag = predict(log_model, df_test, type = "response")
head(plr_flag)
plr_amount = predict(lr_model, df_test)
head(plr_amount)

#Multiply the two values together for each record.
p = plr_flag * plr_amount

#Calculate the RMSE value for the Probability / Severity model.
RMSE = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p ) ^2 ) )
print(RMSE)
