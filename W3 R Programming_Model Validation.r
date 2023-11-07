#W4 R Programming: Model Validation

#========================Step 1: Read in the Data=====================

#Load library
library( rpart )
library( rpart.plot )
library(ROCR)

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

#========================Step 2: Classification Decision Tree=====================

#Using the code discussed in the lecture, split the data into training and testing data sets.
SEED = 1
set.seed( SEED )

df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

FLAG = sample( c( TRUE, FALSE ), nrow(df_flag), replace=TRUE, prob=c(0.8,0.2))
df_train = df_flag[FLAG, ]
df_test = df_flag[! FLAG, ]

dim( df_flag )
dim( df_train )
dim( df_test )

#Use the rpart library to predict the variable TARGET_BAD_FLAG
#Develop two decision trees, one using Gini and the other using Entropy using the training and testing data
#All other parameters such as tree depth are up to you.
#Plot both decision trees
tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )
rpart.plot( t1G )
rpart.plot( t1E )

#List the important variables for both trees
t1G$variable.importance
t1E$variable.importance

#Using the training data set, create a ROC curve for both trees
pG = predict( t1G, df_train )
pG2 = prediction( pG[,2], df_train$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_train )
pE2 = prediction( pE[,2], df_train$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TRAIN GINI","TRAIN ENTROPY"),col=c("red","green"), bty="y", lty=1 )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TRAIN AUC GINI=", aucG) )
print( paste("TRAIN AUC ENTROPY=", aucE) )
fG = predict( t1G, df_train, type="class" )
fE = predict( t1E, df_train, type="class" )

table( fG, df_train$TARGET_BAD_FLAG )
table( fE, df_train$TARGET_BAD_FLAG )

#Using the testing data set, create a ROC curve for both trees
pG = predict( t1G, df_test )
pG2 = prediction( pG[,2], df_test$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df_test )
pE2 = prediction( pE[,2], df_test$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( pE3, col="green", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TEST GINI","TEST ENTROPY"),col=c("red","green"), bty="y", lty=1 )
print( paste("TEST AUC GINI=", aucG) )
print( paste("TEST AUC ENTROPY=", aucE) )

#Rerun with different training and testing data at least three times.
#Rerun 1===========
FLAG = sample( c( TRUE, FALSE ), nrow(df_flag), replace=TRUE, prob=c(0.7,0.3))
df_train = df_flag[FLAG, ]
df_test = df_flag[! FLAG, ]

tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )
 
# Rerun 1 Train data
pG = predict( t1G, df_train )
pG2 = prediction( pG[,2], df_train$TARGET_BAD_FLAG )

pE = predict( t1E, df_train )
pE2 = prediction( pE[,2], df_train$TARGET_BAD_FLAG )
aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TRAIN AUC GINI=", aucG) )
print( paste("TRAIN AUC ENTROPY=", aucE) )


# Rerun 1 Test data
pG = predict( t1G, df_test )
pG2 = prediction( pG[,2], df_test$TARGET_BAD_FLAG )

pE = predict( t1E, df_test )
pE2 = prediction( pE[,2], df_test$TARGET_BAD_FLAG )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values
print( paste("TEST AUC GINI=", aucG) )
print( paste("TEST AUC ENTROPY=", aucE) )

#Rerun 2===========
FLAG = sample( c( TRUE, FALSE ), nrow(df_flag), replace=TRUE, prob=c(0.6,0.4))
df_train = df_flag[FLAG, ]
df_test = df_flag[! FLAG, ]

tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )

# Rerun 2 Train data
pG = predict( t1G, df_train )
pG2 = prediction( pG[,2], df_train$TARGET_BAD_FLAG )

pE = predict( t1E, df_train )
pE2 = prediction( pE[,2], df_train$TARGET_BAD_FLAG )
aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TRAIN AUC GINI=", aucG) )
print( paste("TRAIN AUC ENTROPY=", aucE) )
 
# Rerun 2 Test data
pG = predict( t1G, df_test )
pG2 = prediction( pG[,2], df_test$TARGET_BAD_FLAG )

pE = predict( t1E, df_test )
pE2 = prediction( pE[,2], df_test$TARGET_BAD_FLAG )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TEST AUC GINI=", aucG) )
print( paste("TEST AUC ENTROPY=", aucE) )

#Rerun 3=========== 
FLAG = sample( c( TRUE, FALSE ), nrow(df_flag), replace=TRUE, prob=c(0.85,0.15))
df_train = df_flag[FLAG, ]
df_test = df_flag[! FLAG, ]

tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )

# Rerun 3 Train data 
pG = predict( t1G, df_train )
pG2 = prediction( pG[,2], df_train$TARGET_BAD_FLAG )

pE = predict( t1E, df_train )
pE2 = prediction( pE[,2], df_train$TARGET_BAD_FLAG )
aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TRAIN AUC GINI=", aucG) )
print( paste("TRAIN AUC ENTROPY=", aucE) )

# Rerun 3 Test data  
pG = predict( t1G, df_test )
pG2 = prediction( pG[,2], df_test$TARGET_BAD_FLAG )

pE = predict( t1E, df_test )
pE2 = prediction( pE[,2], df_test$TARGET_BAD_FLAG )

aucG = performance( pG2, "auc" )@y.values
aucE = performance( pE2, "auc" )@y.values

print( paste("TEST AUC GINI=", aucG) )
print( paste("TEST AUC ENTROPY=", aucE) )


#========================Step 3: Regression Decision Tree=====================
#Using the code discussed in the lecture, split the data into training and testing data sets.
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL

FLAG = sample( c( TRUE, FALSE ), nrow(df_amt), replace=TRUE, prob=c(0.7,0.3))
df_train = df_amt[FLAG, ]
df_test = df_amt[! FLAG, ]

mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )

#Use the rpart library to predict the variable TARGET_LOSS_AMT
#Develop two decision trees, one using anova and the other using poisson
#All other parameters such as tree depth are up to you.
#Plot both decision trees
tr_set = rpart.control( maxdepth = 5 )
t1a = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
rpart.plot( t1a )
t1p = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( t1p )

#List the important variables for both trees
t1a$variable.importance
t1p$variable.importance

#Using the training data set, calculate the Root Mean Square Error (RMSE) for both trees
p1a = predict( t1a, df_train )
RMSE1a = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1a )^2 ) )
p1p = predict( t1p, df_train )
RMSE1p = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1p )^2 ) )
print( paste("TRAIN RMSE ANOVA =", RMSE1a) )
print( paste("TRAIN RMSE POISSON =", RMSE1p) )

#Using the testing data set, calculate the Root Mean Square Error (RMSE) for both trees
p1a = predict( t1a, df_test )
RMSE1a = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1a )^2 ) )
p1p = predict( t1p, df_test )
RMSE1p = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1p )^2 ) )
print( paste("TEST RMSE ANOVA =", RMSE1a) )
print( paste("TEST RMSE POISSON =", RMSE1p) )

#Rerun with different training and testing data at least three times.
# Rerun 1
FLAG = sample( c( TRUE, FALSE ), nrow(df_amt), replace=TRUE, prob=c(0.8,0.2))
df_train = df_amt[FLAG, ]
df_test = df_amt[! FLAG, ]
mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )

tr_set = rpart.control( maxdepth = 5 )

t1a = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
t1p = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )

# Rerun 1 Train data
p1a = predict( t1a, df_train )
RMSE1a = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1a )^2 ) )
p1p = predict( t1p, df_train )
RMSE1p = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1p )^2 ) )
print( paste("TRAIN RMSE ANOVA =", RMSE1a) )
print( paste("TRAIN RMSE POISSON =", RMSE1p) )
 
# Rerun 1 Test data
p1a = predict( t1a, df_test )
RMSE1a = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1a )^2 ) )
p1p = predict( t1p, df_test )
RMSE1p = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1p )^2 ) )
print( paste("TEST RMSE ANOVA =", RMSE1a) )
print( paste("TEST RMSE POISSON =", RMSE1p) )

# Rerun 2====================
FLAG = sample( c( TRUE, FALSE ), nrow(df_amt), replace=TRUE, prob=c(0.85,0.15))
df_train = df_amt[FLAG, ]
df_test = df_amt[! FLAG, ]
 
mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )

tr_set = rpart.control( maxdepth = 5 )
t1a = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
t1p = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )

# Rerun 2 Train data
p1a = predict( t1a, df_train )
RMSE1a = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1a )^2 ) )

p1p = predict( t1p, df_train )
RMSE1p = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1p )^2 ) )
print( paste("TRAIN RMSE ANOVA =", RMSE1a) )

print( paste("TRAIN RMSE POISSON =", RMSE1p) )

# Rerun 2 Test data
p1a = predict( t1a, df_test )
RMSE1a = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1a )^2 ) )
p1p = predict( t1p, df_test )
RMSE1p = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1p )^2 ) )
print( paste("TEST RMSE ANOVA =", RMSE1a) )
print( paste("TEST RMSE POISSON =", RMSE1p) )

# Rerun 3=============
FLAG = sample( c( TRUE, FALSE ), nrow(df_amt), replace=TRUE, prob=c(0.65,0.35))
df_train = df_amt[FLAG, ]
df_test = df_amt[! FLAG, ]
 
mean( df_amt$TARGET_LOSS_AMT )
mean( df_train$TARGET_LOSS_AMT )
mean( df_test$TARGET_LOSS_AMT )
 
tr_set = rpart.control( maxdepth = 5 )

t1a = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="anova" )
t1p = rpart( data=df_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
 
# Rerun 3 Train data
p1a = predict( t1a, df_train )
RMSE1a = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1a )^2 ) )
p1p = predict( t1p, df_train )
RMSE1p = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p1p )^2 ) )
print( paste("TRAIN RMSE ANOVA =", RMSE1a) )
print( paste("TRAIN RMSE POISSON =", RMSE1p) )

# Rerun 3 Test data
p1a = predict( t1a, df_test )
RMSE1a = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1a )^2 ) )
p1p = predict( t1p, df_test )
RMSE1p = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p1p )^2 ) )
print( paste("TEST RMSE ANOVA =", RMSE1a) )
print( paste("TEST RMSE POISSON =", RMSE1p) )

#========================Step 4: Probability / Severity Model Decision Tree=====================
#Using the code discussed in the lecture, split the data into training and testing data sets.
df_4 = df
FLAG = sample( c( TRUE, FALSE ), nrow(df_4), replace=TRUE, prob=c(0.7,0.3))
df_train = df_4[FLAG, ]
df_test = df_4[! FLAG, ]

#Use the rpart library to predict the variable TARGET_BAD_FLAG
df_4_flag = df_train
df_4_flag$TARGET_LOSS_AMT = NULL
tr_set = rpart.control( maxdepth = 3 )
t2_f = rpart(data = df_4_flag, TARGET_BAD_FLAG ~ ., control=tr_set)
rpart.plot( t2_f )

#Use the rpart library to predict the variable TARGET_LOSS_AMT using only records where TARGET_BAD_FLAG is 1.
#Plot both decision trees
df_4_amt = subset( df_train, TARGET_BAD_FLAG == 1)
df_4_amt$TARGET_BAD_FLAG = NULL
tr_set = rpart.control( maxdepth = 2 )
t2_a = rpart(data = df_4_amt, TARGET_LOSS_AMT ~ ., control=tr_set)
rpart.plot( t2_a )

#List the important variables for both trees
t2_f$variable.importance
t2_a$variable.importance

#Using your models, predict the probability of default and the loss given default.
p2_f = predict( t2_f, df_train)
p2_a = predict(t2_a, df_train)
head(p2_f)
head(p2_a)

#Multiply the two values together for each record.
p2 = p2_f * p2_a
head(p2)

#Calculate the RMSE value for the Probability / Severity model.
RMSE2 = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p2 ) ^2 ) )
print(RMSE2)

#Test for step 4
df_4_flag = df_test
df_4_flag$TARGET_LOSS_AMT = NULL
tr_set = rpart.control( maxdepth = 3 )
t2_f = rpart(data = df_4_flag, TARGET_BAD_FLAG ~ ., control=tr_set)

df_4_amt = subset( df_test, TARGET_BAD_FLAG == 1)
df_4_amt$TARGET_BAD_FLAG = NULL
tr_set = rpart.control( maxdepth = 2 )
t2_a = rpart(data = df_4_amt, TARGET_LOSS_AMT ~ ., control=tr_set)
p2_f = predict( t2_f, df_test)
p2_a = predict(t2_a, df_test)
head(p2_f)
head(p2_a)

p2 = p2_f * p2_a
head(p2)
RMSE2 = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p2 ) ^2 ) )
print(RMSE2)

#Rerun at least three times to be assured that the model is optimal and not over fit or under fit.
# Rerun 1================
FLAG = sample( c( TRUE, FALSE ), nrow(df_4), replace=TRUE, prob=c(0.8,0.2))
df_train = df_4[FLAG, ]
df_test = df_4[! FLAG, ]

# Rerun 1 Train data
df_4_flag = df_train
df_4_flag$TARGET_LOSS_AMT = NULL
tr_set = rpart.control( maxdepth = 3 )
t2_f = rpart(data = df_4_flag, TARGET_BAD_FLAG ~ ., control=tr_set)

df_4_amt = subset( df_train, TARGET_BAD_FLAG == 1)
df_4_amt$TARGET_BAD_FLAG = NULL
tr_set = rpart.control( maxdepth = 2 )
t2_a = rpart(data = df_4_amt, TARGET_LOSS_AMT ~ ., control=tr_set)

p2_f = predict( t2_f, df_train)
p2_a = predict(t2_a, df_train)
p2 = p2_f * p2_a
RMSE2 = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p2 ) ^2 ) )
print(RMSE2)

# Rerun 1 Test data
df_4_flag = df_test
df_4_flag$TARGET_LOSS_AMT = NULL
tr_set = rpart.control( maxdepth = 3 )
t2_f = rpart(data = df_4_flag, TARGET_BAD_FLAG ~ ., control=tr_set)

df_4_amt = subset( df_test, TARGET_BAD_FLAG == 1)
df_4_amt$TARGET_BAD_FLAG = NULL
tr_set = rpart.control( maxdepth = 2 )
t2_a = rpart(data = df_4_amt, TARGET_LOSS_AMT ~ ., control=tr_set)

p2_f = predict( t2_f, df_test)
p2_a = predict(t2_a, df_test)
p2 = p2_f * p2_a
RMSE2 = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p2 ) ^2 ) )
print(RMSE2)

# Rerun 2===============
FLAG = sample( c( TRUE, FALSE ), nrow(df_4), replace=TRUE, prob=c(0.85,0.15))
df_train = df_4[FLAG, ]
df_test = df_4[! FLAG, ]

# Rerun 2 Train data
df_4_flag = df_train
df_4_flag$TARGET_LOSS_AMT = NULL
tr_set = rpart.control( maxdepth = 3 )
t2_f = rpart(data = df_4_flag, TARGET_BAD_FLAG ~ ., control=tr_set)

df_4_amt = subset( df_train, TARGET_BAD_FLAG == 1)
df_4_amt$TARGET_BAD_FLAG = NULL
tr_set = rpart.control( maxdepth = 2 )
t2_a = rpart(data = df_4_amt, TARGET_LOSS_AMT ~ ., control=tr_set)

p2_f = predict( t2_f, df_train)
p2_a = predict(t2_a, df_train)
p2 = p2_f * p2_a
RMSE2 = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p2 ) ^2 ) )
print(RMSE2)

# Rerun 2 Test data
df_4_flag = df_test
df_4_flag$TARGET_LOSS_AMT = NULL
tr_set = rpart.control( maxdepth = 3 )
t2_f = rpart(data = df_4_flag, TARGET_BAD_FLAG ~ ., control=tr_set)

df_4_amt = subset( df_test, TARGET_BAD_FLAG == 1)
df_4_amt$TARGET_BAD_FLAG = NULL
tr_set = rpart.control( maxdepth = 2 )
t2_a = rpart(data = df_4_amt, TARGET_LOSS_AMT ~ ., control=tr_set)

p2_f = predict( t2_f, df_test)
p2_a = predict(t2_a, df_test)
p2 = p2_f * p2_a
RMSE2 = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p2 ) ^2 ) )
print(RMSE2)


# Rerun 3==================
FLAG = sample( c( TRUE, FALSE ), nrow(df_4), replace=TRUE, prob=c(0.65,0.35))
df_train = df_4[FLAG, ]
df_test = df_4[! FLAG, ]

# Rerun 3 Train data
df_4_flag = df_train
df_4_flag$TARGET_LOSS_AMT = NULL
tr_set = rpart.control( maxdepth = 3 )
t2_f = rpart(data = df_4_flag, TARGET_BAD_FLAG ~ ., control=tr_set)

df_4_amt = subset( df_train, TARGET_BAD_FLAG == 1)
df_4_amt$TARGET_BAD_FLAG = NULL
tr_set = rpart.control( maxdepth = 2 )
t2_a = rpart(data = df_4_amt, TARGET_LOSS_AMT ~ ., control=tr_set)

p2_f = predict( t2_f, df_train)
p2_a = predict(t2_a, df_train)
p2 = p2_f * p2_a
RMSE2 = sqrt( mean( ( df_train$TARGET_LOSS_AMT - p2 ) ^2 ) )
print(RMSE2)

# Rerun 3 Test data
df_4_flag = df_test
df_4_flag$TARGET_LOSS_AMT = NULL
tr_set = rpart.control( maxdepth = 3 )
t2_f = rpart(data = df_4_flag, TARGET_BAD_FLAG ~ ., control=tr_set)

df_4_amt = subset( df_test, TARGET_BAD_FLAG == 1)
df_4_amt$TARGET_BAD_FLAG = NULL
tr_set = rpart.control( maxdepth = 2 )
t2_a = rpart(data = df_4_amt, TARGET_LOSS_AMT ~ ., control=tr_set)

p2_f = predict( t2_f, df_test)
p2_a = predict(t2_a, df_test)
p2 = p2_f * p2_a
RMSE2 = sqrt( mean( ( df_test$TARGET_LOSS_AMT - p2 ) ^2 ) )
print(RMSE2)

