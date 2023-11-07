#W3 R Programming: Decision Trees

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
#Use the rpart library to predict the variable TARGET_BAD_FLAG
#Develop two decision trees, one using Gini and the other using Entropy
#All other parameters such as tree depth are up to you.
#Plot both decision trees
df_flag = df
df_flag$TARGET_LOSS_AMT = NULL
tr_set = rpart.control( maxdepth = 10 )
t1G = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
t1E = rpart( data=df_flag, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )
rpart.plot( t1G )
rpart.plot( t1E )

#List the important variables for both trees
t1G$variable.importance
t1E$variable.importance

#Create a ROC curve for both trees
pG = predict(t1G, df)
pG2 = prediction( pG[,2], df$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

pE = predict( t1E, df )
pE2 = prediction( pE[,2], df$TARGET_BAD_FLAG )
pE3 = performance( pE2, "tpr", "fpr" )

plot(pG3, col="red")
plot(pE3, col="green", add=TRUE)
abline(0,1,lty=2)

legend("bottomright", c("GINI", "ENTROPY"), col=c("red","green"), bty="y", lty=1)

#========================Step 3: Regression Decision Tree=====================

#Use the rpart library to predict the variable TARGET_LOSS_AMT
#Develop two decision trees, one using anova and the other using poisson
#All other parameters such as tree depth are up to you.
#Plot both decision trees
df_amt = df
df_amt$TARGET_BAD_FLAG = NULL

tr_set = rpart.control(maxdepth = 10)
t1a = rpart(data = df_amt, TARGET_LOSS_AMT ~ ., control = tr_set, method = "anova")
rpart.plot(t1a)

t1p = rpart(data = df_amt, TARGET_LOSS_AMT ~ ., control = tr_set, method = "poisson")
rpart.plot(t1p)

#List the important variables for both trees
t1a$variable.importance
t1p$variable.importance

#Calculate the Root Mean Square Error (RMSE) for both trees
#Decision tress (anova):
p1a = predict(t1a, df)
RMSE1a = sqrt( mean( ( df$TARGET_LOSS_AMT - p1a ) ^ 2 ) )
print( RMSE1a )

#Decision tress (Poisson):
p1p = predict(t1p, df)
RMSE1p = sqrt( mean( ( df$TARGET_LOSS_AMT - p1p ) ^ 2 ) )
print( RMSE1p )

#========================Step 4: Probability / Severity Model Decision Tree=====================

#Use the rpart library to predict the variable TARGET_BAD_FLAG
#Plot decision trees
df_flag = df
df_flag$TARGET_LOSS_AMT = NULL

t2_f = rpart(data = df_flag, TARGET_BAD_FLAG ~ ., control=tr_set)
rpart.plot( t2_f )
p2_f = predict( t2_f, df)
head(p2_f)

#Use the rpart library to predict the variable TARGET_LOSS_AMT using only records where TARGET_BAD_FLAG is 1.
#Plot decision trees
df_amt_2 = subset( df, TARGET_BAD_FLAG == 1)
df_amt_2$TARGET_BAD_FLAG = NULL
t2_a = rpart(data = df_amt_2, TARGET_LOSS_AMT ~ ., control=tr_set)
rpart.plot( t2_a )
p2_a = predict(t2_a, df)

#List the important variables for both trees
t2_f$variable.importance
t2_a$variable.importance

#Using your models, predict the probability of default and the loss given default.
head(p2_f)
head(p2_a)

#Multiply the two values together for each record.
p2 = p2_f * p2_a
head(p2)
RMSE2 = sqrt( mean( ( df$TARGET_LOSS_AMT - p2 ) ^2 ) )
print(RMSE2)
