#W6 R Programming: PCA and TSNE

#========================Step 1: Use the Decision Tree / Random Forest / Decision Tree / Regression code 
#from Week 6 as a Starting Point=====================

#Load library
library( rpart )
library( rpart.plot )
library(ROCR)
library(randomForest)
library(gbm)
library( MASS )
library( Rtsne )

#Read the data into R
PATH 		= "/Users/chichi/Desktop/IS_5213/W3/HMEQ"
FILE_NAME 	= "HMEQ_Scrubbed.csv"
INFILE = paste(PATH, FILE_NAME, sep="/")
df = read.csv( INFILE )

#===Step 2: PCA Analysis. Use only the input variables. Do not use either of the target variables.=======================
SEED = 1
set.seed( SEED )

TARGET = "TARGET_CLM_FLAG"
df_pca = df
df_pca$TARGET_CLM_FLAG = NULL
df_pca$TARGET_CLM_AMT = NULL
summary(df_pca)

#Use only the continuous variables. Do not use any of the flag variables.
#Do a Principal Component Analysis (PCA) on the continuous variables.
str(df_pca)
pca2 = prcomp(df_pca[,c(1,2,4,6,12,18)] ,center=TRUE, scale=TRUE)
summary(pca2)
#Display the Scree Plot of the PCA analysis.
plot(pca2, type = "l")
#Print the weights of the Principal Components. Use the weights to tell a story on what the Principal Components represent.
pca2

#Perform a scatter plot using the first two Principal Components. 
#Color the scatter plot dots using the Target Flag. 
#One color will represent "defaults" and the other color will represent "non defaults". 
#Comment on whether you consider the first two Principal Components to be predictive. 
#If you believe the graph is too cluttered, you are free to do a random sample of the data to make it more readable. 
#That is up to you.
df_new = predict( pca2, df_pca )

df_no_flags = df
df_no_flags$PC1 = df_new[,"PC1"]
df_no_flags$PC2 = df_new[,"PC2"]

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[df_no_flags$TARGET_BAD_FLAG + 1]
plot( df_no_flags$PC1, df_no_flags$PC2, col=colors, pch=16 )

# This code takes a random sample of the data so that we can visualize it easier.
df_no_flags$RAND1 = sample(100, size = nrow(df_no_flags), replace = TRUE)
df_no_flags$RAND2 = sample(100, size = nrow(df_no_flags), replace = TRUE)

df_no_flags0 = df_no_flags[ which(df_no_flags$TARGET_BAD_FLAG == 0), ]
df_no_flags1 = df_no_flags[ which(df_no_flags$TARGET_BAD_FLAG == 1), ]

df_no_flags0 = df_no_flags0[ df_no_flags0$RAND1 < 25, ]
df_no_flags1 = df_no_flags1[ df_no_flags1$RAND1 < 75, ]

df_no_flagsx = rbind( df_no_flags0, df_no_flags1 )
df_no_flagsx = df_no_flagsx[ df_no_flagsx$RAND2 < 15, ]

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[df_no_flagsx$TARGET_BAD_FLAG + 1]
plot( df_no_flagsx$PC1, df_no_flagsx$PC2, col=colors, pch=16 )

#=====step3 tSNE Analysis====================
#Use only the input variables. Do not use either of the target variables.
#Use only the continuous variables. Do not use any of the flag variables.
dfu = df
dfu$TARGET_LOSS_AMT = NULL
dfu = unique(dfu)
head( dfu )
str(dfu)

#Do a tSNE analysis on the data. Set the dimensions to 2. 
#Run two tSNE analysis for Perplexity=30. Color the scatter plot dots using the Target Flag. 
#One color will represent "defaults" and the other color will represent "non defaults". 
#Comment on whether you consider the tSNE values to be predictive.
theTSNE = Rtsne( dfu[,c(2,3,5,7,13,17,19)], dims = 2, perplexity=100, verbose=TRUE, max_iter = 500)
dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]
colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[dfu$TARGET_BAD_FLAG + 1]
plot( dfu$TS1, dfu$TS2, col=colors, pch=16 )
#Repeat the previous step with a Perplexity greater than 30 (try to get a value much higher than 30).
theTSNE = Rtsne( dfu[,c(2,3,5,7,13,17,19)], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]
colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[dfu$TARGET_BAD_FLAG + 1]
plot( dfu$TS1, dfu$TS2, col=colors, pch=16 )
#Repeat the previous step with a Perplexity less than 30 (try to get a value much lower than 30).
theTSNE = Rtsne( dfu[,c(2,3,5,7,13,17,19)], dims = 2, perplexity=5, verbose=TRUE, max_iter = 500)
dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]
colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[dfu$TARGET_BAD_FLAG + 1]
plot( dfu$TS1, dfu$TS2, col=colors, pch=16 )

#Train two Random Forest Models to predict each of the tSNE values.
dfu$TS1 = theTSNE$Y[,1]
dfu$TS2 = theTSNE$Y[,2]

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[dfu$TARGET_BAD_FLAG + 1]
plot( dfu$TS1, dfu$TS2, col=colors, pch=16 )

P = paste(colnames(dfu)[c(2,3,5,7,13,17,19)], collapse = "+")
F1 = as.formula( paste("TS1 ~", P ) )
F2 = as.formula( paste("TS2 ~", P ) )

print( F1 )
print( F2 )

ts1_model = lm( F1,data=dfu )
ts2_model = lm( F2,data=dfu )

ts1_model_rf = randomForest( data=dfu, F1, ntree=500, importance=TRUE )
ts2_model_rf = randomForest( data=dfu, F2, ntree=500, importance=TRUE )

df_tsne = df

df_tsne$TS1M = predict( ts1_model, df_tsne )
df_tsne$TS2M = predict( ts2_model, df_tsne )

df_tsne$TS1M_RF = predict( ts1_model_rf, df_tsne )
df_tsne$TS2M_RF = predict( ts2_model_rf, df_tsne )
 
# This code takes a random sample of the data so that we can visualize it easier.
df_tsne$RAND1 = sample(100, size = nrow(df_tsne), replace = TRUE)
df_tsne$RAND2 = sample(100, size = nrow(df_tsne), replace = TRUE)

df_tsne0 = df_tsne[ which(df_tsne$TARGET_BAD_FLAG == 0), ]
df_tsne1 = df_tsne[ which(df_tsne$TARGET_BAD_FLAG == 1), ]

df_tsne0 = df_tsne0[ df_tsne$RAND1 < 25, ]
df_tsne1 = df_tsne1[ df_tsne$RAND1 < 75, ]

df_tsnex = rbind( df_tsne0, df_tsne1 )
df_tsnex = df_tsnex[ df_tsnex$RAND2 < 20, ]

colors <- c("#00AFBB", "#E7B800")
colors <- c("red", "black")
colors <- colors[df_tsnex$TARGET_BAD_FLAG + 1]
plot( df_tsnex$TS1M, df_tsnex$TS2M, col=colors, pch=16 )

#======STEP 4: Tree and Regression Analysis on the Original Data
#Create a Decision Tree to predict Loan Default (Target Flag=1).=====

df_model = df
df_model$TARGET_LOSS_AMT = NULL

head( df_model )
 
tr_set = rpart.control( maxdepth = 5 )
t1G = rpart( data=df_model, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
rpart.plot( t1G )

#Comment on the variables that were included in the model.
t1G$variable.importance
 
#Create a Logistic Regression model to predict Loan Default (Target Flag=1). 
#Use either Forward, Backward, or Stepwise variable selection.
theUpper_LR = glm( TARGET_BAD_FLAG ~ ., family = "binomial", data=df_model )
theLower_LR = glm( TARGET_BAD_FLAG ~ 1, family = "binomial", data=df_model )
lr_model = stepAIC(theLower_LR, direction="forward", scope=list(lower=theLower_LR, upper=theUpper_LR))

summary( lr_model )

#Create a ROC curve showing the accuracy of the model.
pG = predict( t1G, df_model )
pG2 = prediction( pG[,2], df_model$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )

plr = predict( lr_model, df_model, type="response" )
plr2 = prediction( plr, df_model$TARGET_BAD_FLAG )
plr3 = performance( plr2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( plr3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("GINI","REGRESSION"),col=c("red","blue"), bty="y", lty=1 )

#Calculate and display the Area Under the ROC Curve (AUC).
aucG = performance( pG2, "auc" )@y.values
aucR = performance( plr2, "auc" )@y.values
 
print( aucG )
print( aucR )


#===========STEP5: Tree and Regression Analysis on the PCA/tSNE Data=======
#Append the Principal Component values from Step 2 to your data set.
df_model = df
df_model$TARGET_LOSS_AMT = NULL

df_model$PC1 = df_new[,"PC1"]
df_model$PC2 = df_new[,"PC2"]
df_model$PC3 = df_new[,"PC3"]
df_model$PC4 = df_new[,"PC4"]

#Using the Random Forest models from Step 3, append the two tSNE values to the data set.
df_model$TS1M_RF = predict( ts1_model_rf, df_model )
df_model$TS2M_RF = predict( ts2_model_rf, df_model )
head(df_model)

#Remove all of the continuous variables from the data set (set them to NULL). 
#Keep the flag variables in the data set.
df_model$LOAN = NULL
df_model$IMP_MORTDUE = NULL
df_model$IMP_VALUE = NULL
df_model$IMP_YOJ = NULL
df_model$IMP_CLAGE = NULL
df_model$IMP_CLNO = NULL
df_model$IMP_DEBTINC = NULL

head( df_model )

#Create a Decision Tree to predict Loan Default (Target Flag=1).
tr_set = rpart.control( maxdepth = 5 )
t1G = rpart( data=df_model, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='gini') )
rpart.plot( t1G )

#Comment on the variables that were included in the model.
t1G$variable.importance

#Create a Logistic Regression model to predict Loan Default (Target Flag=1). 
#Use either Forward, Backward, or Stepwise variable selection.
theUpper_LR = glm( TARGET_BAD_FLAG ~ ., family = "binomial", data=df_model )
theLower_LR = glm( TARGET_BAD_FLAG ~ 1, family = "binomial", data=df_model )
lr_model = stepAIC(theLower_LR, direction="forward", scope=list(lower=theLower_LR, upper=theUpper_LR))

summary( lr_model )

#Create a ROC curve showing the accuracy of the model.
pG = predict( t1G, df_model )
pG2 = prediction( pG[,2], df_model$TARGET_BAD_FLAG )
pG3 = performance( pG2, "tpr", "fpr" )
plr = predict( lr_model, df_model, type="response" )
plr2 = prediction( plr, df_model$TARGET_BAD_FLAG )
plr3 = performance( plr2, "tpr", "fpr" )

plot( pG3, col="red" )
plot( plr3, col="blue", add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("GINI","REGRESSION"),col=c("red","blue"), bty="y", lty=1 )

#Calculate and display the Area Under the ROC Curve (AUC).
aucG = performance( pG2, "auc" )@y.values
aucR = performance( plr2, "auc" )@y.values

print( aucG )
print( aucR )
