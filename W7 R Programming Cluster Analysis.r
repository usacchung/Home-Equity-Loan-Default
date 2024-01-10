#W7 R Programming: Cluster Analysis

#===========Step 1: Use the code from Week 6 as a Starting Point===
#In this assignment, we will not be doing all the analysis as before. 
#But much of the code from week 6 can be used as a starting point for this assignment. 
#For this assignment, do not be concerned with splitting data into training 
#and test sets. In the real world, you would do that. 
#But for this exercise, it would only be an unnecessary complication.

#Load library
library( rpart )
library( rpart.plot )
library(ROCR)
library(randomForest)
library(gbm)
library( MASS )
library( Rtsne )
library( Rtsne )
library( ggplot2 )
library( flexclust )

#Read the data into R
PATH 		= "/Users/chichi/Desktop/IS_5213/W3/HMEQ"
FILE_NAME 	= "HMEQ_Scrubbed.csv"
INFILE = paste(PATH, FILE_NAME, sep="/")
df = read.csv( INFILE )

#======Step 2: PCA Analysis=================

#Use only the input variables. Do not use either of the target variables.
df_pca = df
df_pca$TARGET_BAD_FLAG = NULL
df_pca$TARGET_LOSS_AMT = NULL

#Use only the continuous variables. Do not use any of the flag variables.
#Select at least 4 of the continuous variables. 
#It would be preferable if there were a theme to the variables selected.
#=====================
#MORTDUE
#DEROG
#DELINQ
#DEBTINC

df_pca = df[ , c( "IMP_MORTDUE", "IMP_DEROG", "IMP_DELINQ", "IMP_DEBTINC" ) ]

#Do a Principal Component Analysis (PCA) on the continuous variables.
#Display the Scree Plot of the PCA analysis.
pca = prcomp(df_pca, center=TRUE, scale=TRUE)
#Print the weights of the Principal Components.
#Use the weights to tell a story on what the Principal Components represent.
print( pca )
summary(pca)
plot(pca, type = "l")

#Perform a scatter plot using the first two Principal Components. 
#Do not color the dots. Leave them black.
df_new = data.frame( predict( pca, df_pca ) )
head(df_new)

df_kmeans = df_new[1:2]
print( head( df_kmeans ) )
plot( df_kmeans$PC1, df_kmeans$PC2 )

#======Step 3: Cluster Analysis - Find the Number of Clusters=======

#Use the principal components from Step 2 for this step.
#Using the methods presented in the lectures, 
#complete a KMeans cluster analysis for N=1 to at least N=10. 
#Feel free to take the number higher.

# Maximum Clusters To Search
MAX_N = 10

# Set up an array to hold the Sum of Square Errors
WSS = numeric( MAX_N )

for ( N in 1:MAX_N ) 
{
  km = kmeans( df_kmeans, centers=N, nstart=20  )
  WSS[N] = km$tot.withinss
}

df_wss = as.data.frame( WSS )
df_wss$clusters = 1:MAX_N

#Print a scree plot of the clusters and determine how many clusters would be optimum.
scree_plot = ggplot( df_wss, aes( x=clusters, y=WSS, group=1 )) +
  geom_point( size=4 ) +
  geom_line() +
  scale_x_continuous( breaks=c(2,4,6,8,10)) +
  xlab("Number of Clusters")

scree_plot

#============Step 4: Cluster Analysis====================

#Using the number of clusters from step 3, 
#perform a cluster analysis using the principle components from Step 2.
BEST_N = 4
km = kmeans( df_kmeans, centers=BEST_N, nstart=20  )

#Print the number of records in each cluster.
print( km$size )
#Print the cluster center points for each cluster
print( km$centers )

#Convert the KMeans clusters into "flexclust" clusters
kf = as.kcca( object=km, data=df_kmeans, save.data=TRUE )
kfi = kcca2df( kf )

#Print the barplot of the cluster. Describe the clusters from the barplot.
barplot(kf)

#Score the training data using the flexclust clusters. 
#In other words, determine which cluster they are in.
clus = predict( kf, df_kmeans )
df$CLUSTER = clus
agg = aggregate( df$TARGET_BAD_FLAG, list( df$CLUSTER ), FUN=mean )
#Determine if the clusters predict loan default.
agg

#Perform a scatter plot using the first two Principal Components. 
#Color the plot by the cluster membership. 
plot( df_kmeans$PC1, df_kmeans$PC2 )
plot( df_kmeans$PC1, df_kmeans$PC2, col=clus )
#Add a legend to the plot.
legend( x="topright", legend=c(1:BEST_N), fill=c(1:BEST_N) )

#================Step 5: Describe the Clusters Using Decision Trees========

df_tree = df_pca
df_tree$CLUSTER = as.factor(clus)
dt = rpart( CLUSTER ~ . , data=df_tree )
rpart.plot( dt )
 
