#W1 R Programming

#========================Step 1: Describe the Data=====================

#List the structure of the data (str)
#Execute a summary of the data
#Print the first six records

str( iris )
summary( iris ) 
head(iris)

#========================Step 2: Box-Whisker Plots=====================

#Plot a box plot of the numeric variable split by the grouping variable. The plot needs the following:
#The MAIN TITLE of the box plot should be set to your name
#Add a notch to the boxes
#Add color to the boxes

boxplot(iris$Sepal.Length ~ iris$Species, notch=TRUE, col=c("darkred","blue"), main="My Name is Catherine Chung")

#========================Step 3: Histograms=====================

#Plot a histogram of the numeric variable
#Manually set the number of breaks to a value that makes sense
#Superimpose a density line to the graph
#Bonus: Find a way to add color or decoration to this graph and earn bonus points. 

hist( iris$Sepal.Length, breaks=30, prob=TRUE, col=c("darkred"))
lines( density( iris$Sepal.Length ), col=c("blue") )

#========================Step 4: Scatter Plots=====================

#Create a scatter plot of the numeric variable and variable #2.

#Each group member needs to be in a different color
#Set the plot character to a value that is easy to see. Character value 16 is a good choice.
#BONUS: Find a way to add color or decoration to this graph and earn bonus points.

plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species, pch=16, main="Scatter Plot")
legend("topright", legend=c("setosa","virginica", "versicolor"))

#========================Step 5: Simple Math=====================

#For the numeric variable, compute the following statistics
#Mean
#Median
#Min
#Max
#Standard Deviation

mean( iris$Sepal.Length )
median( iris$Sepal.Length )
min( iris$Sepal.Length )
max( iris$Sepal.Length )
sd( iris$Sepal.Length )

#Calculate the Median for the numeric for each group member. Sort the result in Descending order.
a = aggregate( x=iris$Sepal.Length, by=list( iris$Species), FUN=median )
a = a[ order( a$x, decreasing=TRUE), ]
a