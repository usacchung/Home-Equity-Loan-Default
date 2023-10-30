#W2 R Programming: Data Scrubbing
#========================Step 1: Read in the Data=====================

#Read the data into R
PATH 		= "/Users/chichi/Desktop/IS_5213/W2"
FILE_NAME 	= "HMEQ_Loss.csv"
INFILE = paste(PATH, FILE_NAME, sep="/")
df = read.csv( INFILE )

#List the structure of the data (str)
str(df)

#Execute a summary of the data
summary(df)

#Print the first six records
head(df)

#========================Step 2: Box-Whisker Plots=====================

#Plot a box plot of all the numeric variables split by the grouping variable. The plot needs the following:
#The MAIN TITLE of the box plot should be set to your name
#Add color to the boxes
library(reshape2)
library(ggplot2)
df_new <- melt(df, id = c("TARGET_BAD_FLAG", "REASON", "JOB"))
p <- ggplot(df_new, aes(x=variable, y=value)) + geom_boxplot(aes(fill=factor(TARGET_BAD_FLAG)))
p <- p + facet_wrap( ~ variable, scales="free")
p + xlab("All the Numeric Variables") + ylab("Value") + ggtitle("Boxplot of HMEQ_Loss from Chi Chung")

#========================Step 3: Histograms=====================

#Plot a histogram of at least one of the numeric variables
#Manually set the number of breaks to a value that makes sense
#Superimpose a density line to the graph
hist( df$LOAN, breaks=50, prob=TRUE)
lines( density( df$LOAN ), col=c("red") )

#========================Step 4: Impute "Fix" all the numeric variables that have missing values=====================

#For the missing Target variables, simply set the missing values to zero
df$TARGET_LOSS_AMT[ is.na(df$TARGET_LOSS_AMT)] = 0

#For the remaining numeric variables with missing values, create two new variables. 
#One variable will have a name beginning with IMP_ and it will contained the imputed value. 
#The second value will have a name beginning with M_ and it will contain a 1 if the record was imputed and a zero if it was not.
df$IMP_MORTDUE = df$MORTDUE
df$M_MORTDUE = is.na(df$MORTDUE) + 0

df$IMP_YOJ = df$YOJ
df$M_YOJ = is.na(df$YOJ) + 0

df$IMP_DEROG = df$DEROG
df$M_DEROG = is.na(df$DEROG) + 0

df$IMP_DELINQ = df$DELINQ
df$M_DELINQ = is.na(df$DELINQ) + 0

df$IMP_CLAGE = df$CLAGE
df$M_CLAGE = is.na(df$CLAGE) + 0

df$IMP_NINQ = df$NINQ
df$M_NINQ = is.na(df$NINQ) + 0

df$IMP_CLNO = df$CLNO
df$M_CLNO = is.na(df$CLNO) + 0

df$IMP_DEBTINC = df$DEBTINC
df$M_DEBTINC = is.na(df$DEBTINC) + 0

#You may impute with any method that makes sense. 
#The median or mean value will be useful in most cases.
df$IMP_MORTDUE[ is.na(df$MORTDUE)] = 65019
df$IMP_YOJ[ is.na(df$YOJ)] = 7
df$IMP_DEROG[ is.na(df$DEROG)] = 0
df$IMP_DELINQ[ is.na(df$DELINQ)] = 0
df$IMP_CLAGE[ is.na(df$CLAGE)] = 173.5
df$IMP_NINQ[ is.na(df$NINQ)] = 1
df$IMP_CLNO[ is.na(df$CLNO)] = 20
df$IMP_DEBTINC[ is.na(df$DEBTINC)] = 34.8183

#Bonus: Try one complex imputation like the one described in the lectures.
a = aggregate( x=df$VALUE, by=list( df$JOB ), na.rm=TRUE, FUN=median )
a = a[ order(a$x, decreasing=TRUE), ]
a

df$IMP_VALUE = df$VALUE
df$IMP_VALUE[ is.na(df$VALUE) & ( df$JOB == "Self"     ) ] = 130631
df$IMP_VALUE[ is.na(df$VALUE) & ( df$JOB == "ProfExe"  ) ] = 110007
df$IMP_VALUE[ is.na(df$VALUE) & ( df$JOB == "Mgr"      ) ] = 101258
df$IMP_VALUE[ is.na(df$VALUE) & ( df$JOB == "Office"   ) ] = 89094.5
df$IMP_VALUE[ is.na(df$VALUE) & ( df$JOB == "Sales"    ) ] = 84473.5
df$IMP_VALUE[ is.na(df$VALUE) & ( df$JOB == "Other"    ) ] = 76599.5
df$IMP_VALUE[ is.na(df$VALUE) ] = 78227
df$M_VALUE = is.na(df$VALUE) + 0

#Delete the original variable after it has been imputed.
df$MORTDUE = NULL
df$VALUE = NULL
df$YOJ = NULL
df$DEROG = NULL
df$DELINQ = NULL
df$CLAGE = NULL
df$NINQ = NULL
df$CLNO = NULL
df$DEBTINC = NULL

#Run a summary to prove that all the variables have been imputed
summary(df)

#Compute a sum for all the M_ variables to prove that the number of flags is equal to the number of missing values.
sum( df$M_MORTDUE)
sum(df$M_VALUE)
sum( df$M_YOJ)
sum( df$M_DEROG)
sum( df$M_DELINQ)
sum( df$M_CLAGE)
sum( df$M_NINQ)
sum( df$M_CLNO)
sum( df$M_DEBTINC)

#========================Step 5: One Hot Encoding=====================

#For the character / category variables, perform one hot encoding. 
#For this create a Flag for each categories.
table(df$REASON)
table(df$JOB)

#REASON
df$FLAG.DebtCon = ( df$REASON == "DebtCon" ) + 0
df$FLAG.HomeImp = ( df$REASON == "HomeImp" ) + 0
#JOB
df$FLAG.Mgr = ( df$JOB == "Mgr" ) + 0
df$FLAG.Office = ( df$JOB == "Office" ) + 0
df$FLAG.Other = ( df$JOB == "Other" ) + 0
df$FLAG.ProfExe = ( df$JOB == "ProfExe" ) + 0
df$FLAG.Sales = ( df$JOB == "Sales" ) + 0
df$FLAG.Self = ( df$JOB == "Self" ) + 0
df$FLAG.HighSalary = ( df$JOB %in% c("Mgr", "ProfExe") ) + 0

#Delete the original class variable
df$REASON = NULL
df$JOB = NULL

#Run a summary to show that the category variables have been replaced by Flag variables.
summary(df)
 
