# attach the iris dataset to the environment
data(iris)
dataset <- iris
#type of dataset
class(dataset)
attach(dataset)



########################   Indices of Location ########################

#number of columns
ncol(dataset)
#columns' names
colnames(dataset)
#number of rows
nrow(dataset)
#attributes
attributes(dataset)

########################   Spread of Data ########################

#mean 
mean(Sepal.Length)
mean(Sepal.Width)
mean(Petal.Length)
mean(Petal.Width)
#median
median(Sepal.Length)
median(Sepal.Width)
median(Petal.Length)
median(Petal.Width)

#Variance 
var(Sepal.Length)
var(Sepal.Width)
var(Petal.Length)
var(Petal.Width)
#standard deviation 
sd(Sepal.Length)
sd(Sepal.Width)
sd(Petal.Length)
sd(Petal.Width)


#summary stats for all variables in dataset(spread of data: min, max,1st Q, mean, median and 3rd Q for each column)
summary(dataset)


#asymmetry
skewness(Sepal.Length)
skewness(Sepal.Width)
skewness(Petal.Length)
skewness(Petal.Width)
#negative skewness indicates that the mean of the data values is less than the median, and the data distribution is left-skewed. 
#Positive skewness would indicate that the mean of the data values is larger than the median, and the data distribution is right-skewed.

#kurtosis
kurtosis(Sepal.Length)
kurtosis(Sepal.Width)
kurtosis(Petal.Length)
kurtosis(Petal.Width)

#If the kurtosis is greater than 3, then the dataset has heavier tails than a normal distribution (more in the tails) (leptokurtic distribution). 
#If the kurtosis is equal to 3, then the dataset has ha normal distribution .
#If the kurtosis is less than 3, then the dataset has lighter tails than a normal distribution (less in the tails) (platykurtic distribution)

#frequent subinterval table
table(cut(Sepal.Length,3))
table(cut(Sepal.Width,3))
table(cut(Petal.Length,3))
table(cut(Petal.Width,3))



#the proportion of the variables by Species
prop.table(xtabs(Sepal.Length~Species)) 
prop.table(xtabs(Sepal.Width~Species))
prop.table(xtabs(Petal.Length~Species)) 
prop.table(xtabs(Petal.Width~Species))


########################   graphs ########################
#barplot
barplot(table(Sepal.Length),main="Sepal.Length") 
barplot(table(Sepal.Width),main="Sepal.Width") 
barplot(table(Petal.Length),main="Petal.Length") 
barplot(table(Petal.Width),main="Petal.Width") 


#create histogram of Sepal Length variable
hist(Sepal.Length,freq = F,xlab = "Sepal Length variable", main = "Histogram of Sepal length variable")
#adding the density curve over the polt
lines(density(Sepal.Length),col=2,lwd=3)


#create histogram of Sepal Width variable
hist(Sepal.Width,freq = F,xlab = "Sepal Width variable", main = "Histogram of Sepal Width variable")
#adding the density curve over the polt
lines(density(Sepal.Width),col=2,lwd=3)


#create histogram of Petal Length variable
hist(Petal.Length,freq = F,xlab = "Petal Length variable", main = "Histogram of Petal length variable")
#adding the density curve over the polt
lines(density(Petal.Length),col=2,lwd=3)


#create histogram of Petal Width variable
hist(Petal.Width,freq = F,xlab = "Petal Width variable", main = "Histogram of Petal Width variable")
#adding the density curve over the polt
lines(density(Petal.Width),col=2,lwd=3)

# Notice the shape of the data, most attributes exhibit a normal distribution. 

#pie to show the species
pie(table(Species), main = "Pie Chart of the Iris data set Species", radius = 1)

#Boxplot
#compare each variable with the Species variable
boxplot(Sepal.Length~Species, ylab = "Sepal Length", main = "distribution of Sepal Length for Species variables")
boxplot(Sepal.Width~Species, ylab = "Sepal Width", main = "distribution of Sepal Width for Species variables")
boxplot(Petal.Length~Species, ylab = "Petal length", main = "distribution of Petal length for Species variables")
boxplot(Petal.Width~Species, ylab = "Petal Width", main = "distribution of Petal Width for Species variables")


#get the plot for numeric columns:
pairs(iris[,1:4])

# Check the covariance and correlation between variables
cov(dataset[ ,1:4])
cor(dataset[ ,1:4])

# correlation is positive when the values increases together (positive linear correlation).
# correlation is ~ Zero when there is no linear association between variables.
# correlation in negative when the values decreses together (negative linear correlation).




########################   Hypothesis ########################
#t test
t.test(Petal.Length, mu=0 , alternative = "less")

# Subset setosa data
setosa <- dataset[dataset$Species == "setosa", ]
# Subset versicolor data
versicolor <- dataset[dataset$Species == "versicolor", ]
# Subset virginica data
virginica <- dataset[dataset$Species == "virginica", ]



#Do t test for spesific Species
# t test for Petal Lenght for different Species
t.test(x = versicolor$Petal.Length, y = virginica$Petal.Length)
# we noticed that versicolor and virginica also have different significantly different petal lengths.
t.test(x = setosa$Petal.Length, y = virginica$Petal.Length)
#we noticed that setosa and virginica have significantly different petal lengths.



#In the result above :
# t is the t-test statistic value ,
#df is the degrees of freedom ,
#p-value is the significance level of the t-test .
#conf.int is the confidence interval of the mean at 95% .
# sample estimates is he mean value of the sample .

#If the p-value is inferior or equal to the significance level 0.05, we can reject the null hypothesis and accept the alternative hypothesis. 
#In other words, we conclude that the sample mean is significantly different from the theoretical mean.





#ANOVA for all variables with Species
#ANOVA allows us to simultaneously compare multiple groups, to test whether group membership has a significant effect on a variable of interest.
#The question we will address is: are there differences in petal length among the three species?
aov(Petal.Length~Species)
summary(aov(Petal.Length~Species))
# The species do have significantly different petal lengths. 
aov(Sepal.Length~Species)
summary(aov(Sepal.Length~Species))
aov(Sepal.Width~Species)
summary(aov(Sepal.Width~Species))
#The F-statistic = 49.16, and the p-value is quite small, so there are significant sepal width differences among species.
aov(Petal.Width~Species)
summary(aov(Petal.Width~Species))



########################   Linear Regression Module ########################



set.seed(12)
library(caTools)
#split the data with ration of 70%
split<- sample.split(dataset, SplitRatio=0.7)
#create the subsets for studing and testing
studied_set <- subset(dataset, split="TRUE")
test <- subset(dataset, split="FALSE")




#create the module to predicate the Peta. length variable
mod_Petal_Length <- lm(Petal.Length~. , data = dataset)
#check the summary result of this module
summary(mod_Petal_Length)
#prediction
pred_Petal_Length <- predict(mod_Petal_Length, test)
#prediction value
pred_Petal_Length
#check the Petal.Length values only
plot(Petal.Length,type='l',lty=1.8, col=2)

#check the predicted values only
plot(pred_Petal_Length,type='l',lty=1.8, col="blue")

#comparing predicted VS actual values of Petal.Length
plot(Petal.Length,type='l',lty=1.8, col=2)
lines(pred_Petal_Length,type='l', col="blue")




#create the module to predicate the Peta. length variable
mod_Petal_Width <- lm(Petal.Width~. , data = dataset)
#check the summary result of this module
summary(mod_Petal_Width)
#prediction
pred_Petal_Width <- predict(mod_Petal_Width, test)
#prediction value
pred_Petal_Width
#check the Petal.Width values only
plot(Petal.Width,type='l',lty=1.8, col=2)

#check the predicted values only
plot(pred_Petal_Width,type='l',lty=1.8, col="blue")

#comparing predicted VS actual values of Petal.Width
plot(Petal.Width,type='l',lty=1.8, col=2)
lines(pred_Petal_Width,type='l', col="blue")
