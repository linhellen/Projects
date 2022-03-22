#Set working directory (PC)
setwd("C:/Users/Hellen/Desktop/STAT FINAL")

##Packages
library(readxl)
library(readr)
library(qualityTools)
library(ggplot2)
library(dplyr)
library(DescTools)

#Import Excel File
#df = read_excel("Long_Term_Occupational_Projections.xls")

#Import CSV File
#Reading data for project into R
df = read_csv("Long_Term_Occupational_Projections.csv")

#Remove all columns but one
#Let's only look at the Replacement Annual Openings for 2018 - 2028
#Drop all other years except for 2018
df2 <- df[-c(798:6629),]

#This is the same as keeping the rows you want
df2 = df[c(1:797),]

#Drop all other variables except Replacement Annual Openings

df3 = df2[-c(1:10)]

#This is the same as keeping the columns you want
df3 = df2[c(11)]

#Finding mean
#Find the Mean of the Replacement Annual Openings 
mean = Mean(df3$'Replacement Annual Openings')

#Finding the Median of the Replacement Annual Openings
med = Median(df3$'Replacement Annual Openings')

#Finding the Mode of the Replacement Annual Openings
md = Mode(df3$`Replacement Annual Openings`)

#Finding the Range of the Replacement Annual Openings
ran = max(df3, na.rm = TRUE) - min(df3, na.rm = TRUE)

#Finding standard deviation
#Writing a function (S) to find the SD of Replacement Annual Openings 
N = length(df3$'Replacement Annual Openings')
S = sqrt(sum((df3$'Replacement Annual Openings' - mean(df3$'Replacement Annual Openings'))^2)/(N - 1)); S

#Find the SD (note this gives you the adjusted SD) (S)
sd = sd(df3$'Replacement Annual Openings')

#Writing a function (S^2) to find the variance of Replacement Annual Openings 
N = length(df3$'Replacement Annual Openings')
S_sq= sum((df3$'Replacement Annual Openings' - mean(df3$'Replacement Annual Openings'))^2)/(N - 1); S_sq

#Find the variance (note this gives you the adjusted variance)
var = var(df3$'Replacement Annual Openings')

#Find the UNADJUSTED SD 
sigma = sqrt(sum((df3$'Replacement Annual Openings' - mean(df3$'Replacement Annual Openings'))^2)/(N))
sigma

#Find the UNADJUSTED Variance
sigma_sq = sum((df3$'Replacement Annual Openings' - mean(df3$'Replacement Annual Openings'))^2)/(N)
sigma_sq

# Types of graphs 
#Graph 1
#Histogram Plot 
#hist(df3$'Replacement Annual Openings')
hist(df3$'Replacement Annual Openings', main="Replacement Annual Openings in 2018 - 2028 in NYS", 
     xlab= "Replacement Annual Openings", ylab = "Frequency", col="lightblue")

#Graph 2
#Stem and Leaf Plot
stem(df3$'Replacement Annual Openings')

#Graph 3
#Dot plot
dotchart(df3$'Replacement Annual Openings', stacked = TRUE, pch = 20, main = "Replacement Annual Openings in 2018 - 2028 in NYS", 
         xlab="Replacement Annual Openings", ylab="Frequency", col= "purple")

#To compose your reduced target population you might want to look at the data summary
summary(df3$'Replacement Annual Openings')

####################################
###############REDUCED##############
####################################
#To create a reduced target population of size N=8
df4 = data.frame(c(20, 700.0, 10680.0, 22490.0, 1950.0, 1236750.0, 3990.0, 23050.0))

as.numeric(df4)

#To rename the column
names(df4)[1] <- "Reduced Replacement Annual Openings"

#Finding mean for reduced
#Find the Mean of the Reduced Replacement Annual Openings 
mean2 = mean(df4$`Reduced Replacement Annual Openings`)
mean2

#Finding the Median of the Reduced Replacement Annual Openings
med2 = median(df4$`Reduced Replacement Annual Openings`)

#Finding the Mode of the Reduced Replacement Annual Openings
md2 = Mode(df4$`Reduced Replacement Annual Openings`)

#Finding the Range of the Reduced Replacement Annual Openings
ran2 = max(df4, na.rm = TRUE) - min(df4, na.rm = TRUE)
#Types of Graphs for Reduced

#Finding standard deviation
#Writing a function (S) to find the SD of Replacement Annual Openings 
N2 = length(df4$`Reduced Replacement Annual Openings`)
S2 = sqrt(sum((df4$`Reduced Replacement Annual Openings` - mean(df4$`Reduced Replacement Annual Openings`))^2)/(N2 - 1)); S

#Find the SD (note this gives you the adjusted SD) (S)
sd2 = sd(df4$`Reduced Replacement Annual Openings`)

#Writing a function (S^2) to find the variance of Replacement Annual Openings 
S_sq2 = sum((df4$`Reduced Replacement Annual Openings` - mean(df4$`Reduced Replacement Annual Openings`))^2)/(N2 - 1); S_sq2

#Find the variance (note this gives you the adjusted variance)
var2 = var(df4$`Reduced Replacement Annual Openings`)

#Find the UNADJUSTED SD and Variance
sigma2 = sqrt(sum((df4$`Reduced Replacement Annual Openings` - mean(df4$`Reduced Replacement Annual Openings`))^2)/(N2))
sigma2

sigma_sq2 = sum((df4$`Reduced Replacement Annual Openings` - mean(df4$`Reduced Replacement Annual Openings`))^2)/(N2)
sigma_sq2


#Reduced Graph 1
#Histogram Plot
hist(df4$`Reduced Replacement Annual Openings`, main="Reduced Replacement Annual Openings in 2018 - 2028 in NYS", 
     xlab="Reduced Replacement Annual Openings", ylab = "Frequency", col="red")

#Reduced Graph 2
#Stem and Leaf Plot
stem(df4$'Replacement Annual Openings')

#Reduced Graph 3
#Dot plot
dotchart(df4$`Reduced Replacement Annual Openings`, pch = 20, main = "Reduced Replacement Annual Openings in 2018 - 2028 in NYS", 
         xlab="Reduced Replacement Annual Openings", ylab="Frequency", col= "purple")

####################################
#######SAMPLING DISTRIBUTION########
####################################


#To Create a sampling distribution of size n = 3
df5 <- t(combn(df4$`Reduced Replacement Annual Openings`,3)); df5

#Convert to a data frame
sampl_distr = data.frame(df5)

as.numeric(df5)

#To rename the column
names(sampl_distr)[1] <- "Sampling Distribution"

#Calculate your sample means for each sample to fully create your sampling distribution
sampl_distr$mean = rowMeans(sampl_distr, na.rm=TRUE)


#Frequency Histogram of the Sampling Distribution of the Sample Mean
hist(sampl_distr$`Sampling Distribution`, main = "Sampling Distribution" , xlab = 'Sampling Distribution Mean' , ylab = "Frequency" , col = "pink")

#Finding mean
#Find the Mean of the Replacement Annual Openings 
mean3 = Mean(sampl_distr$`Sampling Distribution`)

#Finding the Median of the Replacement Annual Openings
med3 = Median(sampl_distr$`Sampling Distribution`)

#Finding the Mode of the Replacement Annual Openings
md3 = Mode(sampl_distr$`Sampling Distribution`)

#Finding the Range of the Replacement Annual Openings
ran3 = max(sampl_distr$`Sampling Distribution`, na.rm = TRUE) - min(sampl_distr$`Sampling Distribution`, na.rm = TRUE)

#Finding standard deviation
#Writing a function (S) to find the SD of Replacement Annual Openings 
N3 = length(sampl_distr$`Sampling Distribution`)
S3 = sqrt(sum((sampl_distr$`Sampling Distribution` - mean(sampl_distr$`Sampling Distribution`))^2)/(N3 - 1)); S

#Find the SD (note this gives you the adjusted SD) (S)
sd3 = sd(sampl_distr$`Sampling Distribution`)

#Writing a function (S^2) to find the variance of Replacement Annual Openings 
N3 = length(sampl_distr$`Sampling Distribution`)
S_sq3= sum((sampl_distr$`Sampling Distribution` - mean(sampl_distr$`Sampling Distribution`))^2)/(N3 - 1); S_sq

#Find the variance (note this gives you the adjusted variance)
var3 = var(sampl_distr$`Sampling Distribution`)

#Find the UNADJUSTED SD 
sigma3 = sqrt(sum((sampl_distr$`Sampling Distribution` - mean(sampl_distr$`Sampling Distribution`))^2)/(N3))
sigma3

#Find the UNADJUSTED Variance
sigma_sq3 = sum((sampl_distr$`Sampling Distribution` - mean(sampl_distr$`Sampling Distribution`))^2)/(N3)
sigma_sq3


