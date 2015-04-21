library(ISLR)

?Hitters
attach(Hitters)

### check for missing Salary Values. 
### There are 59 missing salaries
sum(is.na(Salary))

Hitters = na.omit(Hitters)
attach(Hitters)
dim(Hitters)

### the observations with missing values are deleted

### Use subset selection to decide what 
### variables should be in our model 

### Use "leaps" package to run subset selection
library(leaps)

model = regsubsets(Salary~., data = Hitters)
summary(model)

### the "*" in the output refers that the variable is included in the model
### notice that regsubsets() gives you the first 8 models
### with 8 variables, but we can change that as follows:

model = regsubsets(Salary~., data = Hitters, nvmax = 19)
model_summary = summary(model)

names(model_summary)

model_summary$adjr2
max(model_summary$adjr2)

### plot in order to find which model is best based on adjusted 
### r-squared

par(mfrow= c(1,2))
plot(1:19, 
     model_summary$adjr2, 
     xlab = "Number of Variables", 
     ylab = "Adjusted R-sqaured", 
     type = "l")

index = which.max(model_summary$adjr2)

points(index,
       model_summary$adjr2[index], 
       col = "red", cex = 2, pch = 4)

abline(v=index, col = "blue")


### CP 

plot(1:19, 
     model_summary$cp, 
     xlab = "Number of Variables", 
     ylab = "CP", 
     type = "l")

index = which.min(model_summary$cp)

points(index,
       model_summary$cp[index], 
       col = "red", cex = 2, pch = 4)

abline(v=index, col = "blue")

par(mfrow= c(1,1))







