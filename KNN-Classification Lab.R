### In this file, we will use a dataset called
## Caravan. This dataset is in ISLR library.

## load library
library(ISLR)

## In order to run KNN methods, then you have 
## to load "class" package
library(class)

### Let's explore Caravan
?Caravan
dim(Caravan)
num_of_obs = dim(Caravan)[1]

### Let's explore Purchase

purchase = Caravan[,86]
summary(purchase)

### The proportion of people who bought the insurance

384/num_of_obs

### The KNN function takes 4 arguments: 
### 1- Training data set (without the y variable)
### 2- Testing dataset (without the y variable)
### 3- Training dataset y variable
### 4- K, the number of neighbors


### checkf for the variances of the variables

var1 = var(Caravan[,1])
var1

var2 = var(Caravan[,2])
var2

### The variances are very different, so let's
### standardize 

Standardized_Caravan = scale(Caravan[,-86])
var(Standardized_Caravan[,1])
var(Standardized_Caravan[,2])

### STEP 1:  Split the data into training and testing

train = 1:4822
train = sample(1:5822,size = 4822)
train_data = Standardized_Caravan[train,]
train_purchase = Caravan[train, 86]

test = -train ### anything but the training data
test_data = Standardized_Caravan[test, ]
test_purchase = Caravan[test, 86]

### STEP 2: Fit the KNN model using the knn()

### To control randomness, we can use 
### the set.seed()


set.seed(3)
predicted_purchase = knn(train_data, 
                         test_data, 
                         train_purchase, 
                         k = 1)


### STEP 3: Asses the model, and get the error rate
### by comparing the predicted_purchase to 
### testing_purchase
mean(predicted_purchase != test_purchase)

### The error rate is 11.5%, which is low.

### change K and see what happens to the error rate
set.seed(3)
predicted_purchase = knn(train_data, 
                         test_data, 
                         train_purchase, 
                         k = 55)


### STEP 3: Asses the model, and get the error rate
### by comparing the predicted_purchase to 
### testing_purchase
mean(predicted_purchase != test_purchase)

### Use a for loop to check what would be the 
### error rate for models with k = 1,..,100






a = Sys.time()

error = NULL
for(i in 1:30){
#set.seed(3)
predicted_purchase = knn(train_data, 
                         test_data, 
                         train_purchase, 
                         k = i)
error[i] = mean(predicted_purchase != test_purchase)
}

b = a = Sys.time()

b-a

plot(error, xlab = "k", ylab = " Test Set Misclassicifation error")

min_error_rate = min(error)

k = which(error == min_error_rate)

error[error == min_error_rate]

library(ggplot2)
qplot(1:30, error)
