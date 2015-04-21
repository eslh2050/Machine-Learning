library(MASS)
attach(Boston)


### split data into training and testing 
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
test = - train

training_data = Boston[train,]
testing_data = Boston[test,]
testing_medv = medv[test]

### fit tree model for training data

### check accuracy of the model

### prune the tree

### check accuracy of pruned tree