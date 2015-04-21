library(MASS)
library(tree)
attach(Boston)

### split data into training and testing 
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
test = - train

training_data = Boston[train,]
testing_data = Boston[test,]
testing_y = medv[test]

### fit tree model for training data
tree_model = tree(medv~., training_data)
plot(tree_model)
text(tree_model)

### check accuracy of the model
predicted_y = predict(tree_model, testing_data)
mean((predicted_y - testing_y)^2)

### prune the tree

# perform cross validation for prunning tree
cv_tree = cv.tree(tree_model)
names(cv_tree)
plot(cv_tree$size, 
     cv_tree$dev, 
     xlab = "Tree Size",
     ylab= "CV MSE",
     type = "b")
index_min = which.min(cv_tree$dev)
tree_size = cv_tree$size[index_min]
min_CV_MSE = cv_tree$dev[index_min]
points(tree_size, min_CV_MSE, col = "red", pch = 20, cex =4)

# prunned tree
pruned_model = prune.tree(tree_model, best = 7)
plot(pruned_model)
text(pruned_model)
### check accuracy of pruned tree
predicted_y = predict(pruned_model, testing_data)
mean((predicted_y - testing_y)^2)
