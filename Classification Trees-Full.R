library(ISLR)
library(tree)

attach(Carseats)

### data manipulation
?Carseats # the dataset is to predict the Sales of child car seats in 400 locations
head(Carseats) # we can see that Sales is numerical variable, so we might need to create a categorical variable for the sake of classification

range(Sales) #Sales range from 0 to 16
High = ifelse(Sales >=8, "Yes", "No") # create a categorical variables bases on Sales
Carseats = data.frame(Carseats, High) # appends High to Carseat dataset, and now our dataset is ready!

### split data into training and testing 
set.seed(2)
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
test = -train
training_data = Carseats[train,]
testing_data = Carseats[test, ]
testing_High = High[test]

### fit tree model for training data
tree_model = tree(High~.-Sales, training_data)

plot(tree_model) ### plot tree
text(tree_model, pretty = 0) ### add variable names to the tree
### check accuracy of the model

tree_pred = predict(tree_model, testing_data, type = "class")
table(tree_pred, testing_High)
mean(tree_pred != testing_High) # 28.5%

### prune the tree

set.seed(3)
cv_tree = cv.tree(tree_model, FUN = prune.misclass)
names(cv_tree) #size is the number of terminal nodes, an dev is the cross validation error rate

plot(cv_tree$size, cv_tree$dev, 
     xlab = "Tree Size", 
     ylab = "CV Error Rate", 
     type = "b")
index_min = which.min(cv_tree$dev)
min_cv_rate = cv_tree$dev[index_min]
tree_size = cv_tree$size[index_min]
points(tree_size, min_cv_rate, col ="red", pch = 20, cex = 4)

## prune the tree using the tree_size

pruned_model = prune.misclass(tree_model, best = tree_size)
plot(pruned_model)
text(pruned_model, pretty =0)

### check accuracy of pruned tree

tree_pred = predict(pruned_model, testing_data, type = "class")
table(tree_pred, testing_High)
mean(tree_pred !=testing_High) ## 23%