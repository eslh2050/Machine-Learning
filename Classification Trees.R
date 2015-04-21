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

### check accuracy of the model

### prune the tree

### check accuracy of pruned tree