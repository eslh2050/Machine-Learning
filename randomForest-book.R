

library(randomForest)
set.seed(1)
bag.boston = randomForest(medv~., data = Boston, mtry= 13, importance = T)
bag.boston
yhat.bag = predict (bag.boston , newdata=Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(yhat.bag , boston.test)
abline (0,1)
mean((yhat.bag -boston.test)^2)

## random forest
set.seed(1)
rf.boston = randomForest(medv~., data = Boston, mtry= 4, importance = T)
rf.boston
rf.boston$importance
barplot(sort(bag.boston$importance[,2], decreasing = T))
varImpPlot (rf.boston)
