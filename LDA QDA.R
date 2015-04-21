library(ISLR)
library(class)
library(ggplot2)
attach(Smarket)

train=Smarket$Year<2005
train[1:10]
test=!train

training_data=Smarket[train,-8]
testing_data=Smarket[test,-8]

training_y=Direction[test]

head(Smarket)
#KNN
Smarket_scaled <- scale(Smarket[,-c(8,9)])

response <- knn(Smarket_scaled[train,],
                Smarket_scaled[test,],
                Smarket[train,]$Direction,
                k=1)

table <- table(response, Smarket[test,]$Direction)
table

(table[1,2]+table[2,1])/sum(table)


error = NULL
for(i in 1:200){
  #set.seed(3)
  response <- knn(Smarket_scaled[train,],
                  Smarket_scaled[test,],
                  Smarket[train,]$Direction,
                  k=i)
  table <-table(response,Smarket[test,]$Direction)
  error[i] = (table[1,2]+table[2,1])/sum(table)
}


plot(error, xlab = "k", ylab = " Test Set Misclassicifation error")

min_error_rate = min(error)

k = which(error == min_error_rate)

error[error == min_error_rate]

qplot(1:200, error,xlab = "k", ylab= "Misclassicifation", geom=c("line","point"))


## LDA
library(MASS)
[,-c(8,9)]
fit <- lda(Direction~.,training_data)
response <- predict(fit,testing_data)
table <- table(response$class, testing_data$Direction)
table

##QDA

fit2 <- qda(Direction~.,training_data)
response <- predict(fit2,testing_data)
table <- table(response$class, testing_data$Direction)
table
(table[1,2]+table[2,1])/sum(table)
library(scales)
percent((table[1,2]+table[2,1])/sum(table))









