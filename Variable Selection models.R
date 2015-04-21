# Variable Selection models


# Task: Predict life expectancy from the rest of the variables in data set states

library(faraway)
data(state)
statedata <- data.frame(state.x77,row.names=state.abb)
g <- lm(Life.Exp ~ ., data=statedata)
summary(g)
#Remove predictors with the largest p-value:
g <- update(g, . ~ . - Area)
summary(g)
g <- update(g, . ~ . - Illiteracy)
summary(g)
g <- update(g, . ~ . - Income)
summary(g)
g <- update(g, . ~ . - Population)
summary(g)
#It is important to understand that the variables omitted from the model may still be related to teh response. For example:
summary(lm(Life.Exp ~ Illiteracy+Murder+Frost, statedata))
g <- lm(Life.Exp ~ ., data=statedata)
step(g)

# Select model with the smallest residual sum of squares 

library(leaps)
b<-regsubsets(Life.Exp~.,data=statedata)
(rs <- summary(b))
plot(2:8,rs$cp,xlab="No. of Parameters",ylab="Cp Statistic")
abline(0,1)
plot(2:8,rs$adjr2,xlab="No. of Parameters",ylab="Adjusted R-square")
h <- lm.influence(g)$hat
names(h) <- state.abb
rev(sort(h))
b<-regsubsets(Life.Exp~.,data=statedata,subset=(state.abb!="AK"))
rs <- summary(b)
rs$which[which.max(rs$adjr),]
stripchart(data.frame(scale(statedata)),vertical=TRUE,method="jitter")
b<-regsubsets(Life.Exp~log(Population)+Income+Illiteracy+Murder+HS.Grad+Frost+log(Area),statedata)
rs <- summary(b)
rs$which[which.max(rs$adjr),]
