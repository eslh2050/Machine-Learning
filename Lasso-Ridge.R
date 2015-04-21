### this code is to run ridge and lasso 
### regression in R 

library(ISLR)

## needed to run ridge and lasso function
library(glmnet)


## glmnet needs 4 arguments
## x: matrix
## y = vector 
## alpha = {0= ridge,1 =lasso}
## lambda = a set of lambda values


### prepare the data

x = model.matrix(Salary~., Hitters)
x = x[,-1] #get rid of the intercept col

## model.matrix transofrms the data
# into a matrix, and it deletes obs
# with missing values, and it changes
# categorical variables into dummy variables

y = na.omit(Hitters$Salary)

# split the dataset into training and testing
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = -train

training_x = x[train,]
testing_x = x[test,]

training_y = y[train]
testing_y = y[test]

## define lambda

lambda_grid = seq(0, 10^10, length = 100)

####### Ridge Regression

ridge_model = glmnet(training_x, 
                     training_y, 
                     alpha =0)


plot(ridge_model, xvar = "lambda")

### we expect that as lambda increases
### the coeficients are more shrunk

ridge_model$lambda[50]
ridge_model$lambda[60]

## compare the coeficients for both lambdas
coef(ridge_model)[,50]
coef(ridge_model)[,60]

### choose the best value of lambda that would 
## minimize the error. Run cross validation

set.seed(1)
cv_error = cv.glmnet(training_x, 
                     training_y, 
                     alpha = 0)
best_lambda = cv_error$lambda.min

best_lambda

### OUR FINAL RIDGE 
model_coef = predict(ridge_model, 
                     type = "coefficients",
                     s= best_lambda)

### test the model 

predicted_y = predict(ridge_model, 
                      s= best_lambda,
                      newx = testing_x)
### MSE
mean((predicted_y - testing_y)^2)


##### LASSO 

lasso_model = glmnet(training_x, 
                     training_y, 
                     alpha =1)


plot(lasso_model, xvar = "lambda")


set.seed(1)
cv_error = cv.glmnet(training_x, 
                     training_y, 
                     alpha = 1)
best_lambda = cv_error$lambda.min

best_lambda

### OUR FINAL RIDGE 
model_coef = predict(lasso_model, 
                     type = "coefficients",
                     s= best_lambda)

### test the model 

predicted_y = predict(lasso_model, 
                      s= best_lambda,
                      newx = testing_x)
### MSE
mean((predicted_y - testing_y)^2)


### linear regression 

linear_model = lm(Salary~., data = na.omit(Hitters[train,]))

predicted_y = predict(linear_model, na.omit(Hitters[test,]))

testing_y = na.omit(Hitters$Salary[test])

mean((predicted_y - testing_y)^2)
