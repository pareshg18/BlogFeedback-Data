install.packages("glmnet")
install.packages("caTools")
install.packages("dplyr")
install.packages("caret")

library(glmnet)
library(caTools)
library(dplyr)
library(caret)

#Reading in the train and test datasets
blogData_train <- read.csv("blogData_train.csv",header = FALSE)
blogData_test <- read.csv("blogData_test.csv",header = FALSE)


#defining a range for lambda
lambda <- 10^seq(10,-2,length = 100)

#Dividing the training set into label and feature variables
x_train <- model.matrix(V281 ~., blogData_train)[,-1]
y_train <- blogData_train$V281

#Dividing the testing set into label and feature variables 
x_test <- model.matrix(V281 ~., blogData_test)[,-1]
y_test <- blogData_test$V281


#Least square Method 
lm_model <- lm(V281 ~ ., data = blogData_train)

pred_lm <- predict(lm_model, newdata = blogData_test)

MSE_lm <- mean((pred_lm - blogData_test$V281)^2) 
RMSE_lm <- sqrt(MSE_lm)

#Ridge regression model 

ridge_model <- glmnet(x_train,y_train,alpha = 0 ,lambda = lambda)

#Cross-validating to find best lambda value
ridge_model_cv = cv.glmnet(x_train, y_train, alpha = 0) 

#Selecting lamda that minimizes training MSE
best_lambda = ridge_model_cv$lambda.min


#Use best lambda to predict test data
ridge_pred = predict(ridge_model, s = best_lambda, newx = x_test ) 

#RMSE value for test data
MSE_ridge <- mean((ridge_pred - y_test)^2) 
RMSE_ridge <- sqrt(MSE_ridge)



#Lasso regression model

lasso_model = glmnet(x_train, y_train,alpha = 1,lambda = lambda) 

#Cross validating to find best lambda value
lasso_model_cv = cv.glmnet(x_train, y_train, alpha = 1) 

#Selecting lamda that minimizes training MSE
best_lam = lasso_model_cv$lambda.min 

#Use best lambda to predict test data
lasso_pred = predict(lasso_model, s = best_lam, newx = x_test) 

#RMSE value for test data   
MSE_lasso <- mean((lasso_pred - y_test)^2) 
RMSE_lasso <- sqrt(MSE_lasso)


# Display coefficients using lambda chosen by CV
lasso_coef = predict(lasso_model, type = "coefficients", s = best_lam)[1:281,]
lasso_coef
x  <- as.data.frame(lasso_coef[lasso_coef != 0]) 
y <- abs(x) 

y$features <- row.names(y)
colnames(y) <- c("Coeff. value","features")

y <- arrange(y,desc(y$`Coeff. value`))
y
