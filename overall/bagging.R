library(adabag)
library(ipred) # Improved Predictors
library(caret)
library(tidyverse)
library(C50)

set.seed(300)

credit <- read.csv("../decision_tree/german.csv")
credit$default <- factor(credit$default)
random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:500],]
credit_validate <- credit[random_ids[501:750],]
credit_test <- credit[random_ids[751:1000],]

mybag <- bagging(default~., data = credit_train, nbagg = 25)

credit_pred <- predict(mybag, credit_train)
head(credit_pred)

table(credit_pred, credit_train$default)

set.seed(300)
credit_pred <- predict(mybag, credit_test)
table(credit_pred, credit_test$default)

m <- train(default~., data=credit_train, method='treebag')
m

table(credit_test$default, predict(m, credit_test))

m_c50_bst <- C5.0(default~., data=credit_train, trials=100)
summary(m_c50_bst)

table(credit_test$default, predict(m_c50_bst, credit_test))

m_adaboost <- boosting(default~., data=credit_train)
p_adaboost <- predict(m_adaboost, credit)
head(p_adaboost$class)
p_adaboost$confusion

p_adaboost$error
