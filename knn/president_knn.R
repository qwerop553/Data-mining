setwd("C:/data_mining/knn")
# install.packages(c("caret", "e1071"))
library(caret)
library(e1071)

set.seed(123)
president <- read.csv("US Presidential Data.csv")
head(president$Win.Loss)

president$Win.Loss <- ifelse(president$Win.Loss==1, "win", "loss")
index = createDataPartition(president$Win.Loss, p = 0.7, list = F)
train = president[index,]
validation = president[-index,]

# 모델 설정
# 10-fold cross validation, 반복 횟수등을 설정함
x = trainControl(method = 'repeatedcv',
                number = 10,
                
                repeats = 10,
                classProbs = TRUE,
                summaryFunction = twoClassSummary)

model <- caret::train(Win.Loss~., data=train, method='knn',
                                  # 정규화 하는 옵션 
                                  # center: 평균을 빼주고, 
                                  # scale: 표준편차로 나눠줌
                                  preProcess=c("center", "scale"),
                                  
                                  trControl = x, metric = "ROC", tuneLength = 10)

model

plot(model)

valid.pred <- predict(model, validation, type='prob')
valid.pred

table(apply(valid.pred, 1, which.max), validation$Win.Loss)

model <- caret::train(Win.Loss~., data=train, method='adaboost',
                                  # 정규화 하는 옵션 
                                  # center: 평균을 빼주고, 
                                  # scale: 표준편차로 나눠줌
                                  preProcess=c("center", "scale"),
                                  
                                  trControl = x, metric = "ROC", tuneLength = 10)

model

plot(model)