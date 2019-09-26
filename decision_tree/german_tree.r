setwd("..")
credit <- read.csv("german.csv")

head(credit)

names(credit)[1] <- "checking_balance"
table(credit$checking_balance)

table(credit$default)
prop.table(table(credit$default))

set.seed(123) # 같은 PC에서 동일한 난수를 생성
train_sample <- sample(1000, 900)

str(train_sample)

train <- credit[train_sample,]
test <- credit[-train_sample,]

# test 셋과 train 셋에서의 목표변수 default의 비율확인
prop.table(table(train$default))
prop.table(table(test$default))

library(C50) 
library(dplyr)
m <- C5.0(train %>% select(-default), as.factor(train$default)) # 의사 결정 변수는 제외해야 한다.

summary(m)


