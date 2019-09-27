setwd("C:/data_mining/decision_tree")
credit <- read.csv("german.csv")

head(credit)

names(credit)[1] <- "checking_bala/nce"
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

plot(m)

# 모델의 일반화 능력 predict(모델, 테스트데이터)
test$p <- predict(m, test)
# 또는
# p <- predict(m, test)
# test <- cbind(test, p)

cf.mat <- table(test$default, test$p) 

# 모델 성능 평가 

# 데이터에 모델을 붙일 때 사용하는 패키지,gmodels
library(gmodels)

CrossTable(test$default, test$p)
# 해석: 
# 1번째: count값
# 2번째: x-square 값
# 3번째: 행백분률  
# 4번째: 컬럼백분률
# 5번째: 다 더해서 백이 되는 백분율
 
# 쉽게 precision, recall, accuracy 계산할 수 있음


# 다른 것 보다 더 비싼 실수
# 채무 불이행할 사람에게 돈을 빌려줬을 때 발생하는 손실이
# 돈을 갚을 사람에게 돈을 빌려주지 않았을 때 발생하는 손실보다 훨씬 더 크다.

# 비용을 정의하여 준다
#
#          정답_no     정답_yes
# 예측_no      0             4
# 예측_yes     1             0
#

matrix_dimensions <- list(c("no", "yes"), c("no", "yes")); names(matrix_dimensions) <- c("predicted", "actual");
cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
cost

# 간단하게
cost <- matrix(c(0, 1, 4, 0), nrow=2); 
colnames(cost) <- rownames(cost) <- c("no", "yes");

cost_model <- C5.0(train[-21], as.factor(train$default), costs=cost)
summary(cost_model)


