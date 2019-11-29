library(caret)
library(e1071)



ctrl <- trainControl(method = 'cv', number = 10,
                     selectionFunction = 'oneSE')

## trainControl(selectFunction= ...)
# oneSE 최고 성능의 1 표준 오차 내에 가장 단순한 후보를 선택
# best 최고의 성능을 갖는 후보를 선택

# use expand.grid() to create grid of tuning parameters
grid <- expand.grid(.model = 'tree',
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    .winnow = "FALSE")

# winnow = 사용할 변수를 미리 정해 놓겠다
# 변수를 다 사용할 건지, 일부만 사용할 것인지
# FALSE = 너가 알아서 변수를 골라서 사용해라

set.seed(300)
credit <- read.csv("../decision_tree/german.csv")
credit$default <- factor(credit$default)
m <- train(default~., data=credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)

m

