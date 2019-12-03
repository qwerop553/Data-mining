# 앙상블 기반의 의사결정 트리
# 의사결정 트리 모델에 다양성을 추가하기 위해 배깅의 기본 워닐와
# 임의의 특징 선택을 결합

# 트리 앙상블이 생성된 후 모델은 트리의 예측을 결합하기 위해 투표를 함
# 일부 샘플만 빼놓고, 트리를 생성하고, 이런 과정을 여러번 거쳐  앙상블 트리를 구성함

# 알고리즘 동작 과정
# 배깅과정에서 무작위 복원 추출을 통해 부트스트랩 데이터셋을 생성하고, 
# 각 데이터에 대해 CART를 적용

# 이때 같은 변수를 모든 모델이 사용한다면 비슷비슷한 샘플을 사용하기 때문에
# 앙상블의 효과가 덜 나타남

# 그래서 랜덤포레스트 모델은 다른 입력 변수 집합을 사용 

# 알고리즘 동작 과정
# 각 CART 모델에서 어느 정도로 학습할 것인가는 사용자 정의 부분임
# 나무의 깊이나 잎 노드의 수에 제약을 주는 것이 가능함
# 랜덤포레스트에서 해당데이터에 대해서는 full-grown tree를 생성함
# 과적합 문제가 발생할 수 있긴 하지만 랜덤포레스트의 앙상블 기능이 이 문제를 해결해줌
# 랜덤 포레스트의 최종 결과는 다수결 투표 과정을 통해 최종 클래스를 예측

credit <- read.csv("../decision_tree/german.csv")
credit$default <- as.factor(credit$default)

library(randomForest)

rf <- randomForest(default~., data=credit)
rf

# 부스팅 트리와 비교
library(caret)
ctrl <- trainControl(method = 'repeatedcv',
                     number = 10, repeats = 10)
# 변수 수 설정
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

set.seed(300)
m_rf <- train(default~., data=credit, method='rf',
              metric = "Kappa", trControl = ctrl,
              tuneGrid=grid_rf)

grid_c50 <- expand.grid(.model = "tree",
                        .trials = c(10, 20, 30, 40),
                        .winnow = 'False')

m_c50 <- train(default~., data=credit, method='C5.0',
               metric = "Kappa", trControl = ctrl,
               tuneGrid = grid_c50)
m_c50

# vote를 보면, 가까스로 이겼는지, 월등히 이겼는지 볼 수 있다
