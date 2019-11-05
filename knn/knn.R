# Instance based Learning
#
# 새로운 데이터가 주어졌을 때 가장 가까운 k개 이웃의 정보로 새로운 데이터를 예측한다
#
# 사실 KNN은 학습이라고 할 만한 절차가 없다.
# 새로운 데이터가 생기면, 기존 데이터에서 이웃들을 뽑아 새로운 데이터를 분류한다.
#
# KNN을 모델로 별도로 구축하지 않는다는 뜻으로 게으른 모델이라고 부른다.
# 또는 Instance-based Learning이라고도 한다.
#
# Instance-based Learning 반의어 : model-based learning (모델을 구축함)
#
#


a <- matrix(c(8, 5, 3, 7, 3, 6, 7, 3), 4, 2, byrow=T)

knn_distance <- function(x, y){
    sqrt((a[,1] - x)^2 + (a[,2] - y)^2) # R Style Coding
}

knn_distance(6, 4)

# knn 의 장단점 
# k = 1인 경우, 나랑 가장 가까운 한명을 찾아서 나를 평가하겠다
# k = 1로 잡으면 못해도 에러율이 가장 이상적인 에러율의 두 배는 넘지 않는다.
#
# 데이터가 서로 너무 이질적인 경우에
# ex) 사람마다 굉장히 달라서 패턴을 찾기가 어려운데,
# knn을 돌리면 잘 돌아가는 경향이 있다
# 이 많은 사람들 중에서 같이 묶이진 않지만, 가장 가까운 사람을 보는 것은 괜찮다
# 1 nn 모델에 한해서는, 성능이 어느 정도는 보장된다.
#
# 최적 이웃의 수 k 를 어떻게 설정하는가?
# 거리척도를 어떻게 사용할 것인가?
# 
# knn 모델은 아무것도 안하고 있다가,
# 새로운 데이터가 닥치면 그 때 답을 내므로, 
# 답을 내는데까지 시간이 많이 거린다
#
# 즉각즉각 답을 내야 하는 경우에 관해서는 별로일 수 있다
#
##  K-NN의 특징
# 
# K-NN의 하이퍼파라메터는 탐색할 이웃 수(k) 와 이웃을 결정할 거리지표
# 지나치게 작은 k => 지나치게 데이터의 지역적 특성 반영
# 너무 큰 k => 부정확한 분류
#
##  Best K 찾기
# Greedy Algorithm 으로 적절 k를 찾음
#
# k를 순차적으로 증가시키면서 오류율을 점검
#
## 분류 방법
# K개의 이웃 중 어떤 class로 결정할 것인가?
# 다수결(majority voting): 이웃 데이터에서 다수결로 결정
# 즉 많은 클래스로 할당함
#
# 가중합(weighted voting)
# 거리가 가까운 이웃에 좀 더 가중치를 주어 class를 결정
# 이웃의 정보에 좀 더 가중치를 줌
# 1/(1+x), 1/(1 + x^2), exp(-x)등 x에 따라 거리가 줄어드는 방법을 사용
#
curve(1/(1+x))
curve(1/(1+x^2))
curve(exp(-x))
#
## Voting의 문제점
# 단순히 이웃의 class중 다수결로 결정하거나, 또는 거리에 따른 가중치를 준다고 하더라도
# 데이터가 근본적으로 class의 비중이 다르면 단순히 많은 쪽의 class를 선정하는 것이 옳은가?
# 예를 들어 제품의 불량을 탐지하고자 하면 양품 vs 불량의 비율은 불량이 훨씬 적을 것이다.
# 이 둘의 비율이 9:1 이라고하면
# 5-NN으로 5개를 선택했을 때 양품이 3, 불량이 2로 나왔다.
# 그러면 이를 이웃으로 갖는 제품은 양품인가? 불량인가?
#
## 정규화
# 거리기반 알고리즘은 변수의 정규화가 필수적임
# Km의 거리와 m의 거리는 스케일이 다르다. 이 경우 단위를 맞춰줘야 하는 것은 기본
# 
# 변수의 정규화 (feature scaling)
# X' = (X - Xmin) / (Xmax - Xmin)
# Xs = (X - mean) / (sd) 
#
#
## 거리
# Euclidean Distance
# Manhattan Distance
# Mahalanobis Distance : 변수 내 분산, 변수 간 공분산을 모두 반영하여 거리를 계산
# 두 변수의 상관관계를 고려하여 거리를 계산
#
# in R
# mahalanobis(x, center, cov, inverted = FALSE, ...)
# x : data
# center : mean vector
# cov : covariance matrix

x <- matrix(rnorm(100 * 2), ncol = 2)
plot(x)

Sx <- cov(x)
D2 <- mahalanobis(x, colMeans(x), Sx) # 센터로부터의 거리

y <- c(3, 4)
D <- mahalanobis(x, y, cov(x))