"군집화"

"
데이터를 클러스터로 자동분리하는 자율 머신 러닝 작업이다.
그룹이 어떻게 보이게 될지 사저에 정보가 없이 군집화가 이루어진다.
컴퓨터가 클러스터를 형성할때는 클러스터 안에 있는 아이템들은 서로 아주
비슷해야하지만, 클러스터 밖에 있는 아이템은 아주 달라야한다.

같다와 다르다를 객관화할 수 있는 유사성을 정의해야한다.
유사성을 정의는 알고리즘마다 다르다.

이번시간에 배울 것은 Euclidean Distance로 유사성을 측정한다.
"

"
주 사용: 고객 세분화
일일히 한명한명 맞춤화된 서비스를 제공할 수는 없으므로 
cluster를 구성하여 맞춤화된 서비스를 제공함

이상행동 탐지 등의 anomaly detection(변칙 탐색)
이상여부에 대한 정답지가 없을 경우에 사용
지도학습 이전에 사전 테스트 형태로도 사용한다.
"

"
K-means Algorithm

오랜 역사를 가지는 알고리즘으로 다른 정교화된 알고리즘의 토대가 됨
k개의 그룹으로 데이터를 나누는데, 그룹내 평균값을 이용하여 centroid를 잡고,
가까운 centroid를 재설정하면, 데이터를 재분배하는 과정을 반복하는 학습 과정

trial and error 방식임
"

"
문제점: 
1. k 값이 작을 때는 두 값이 합쳐지면서
평균값이 이도저도 아닌 값이 나올 수 있음 -> means 를 사용할 때 나타나는 문제

2. 군집의 크기가 클러스터링에 반영되지 않을 수 있다.

3. 군집의 밀도도 잘 반영되지 않는다

4. 데이터 분포가 특이한 케이스도 클러스터링도 잘 안된다.
"

# IRIS clustering
iris[,1:4]
irisCluster <- kmeans(iris[,1:4], 3)
irisCluster$cluster

table(irisCluster$cluster, iris$Species)


# teens 

teens <- read.csv("C:/data_mining/k_means/snsdata.csv")

str(teens)

table(teens$gender)

table(teens$gender, useNA='ifany')

summary(teens$age)

teens$age <- ifelse(teens$age >= 13 &teens$age, teens$age, NA)

summary(teens$age)

"
더미 변수

알고리즘에 따라서는 수치형 변수만을 받는 경우가 있음

예를 들면 K-means 알고리즘의 경우 거리를 계산하고,
평균을 구해서 centroid를 구하기 때문에 산술연산이 가능한
수치형 변수를 사용해야 함

명목형 변수를 수치로 변경해서 사용하게 되면

즉 나이를 노인/중년/청년으로 나타내는 변수를 2/1/0 으로 사용하게 되면
숫자가 연산이 되면서 1.5 이런 수치가 나오는데, 이 값은
의미를 가지지 못함(중년스러운 노인?)

그래서 명목형 변수는 값의 수준별로 따로따로 변수를 생성
그렇게 해서 생긴 변수는 1, 0 이 값만을 가지는데 이를 더미변수라고 함

더미변수는 연산이 가능한데, '노인'변수에서 연산을 통해 0.7이나오면
0.7 퍼센트가 노인이다 라는 의미로 사용할 수 있음
"
'
teens$female <- ifelse(teens$gender == "F", 1, 0)

teens$male <- ifelse(teens$gender == "M", 1, 0)

teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

#원하는 데로 들어갔나?

table(teens$female, useNA = 'ifany') # teens$gender == "F" 는 gender 가 NA 일때 비교 없이 NA 가 들어가버렸다.

table(teens$male, useNA = 'ifany')  # teens$gender == "M" 는  gender 가 NA 일때 비교 없이 NA 가 들어가버렸다.

table(teens$no_gender, useNA = 'ifany')
'
# 주의할것 -> ifelse 는 값 자체가 없을 때 test 가 안된다 !

teens$gender <- ifelse(is.na(teens$gender), "0", teens$gender)

teens$gender <- factor(teens$gender, levels = c("0", "1", "2"), labels = c("NA", "F", "M"))

teens$female <- ifelse(teens$gender == "F", 1, 0)

teens$male <- ifelse(teens$gender == "M", 1, 0)

teens$no_gender <- ifelse(teens$gender == "NA", 1, 0)

