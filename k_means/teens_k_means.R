# SNS를 이용한 십대 시장 세분화
# 십대 SNS 페이지의 텍스트를 이요하여 스포츠, 종교, 음악과 같은 공통 관심을 공유하는 그룹을 찾을 수 있음
# 데이터 수집: 데이터는 SNS 사이트를 크롤링해서 가져옴
# 데이터 탐색 및 준비
library(ggplot2)
library(tidyverse)

teens <- read.csv("C:/data_mining/k_means/snsdata.csv")

str(teens)

table(teens$gender)

table(teens$gender, useNA='ifany') # NA 도 집계

summary(teens$age) # summary는 원래 집계

teens$age <- ifelse(teens$age >= 13 &teens$age < 20, teens$age, NA)

summary(teens$age)


## 더미 변수
#
# 알고리즘에 따라서는 수치형 변수만을 받는 경우가 있음
#
# 예를 들면 K-means 알고리즘의 경우 거리를 계산하고,
# 평균을 구해서 centroid를 구하기 때문에 산술연산이 가능한
# 수치형 변수를 사용해야 함
# 
# 명목형 변수를 수치로 변경해서 할 때를 가정해 보자
#
# 남성을 0, 여성을 1, NA인 성별을 2로 바꿔서 사용한다면,
# 최종 cluster의 결과로 1.7 등의 값이 나왔을 때
# 적절히 해석하기가 어렵다
#
# 그래서 위와 같은 명목형 변수는 값의 수준별로 새로운 변수를 생성한다. 
# 컬럼 두 개를 새로 만들어서,
# 첫 번째 컬럼은 남성일 때 0, 여성일 때 1 등의 처리를 해준다
#
# 이렇게 1, 0  값만을 가지는 이런 변수들을 더미변수라고 한다
#
# 더미변수는 해석이 가능한데, cluster의 남성값이 0.7이 나온다면
# 이 클러스터에는 남성이 70% 속해 있다고 해석할 수 있다.
# 더미 변수를 생성해 보자

teens$female <- ifelse(!is.na(teens$gender) & teens$gender == "F", 1, 0)
teens$male <- ifelse(!is.na(teens$gender) & teens$gender == "M", 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

##
# 주의할것 -> ifelse 는 값 자체가 없을 때 test 가 안된다 !
# teens$gender <- ifelse(teens$gender == "F" , ..) 를 사용하면
# NA 값이 있는 곳에서는 그냥 NA 값이 들어간다.

# 확인 
table(teens$female);table(teens$male);table(teens$no_gender);
2724 + 5222 == 7946

# 연령의 평균, 17.25243 세
mean(teens$age, na.rm = T)

# 성별의 분포 
g <- ggplot(teens, aes(age))
g + geom_density(aes(fill=gender), color=NA, width = 0.5, alpha=0.3) 

teens$age_fl <- floor(teens$age)
table(teens$age_fl, teens$gender)
table(teens$age_fl, teens$gender, useNA='ifany')


#
# 연령 결측치 처리
# 졸엽년도가 같은 사람들의 나이로 대체
#
# gradyear 별로 age의 mean 값을 구한다.
#

# 졸업 연도의 결측치 확인
table(teens$gradyear, useNA='ifany') # 없음

# 졸업 연도별 평균나이
teens %>% group_by(gradyear) %>% summarize(me=mean(age, na.rm=T), sd=sd(age, na.rm=T))
aggregate(age~gradyear, teens, FUN = function(x) c(mean(x, na.rm=T), sd(x, na.rm=T)))
aggregate(age~gradyear, teens, mean, na.rm=T)
aggregate(age~gradyear, teens, mean) # na.rm=T 가 default로 들어가 있다

## ave
# ave: 그룹의 대표값을 형성하는 함수
# ave(그룹의 대표값을 구할 특성, 그룹, 대표값을 어떤 통계로 할건지)
# 원본데이터의 차원을 그대로 유지
# 즉 age가 1000개의 값을 들어오면 ave_age도 1000개의 값을 생성
# ave(teens$age, teens$gradyear, FUN=function(x) mean(x, na.rm = TRUE))

# 연령 결측치 처리코드
table(teens$age, useNA='ifany')
ave_age <- ave(teens$age, teens$gradyear, FUN=function(x) mean(x, na.rm = TRUE)) 

# mean의 옵션을 주기 위해 FUN 인자로 mean을 전달 
table(ave_age) #넣기 전에 확인 
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

# 스케일 조정
#
# 데이터를 정규화시켜주는 것
# 두 변수 x, y를 사용할 것인데
# x가 (0~100)값을 갖고,  y가 (0~1)의 값을 갖는다면,
# 유클리디언 디스턴스로 유사성과 이질성을 판단하기 때문에,
# 스케일이 큰(값의 범위가 큰) x가 y에 비해 훨씬 큰 영향을 미친다
#
# 따라서 적절하게 조정해줘야 한다
#  
# 정규화 방법
# Xs = (X - mean) / s.d => 정규분포를 가정하고, 표준정규분포를 따르도록 정규화함
# Xs = (X - mean) / (max - min) => 모든 값을 -0.5, 0.5 사이의 값으로 정규화
# Xs = (X - min)  / (max - min) => 모든 값을 0, 1 사이의 값으로 정규화함
#
# 첫번째는 이상치를 가지고 있을 때,
# 두번째와 세번째는 이상치가 없을 때 사용하는 게 유용하다
#

interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale)) # 표준정규분포화 시켜준다
interests_z
set.seed(1234)
teen_clusters <- kmeans(interests_z, 5)

teen_clusters
teen_clusters$size
teen_clusters$centers

teens$cluster <- teen_clusters$cluster

# 처음 5개의 데이터만 살펴보기
teens[1:5, c("cluster", "gender", "age", "friends")]

# 각 클러스터별로 연령 평균 구하기
aggregate(data = teens, age ~ cluster, mean)

# 각 클러스터별로 여성의 비율 구하기
aggregate(data = teens, female ~ cluster, mean)

# 각 클러스터별로 친구 수 구하기
aggregate(data = teens, friends ~ cluster, mean)

aggregate(data = teens, soccer ~ cluster, mean)