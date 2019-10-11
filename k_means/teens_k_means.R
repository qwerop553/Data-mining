# SNS를 이용한 십대 시장 세분화
# 십대 SNS 페이지의 텍스트를 이요하여 스포츠, 종교, 음악과 같은 공통 관심을 공유하는 그룹을 찾을 수 있음
# 데이터 수집: 데이터는 SNS 사이트를 크롤링해서 가져옴
# 데이터 탐색 및 준비

teens <- read.csv("C:/data_mining/k_means/snsdata.csv")

str(teens)

table(teens$gender)

table(teens$gender, useNA='ifany') # NA 도 집계

summary(teens$age) # summary는 원래 집계

teens$age <- ifelse(teens$age >= 13 &teens$age, teens$age, NA)

summary(teens$age)


# 더미 변수
#
# 알고리즘에 따라서는 수치형 변수만을 받는 경우가 있음
#
# 예를 들면 K-means 알고리즘의 경우 거리를 계산하고,
# 평균을 구해서 centroid를 구하기 때문에 산술연산이 가능한
# 수치형 변수를 사용해야 함
# 
# 명목형 변수를 수치로 변경해서 사용하게 되면
#
# 즉 나이를 노인/중년/청년으로 나타내는 변수를 2/1/0 으로 사용하게 되면
# 숫자가 연산이 되면서 centroid 1.5 이런 수치가 나오는데, 이 값은
# 의미를 가지지 못함(중년스러운 노인?)
#
# 그래서 명목형 변수는 값의 수준별로 따로따로 변수를 생성
# 그렇게 해서 생긴 변수는 1, 0 이 값만을 가지는데 이를 더미변수라고 함
#
# 더미변수는 연산이 가능한데, '노인'변수에서 연산을 통해 0.7이나오면
# 0.7 퍼센트가 노인이다 라는 의미로 사용할 수 있음
#
# 주의할것 -> ifelse 는 값 자체가 없을 때 test 가 안된다 !
# teens$gender <- ifelse(teens$gender == "F" , ..) 를 사용하면
# NA 값이 있는 곳에서는 그냥 NA 값이 들어간다.

teens$female <- ifelse(!is.na(teens$gender) & teens$gender == "F", 1, 0)
teens$male <- ifelse(!is.na(teens$gender) & teens$gender == "M", 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

# 확인 
table(teens$female);table(teens$male);table(teens$no_gender);
2724 + 5222 == 7946