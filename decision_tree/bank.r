# 라이브러리
library(rpart) # 의사결정나무 알고리즘
library(rpart.plot) # 의사결정나무 시각화

# 데이터 준비
bank <- read.csv(file = "https://goo.gl/vE8GyN")

# 데이터 확인
str(bank)
head(bank)

summary(bank)

# 데이터 전처리
bank$PersonalLoan <- as.factor(bank$PersonalLoan)


# 의사결정나무
fitTree <- rpart(formula = PersonalLoan ~ ., data = bank)

# 시각화
print(fitTree)
plot(fitTree)
text(fitTree)
prp(fitTree, type=4, extra=2, digits=3)
