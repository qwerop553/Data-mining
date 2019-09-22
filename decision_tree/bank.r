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

# 평가
predlmsi <- predict(object = fitTree, newdata = bank, type = 'class') # 0, 1의 값을 원함

table(bank$PersonalLoan, predlmsi)

# 일반화 능력 평가
index <- sample.int(nrow(bank), nrow(bank) * 0.7)

train <- bank[index,]
test <- bank[-index,]

fitTree2 <- rpart(formula = PersonalLoan~., train)
p <- predict(object = fitTree2, newdata = test, type = 'class')
table(test$PersonalLoan, p)
"
      0    1
  0 1350    9
  1    9  132
"