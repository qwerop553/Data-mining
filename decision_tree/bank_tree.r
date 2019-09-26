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

cf.mat <- table(bank$PersonalLoan, predlmsi)

# 혼돈행렬 요약함수
cf.test <- function(cf.mat){
  true.positive <- cf.mat[1]
  false.positive <- cf.mat[2]
  false.negative <- cf.mat[3]
  true.negative <- cf.mat[4]
  total <- true.positive + false.positive + false.negative + true.negative
  
  false.positive.ratio <- (false.positive) / (true.negative + false.positive)
  false.negative.ratio <- (false.negative) / (true.positive + false.negative)

  accuracy <- (true.positive + true.negative) / (true.positive + false.positive + false.negative + true.negative)
  precision <- (true.positive) / (true.positive + false.positive)
  recall <- (true.positive) / (true.positive + false.negative)
  
  tagger <- function(x){
    cat("**", deparse(substitute(x)), as.double(x), "**\n")
  }
  
  tagger(true.positive)
  tagger(false.postive)
  tagger(false.negative)
  tagger(true.negative)
  tagger(total)
  cat("\n")
  tagger(false.positive.ratio)
  tagger(false.negative.ratio)
  cat("\n")
  tagger(accuracy)
  tagger(precision)
  tagger(recall)
}

cf.test(cf.mat)

# 일반화 능력 평가
index <- sample.int(nrow(bank), nrow(bank) * 0.7)

train <- bank[index,]
test <- bank[-index,]

fitTree2 <- rpart(formula = PersonalLoan~., train)
p <- predict(object = fitTree2, newdata = test, type = 'class')
cf.mat <- table(test$PersonalLoan, p)

cf.test(cf.mat)