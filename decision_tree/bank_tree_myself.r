library(rpart)
library(rpart.plot)

bank <- read.csv(file = "https://goo.gl/vE8GyN")

str(bank)
summary(bank)

bank$PersonalLoan <- as.factor(bank$PersonalLoan)

fitTree <- rpart(formula=PersonalLoan~., data=bank)

print(fitTree)
plot(fitTree)
text(fitTree)
prp(fitTree, type=4, extra=8, digits=3)

set.seed(2345)
index <- sample.int(nrow(bank), nrow(bank)*0.7)
train <- bank[index,]
test <- bank[-index,]

fitTree2 <- rpart(formula=PersonalLoan~., data=train)
predImsi <- predict(object=fitTree2, newdata=test, type='class')

cf.mat <- table(test$PersonalLoan, predImsi)

true.positive <- cf.mat[1]
false.positive <- cf.mat[2]
false.negative <- cf.mat[3]
true.negative <- cf.mat[4]

Accuracy <- (true.positive + true.negative)/(true.positive + false.positive + false.negative + true.negative)
Precision <- (true.positive) / (true.postive + false.positive)
Recall <- (true.positive) / (true.positive + false.negative)

false.negative.ratio <- (false.negative)/(false.negative + true.positive)
false.positive.ratio <- (false.positive)/(false.positive + true.negative)
