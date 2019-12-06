reg <- function(y, x){
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimate"
  print(b)
}

challenger <- read.csv("challenger.csv")

# 부모의 키와 자식의 키의 상관관계
# install.packages("UsingR")
library(UsingR) # UsingR 패키지 => 데이터가 모여있는 패키지
str(galton)

par(mfrow = c(1, 2))
hist(galton$child, col='blue', breaks=100)
hist(galton$parent, col='blue', breaks=100)

cor(galton$child, galton$parent)

cor.test(galton$child, galton$parent)

lm.model = lm(child~parent, data = galton)
lm.model

summary(lm.model)

plot(child~parent, data=galton)

abline(lm.model, col='red')
