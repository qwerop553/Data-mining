library(arules)

Titanic # 4차원 데이터

titan.df <- as.data.frame(Titanic) # table -> data.frame
head(titan.df)

'
장바구니 분석을 하려면 구매별로 데이터가 다 찍허 있어야 분석을 할 수 있어요
현재 요약되어 있는 데이터를 한줄한줄의 데이터로 바꿀거에요
즉

1st Male Adult No 118 을 
118개의 데이터로 풀어 줄 것이다.
'

titanic <- NULL

for(i in 1:4){titanic <- cbind(
  titanic, rep(as.character(titan.df[,i]), titan.df$Freq))}

titanic <- as.data.frame(titanic)
names(titanic) <- names(titan.df)[1:4]

myrules <- apriori(data=titanic, parameter=list(support=0.01, confidence=0.25, minlen=2))
summary(myrules)
inspect(sort(myrules, by='support')[1:50])

survived <- subset(myrules, items %in% "Survived=No")
head(inspect(sort(survived, by='confidence')), 5)

who_survived <- subset(myrules, rhs %in% "Survived=Yes")
head(inspect(sort(who_survived, by='confidence')), 5)

who_not_survived <- subset(myrules, rhs %pin% "Survived=No")
head(inspect(sort(who_not_survived, by='confidence')), 5)


