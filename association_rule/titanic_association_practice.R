# titanic_association_practice

Titanic
class(Titanic)

titan.df <- as.data.frame(Titanic)
head(titan.df)
titan.df[4, "Freq"]
'
for(i in 1:nrow(titan.df))
  for(j in 0:titan.df[i, "Freq"]-1)
    titan <- rbind(titan, titan.df[i,])
  titanic <- NULL
'

titanic <- NULL
for(i in 1:4) titanic <- cbind(titanic, rep(as.character(titan.df[,i]), titan.df$Freq))
titanic <- as.data.frame(titanic)
titanic
names(titanic) <- c("Class", "Sex", "Age", "Survived")
library(arules)
myrules <- apriori(data = titanic, parameter = list(support = 0.01, confidence = 0.25, minlen = 2))
summary(myrules)
inspect(myrules[1:50])

not_survived <- subset(myrules, rhs %in% "Survived=No")
inspect(sort(not_survived, by='confidence'))