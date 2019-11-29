install.packages("arulesSequences")
library(arulesSequences)

data(zaki)
zaki
inspect(zaki)

# sequenceID => 고객번호
# eventID => 고객별 구매순서
# SIZE => 구매아이템 
install.packages("doSNOW")
library(doSNOW)

library(foreach)
library(parallel)

numCores <- detectCores() - 1
(cl <- makeCluster(numCores))
registerDoSNOW(cl)


s0 <- cspade(zaki, parameter = list(support = 0,
                                    maxsize = 1, maxlen = 1))

# maxsize => 세트를 구성하는 아이템의 갯수
# maxlen => sequence를 구성하는 아이템세트의 갯수

# <{G}, {H}> G가 나오고 H가 나온거
# <{G, H}> G와 H가 동시에 나온거

# 너무 많이 나와서 짜증난다 => support 값을 올린다
as(s0, "data.frame")

s1 <- cspade(zaki, parameter = list(support = 0.4))

library(data.table)
pet <- fread("petbox.csv", encoding="UTF-8")


# 임시고객번호 => sequenceID
# 