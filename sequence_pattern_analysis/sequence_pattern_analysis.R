# install.packages("arulesSequences")
library(arulesSequences)

data(zaki)
zaki
inspect(zaki)

# sequenceID => 고객번호
# eventID => 고객별 구매순서
# SIZE => 구매아이템 수

# install.packages("doSNOW") 
# library(doSNOW)
# library(foreach)
# library(parallel)
# numCores <- detectCores() - 1
# (cl <- makeCluster(numCores))
# registerDoSNOW(cl)

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

library(dplyr)
pet_seq <- pet %>% group_by(임시고객번호, 주문일시) %>% 
  summarize(SIZE = n(), items = paste(상품명, collapse=';'))

# eventID 라는 것이 생겨남
pet_seq <- pet_seq %>% arrange(임시고객번호, 주문일시) %>% group_by(임시고객번호) %>%
  mutate(eventID = row_number()) 

head(pet_seq)

library(stringr)
colnames(pet_seq)[str_detect(colnames(pet_seq), "임시고객번호")] <- "sequenceID"

colnames(pet_seq)
# 열 추출 및 열 순서 바꾸기
pet_seq <- pet_seq[,c(1, 5, 3, 4)]
colnames(pet_seq)

# 트랜잭션 데이터 저장
write.table(pet_seq, 'petboxbasket.txt', sep = ';', row.names = FALSE, col.names = FALSE, quote = TRUE)

# 장바구니 데이터 형태로 다시 읽어오기
trans_matrix <- read_baskets('petboxbasket.txt', sep=';', info=c('sequenceID', 'eventID', 'SIZE'))

trans_matrix
inspect(trans_matrix[1:50])

trans_matrix@itemsetInfo <- arrange(trans_matrix@itemsetInfo,  sequenceID, eventID) # 잘 정렬되는 것 같지는 않군요.. ㅠㅠㅠ

s1 <- cspade(trans_matrix, parameter = list(support = 0.3))

s1.df <- as(s1, 'data.frame')
summary(s1)
