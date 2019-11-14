library(tidyverse)
library(caret)
library(tm) # library for text mining
library(extrafont)

setwd("./naive_bayse")
sms_raw <- read.csv("sms_spam.csv", encoding="UTF-8")

sms_raw$type <- factor(sms_raw$type)

train_index <- caret::createDataPartition(sms_raw$type, p=0.75, list=FALSE) 
# sms_raw$type이 균일하게 분포하도록 row number를 뽑아 준다.
# p는 어느 정도의 비율을 추출할지를 나타낸다.


# train data
sms_raw_train <- sms_raw[train_index,]

# test data
sms_raw_test <- sms_raw[-train_index,]

# create train corpus and refine, and make Document Term Matrix
sms_corpus_clean_train <- Corpus(VectorSource(sms_raw_train$text)) %>%
                          tm_map(content_transformer(tolower)) %>%
                          tm_map(removeNumbers) %>%
                          tm_map(removeWords, stopwords(kind='en')) %>%
                          tm_map(removePunctuation) %>%
                          tm_map(stripWhitespace)
                    
sms_dtm_train <- DocumentTermMatrix(sms_corpus_clean_train)                  
sms_dict <- findFreqTerms(sms_dtm_train, lowfreq=5)
sms_dtm_train <- DocumentTermMatrix(sms_corpus_clean_train, list(dictionary=sms_dict))

# create train corpus and refine, and make Document Term Matrix
sms_corpus_clean_test <- Corpus(VectorSource(sms_raw_test$text)) %>%
                          tm_map(content_transformer(tolower)) %>%
                          tm_map(removeNumbers) %>%
                          tm_map(removeWords, stopwords(kind='en')) %>%
                          tm_map(removePunctuation) %>%
                          tm_map(stripWhitespace)

sms_dtm_test <- DocumentTermMatrix(sms_corpus_clean_test, list(dictionary=sms_dict))

# labeling function
convert_corpus <- function(x){
  x <- ifelse(x > 0 , 1, 0)
  x <- factor(x, levels=c(0, 1), labels=c("Absent", "Present"))
}

# label
sms_train <- sms_dtm_train %>% apply(MARGIN=2,  FUN=convert_corpus)
sms_test <- sms_dtm_test %>% apply(MARGIN=2, FUN=convert_corpus)

# Modeling
# trainControl : Control the computational nuances of the train function
ctrl <- trainControl(method='cv', number=10) # cross validation 10 times. repeat = 3 이 안되네..

sms_nb_mod <- train(sms_train, sms_raw_train$type, method='nb', trControl=ctrl)

# Evaluation
(sms_nb_mod)
sms_nb_pred <- predict(sms_nb_mod, sms_test)
(cm_nb <- confusionMatrix(sms_nb_pred, sms_raw_test$type, positive='spam'))