library(tidyverse)
library(tm)
library(SnowballC)
library(e1071)
library(gmodels)
library(ROCR)
library(caret)
# library(wordcloud)

sms_raw <- read.csv("sms_spam.csv", stringsAsFactors=FALSE, encoding='UTF-8')

table(sms_raw$type) # ham 4812, spam 747
sms_raw[!sms_raw$type %in% c('ham', 'spam'), ]
sms_raw <- sms_raw[-1072,]
table(sms_raw$type)
sms_raw$type <- factor(sms_raw$type)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
sms_corpus_clean <- sms_corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords(kind='en')) %>% # ?���? 분석?�� ?�� ?��미�?� ?��?�� 불용?��(I, my, me, over)?���?
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)

# check
as.character(sms_corpus_clean[[1]])

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
as.character(sms_corpus_clean[[1]])

library(wordcloud)
wordcloud(sms_corpus_clean, min.freq=50, random.order=FALSE)

ham <- subset(sms_raw, sms_raw$type=='ham')
spam <- subset(sms_raw, sms_raw$type=='spam')

wordcloud(ham$text, min.freq=50, random.order=FALSE)

wordcloud(spam$text, min.freq=10, random.order=FALSE)
wordcloud(spam$text, max.words=10, random.order=FALSE)

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
inspect(sms_dtm[1:10, 1:30])

sms_freq_words <- findFreqTerms(sms_dtm, 5)
sms_dtm_freq <- sms_dtm[,sms_freq_words]
  
dim(sms_dtm_freq)
dim(sms_dtm)

# labeling function
convert_counts <- function(x){
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels=c(0, 1), labels=c("Absent", "Present"))
}

# label
sms_dtm_convert <- apply(sms_dtm_freq, 2, convert_counts)

# data partitioning
train_index <- createDataPartition(sms_raw$type, p=0.75, list=FALSE)

sms_dtm_train <- sms_dtm_convert[train_index,]
sms_raw_train <- sms_raw[train_index,]

sms_dtm_test <- sms_dtm_convert[-train_index,]
sms_raw_test <- sms_raw[-train_index,]

m <- naiveBayes(sms_dtm_train, sms_raw_train$type)

p <- predict(m, sms_dtm_test, type='class')
table(p)
table(sms_raw_test$type, p)
CrossTable(p, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

# Naive Bayes at caret

# traincontrol : control the computational nuane of the train function
ctrl <- trainControl(method='cv', number=10) # cross validation 10 times.

sms_nb_mod <- train(sms_dtm_train, sms_raw_train$type, method='nb', trControl = ctrl)

(sms_nb_mod)
sms_nb_pred <- predict(sms_nb_mod, sms_dtm_test)

(cm_nb <- confusionMatrix(sms_nb_pred, sms_raw_test$type, positive='spam'))

sms_test_prob <- predict(m, sms_dtm_test, type='raw')

sms_test_prob

pred <- ROCR::prediction(predictions = sms_test_prob[,2],
                   labels = sms_raw_test$type)


perf <- performance(pred, measure = 'tpr', x.measure = 'fpr')

plot(perf, main = "ROC curve for SMS spam filter", col = 'blue', lwd = 2)

perf.auc <- performance(pred, measure='auc')
perf.auc
pop