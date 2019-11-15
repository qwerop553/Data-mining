library(tidyverse)
library(caret)
library(tm) # library for text mining
library(extrafont)
library(SnowballC) # stemming(learning, learned => learn)
library(wordcloud)
library(e1071)
library(gmodels)

setwd("./naive_bayse")
sms_raw <- read.csv("sms_spam.csv", encoding="UTF-8")

table(sms_raw$type)

# create train corpus and refine
sms_corpus_clean <- Corpus(VectorSource(sms_raw$text)) %>%
                          tm_map(content_transformer(tolower)) %>%
                          tm_map(removeNumbers) %>%
                          tm_map(removeWords, stopwords(kind='en')) %>%
                          tm_map(removePunctuation) %>%
                          tm_map(stripWhitespace) %>% 
                          tm_map(stemDocument)

as.character(sms_corpus_clean[[1]])

# wordcloud for ham, spam
wordcloud(sms_corpus_clean, min.freq=50, random.order=FALSE)

ham <- subset(sms_raw, sms_raw$type=='ham')
spam <- subset(sms_raw, sms_raw$type=='spam')

wordcloud(ham$text, min.freq=50, random.order=FALSE)
wordcloud(spam$text, min.freq=10, random.order=FALSE)
wordcloud(spam$text, max.words=40, random.order=FALSE)

# create document term matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
inspect(sms_dtm[1:10, 1:30])

sms_freq_words <- findFreqTerms(sms_dtm, 5)
str(sms_freq_words)
sms_dtm_freq <- sms_dtm[,sms_freq_words]
dim(sms_dtm_freq)
dim(sms_dtm)

# labeling function
convert_counts <- function(x){
  x <- ifelse(x > 0 , 1, 0)
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

# Modeling
## Naive Bayes at e1071
m <- naiveBayes(sms_dtm_train, sms_raw_train$type)

m <- naiveBayes(sms_dtm_train, sms_raw_train$type, laplace = 2)

# Evaluation
p <- predict(m, sms_dtm_test, type='class')
table(sms_raw_test$type, p)
CrossTable(p, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


## Naive Bayes at caret

### trainControl : Control the computational nuances of the train function
ctrl <- trainControl(method='cv', number=10) # cross validation 10 times. repeat = 3 이 안되네..

sms_nb_mod <- train(sms_train, sms_raw_train$type, method='nb', trControl=ctrl)

# Evaluation
(sms_nb_mod)
sms_nb_pred <- predict(sms_nb_mod, sms_test)
(cm_nb <- confusionMatrix(sms_nb_pred, sms_raw_test$type, positive='spam'))