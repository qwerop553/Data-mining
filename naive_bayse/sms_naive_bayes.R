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

sms_freq_words <- findFreqTerms(sms_dtm, 5) # lowfreq = 5, highfreq = Inf
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

sms_nb_mod <- train(sms_dtm_train, sms_raw_train$type, method='nb', trControl=ctrl)

# Evaluation
(sms_nb_mod)
sms_nb_pred <- predict(sms_nb_mod, sms_dtm_test)
(cm_nb <- confusionMatrix(sms_nb_pred, sms_raw_test$type, positive='spam'))

# Kappa Statistic
# 우연히 맞을 확률을 제외하여 평가
# K = (Pr(a) - Pr(e)) / (1 - Pr(e))
# Pr(a) = 분류기가 실제 일치하게 맞추는 비율
# Pr(e) = 분류기 값과 실제값이 무작위 선택되었다고 가정했을때 두 값 사이의 예상 일치
# Pr(e) = (실제 양성의 비율) * (전체 데이터 비 분류기가 양성으로 분류한 비율) + (실제 음성의 비율) * (전체 데이터 비 분류기가 음성으로 분류한 비율)

# F - 척도 
# 모델의 서능을 정밀도와 재현율을 하나의 값으로 평가하고자 할때 사용
# 정밀도와 재현율을 조화평균내서 사용함
# F - 척도 = (2 * 정밀도 * 재현율) / (재현율 + 정밀도)
# 두 메트릭을 하나의 메트릭으로 만들어 사용
# 재현율과 정밀도가 둘 다 높을 때 점수가 좋아짐
# 조화평균 => 두 값이 얼마나 조화로운가?

# 민감도 (sensitivity, true positive rate) 
# 관심있는 클래스를 얼마나 잘 분류해 내는지
# sensitivity = (TP) / (TP + FN)
caret::sensitivity(sms_results$predict_type, sms_result$actual_type, positive="spam")

# 특이도 (specificity, true negative rate)
# 다른 클래스를 얼마나 잘 분류해 내는지
# specificity = (TN) / (TN + FP)
caret::specificity(sms_results$predict_type, sms_results$actual_type, negative="ham")


# ROC Curve
# Receiver Operating Characteristic Curve
# 거짓 긍정을 피하면서 참 긍정을 탐지하는 것 사이의 트레이드오프 관계를 확인하기 위함 

# y 축 => 참 긍정률 (민감도)
# x 축 => 거짓 긍정률 (1 - 특이도) 즉 거짓도 참이라고 하는 비율
# 임의로 찍었으면(데이터의 비율로 예측값을 내는 경우라면), 웬만하면 같은 비율로 틀리거나 맞기 때문에
# ROC 커브의 y = x 처럼 찍히게 됩니다
# 내가 넣은 값에 따라 참 긍정률 값과 거짓 긍정률 값이 바뀔 텐데,
# 성능이 조금씩 바뀔 텐데, 바뀌는 것으로 시뮬레이션을 해보면
# 연속되는 선으로 표현할 수 있을 것입니다.
# 이 알고리즘이 만들어낸 선이 얼마나 붙어 있는지 확인해 보면
# 이 알고리즘일 얼마나 좋은지 확인해 볼 수 있음
# 테스트 분류기 아래의 면적을 AUC (Area Under the Curve) 라 ㅎ함
# AUC 로 트레이드오프 관계를 보는듯..?
# 0.5 <= 1

library(ROCR)
sms_test_prob <- predict(m, sms_dtm_test, type='raw')
pred <- prediction(predictions = sms_test_prob[,2], 
                   labels = sms_raw_test$type)
sms_test_prob[,2]
# cutoff -> 이 값 이상(threshold) 을 positive로 하겠습니다.
# cutoff 값을 변화시키면서 ROC 커브를 그려갈 수 있습니다.

# prediction() 함수는 예측값에 대한 성능 평과결과가 가지고 있음
# 여기에 performance() 함수를 씌우면 다양한 metric 관점으로 성능 평가를 할 수 있음 
perf <- performance(pred, measure = 'tpr', x.measure = 'fpr')
