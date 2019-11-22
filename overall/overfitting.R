# 과적합
# 가지고 있는 데이터셋을 몽땅 넣어서 예측 혹은 분류 모델을 훈련 시키면
# 이게 과소적합(under-fitting)인지, 적정적합(generalized-fitting)인지,
# 과적합인지(over-fitting)인지를 가능하기가 힘듦
#
# 문제는 훈련을 시키면 시킬 수록 Error rate는 계속 줄어드는 경향이 있으므로
# 결국은 과적합(over-fitting)으로 귀결
# (1)
# Training set 을 가지고 예측 혹은 분류 모델을 훈련시키고
# 
# (2)
# Validation set을 가지고서 (1)번의 Training set을 가지고
# 훈련 중인 모델이 혹시 과적합(over-fitting)인지, 아니면
# 훈련 더해야 하는 과소적합(under-fitting)인 것은 아닌지 검사해야 함
# 
# (3)
# Test set을 사용해서 (1)번과 (2)번의 협동작업으로 도출한
# 최종 모델(final model)에 대해서 성적을 매기는 작업을
# 하게 됨

# Holdout method
# 검증/테스트 데이터를 따로 남겨두는 방식을 홀드아웃 방법이라고 함

# 난수를 이용하는 방법
random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:500],]
credit_validate <- credit[random_ids[501:750],]
credit_test <- credit[random_ids[751:1000],]

in_train <- createDataPartition(credit$default, p = 0.75, list=FALSE)
credit_train <- credit[in_train,]
creidt_test <- credit[-in_train,]

# k-fold Cross Validation
# training set, validation set, test set으로 나누게 되면
# 모델 훈련에는 training set(50%~60%)만이 사용
#
# 데이터가 충분하지 못한 상태에서 그걸 3개의 훈련, 검증, 테스트 셋으로 나누면
# 분할된 데이터셋에 무슨 데이터가 들어갔느냐에 따라 모형이 심하게 영향을 받을 수 있음
#
# training set을 k등분한 후에 --> (k-1)개의 fold( = (k-1)/k 구성비) 는 train으로 사용하고, 
# 나머지 1개의 fold(1/k 구성비)은 validation set으로 사용하며, --> validation set에 해당하는 fold를
# round를 거듭하면서 바꿔주게 됨 

folds <- caret::createFolds(credit$default, k=10)

str(folds)
credit01_test <- credit[folds$Fold01,]
credit01_train <- credit[-folds$Fold01,]

cv_results <- lapply(folds, function(x){
  credit_train <- credit[-x,]
  credit_test <- credit[x,]
  credit_model <- C50::C5.0(as.factor(default)~., data=credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  rslt <- table(credit_test$answer, credit_pred)
  pr_a <- sum(diag(rslt)) / sum(rslt)
  pr_e <- sum((diag(rslt) / rowSums(rslt)) * (diag(rslt) / colSums(rslt)))
  kappa = (pr_a - pr_e) / (1 - pr_e)
  return(kappa)
})
