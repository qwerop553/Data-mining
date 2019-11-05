유방암의 진단

임상적 방법
조직 검출 과정에서는 비정상적인 덩어리나 종양이 있는지 조직검사를 함
덩어리가 발견되며 미세침 흡인 조직검사를 실행하는데 임상의가 이의 악성인지/양성인지를
현미경으로 세포관찰을 하게 됨

기계 학습 방법
세포관찰에 의한 주관적인 판단 대신에 조직의 특성을 기계 학습하여 자동화하면 의료 시스템을 개선할 수 있음
진단 시간을 감소시키고, 사람의 주관적 편차를 줄일 수 있음

사용 변수
디지털 이미지에 존재하는 세포핵의 특성을 나타냄
위스컨신 대학원의 연구원들이 기부한 데이터
UCI breast cancer
1. 반지름 radius
2. 질감 texture
3. 둘레 perimeter
4. 넓이 area
5. 매끄러움 smoothness
6. 조밀성 compactness
7. 오목함 concavity
8. 오목점 concave points
9. 대칭성 symmetry
10. 프랙탈 차원 Fractal dimension

이와 같은 10개의 특성에 대한 평균/표준편차/최대값 의 세가지 특성을 이용
-> 총 30개의 변수가 생성됨
여기에 식별 번호 및 진단 결과가 추가되어 있음

###분석 코드
##데이터 읽어오기
wbcd <- read.csv("c:/data_mining/knn/wisc_bc_data.csv", stringsAsFactors=FALSE)
str(wbcd)

wbcd <- wbcd[-1] # id 제거

table(wbcd$diagnosis)

wbcd$diagnosis <- factor(wbcd$diagnosis, levels=c("B", "M"), labels=c("Benign", "Malignant"))

round(prop.table(table(wbcd$diagnosis) * 100), 2)

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

normalize <- function(x){
    return((x - min(x))/(max(x) - min(x)))
}

normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

summary(wbcd_n$area_mean) # 임의의 변수에 대해 테스트

index <- sample(1:nrow(wbcd_n), nrow(wbcd_n) * 0.7)
wbcd_train <- wbcd_n[index,]
wbcd_test <- wbcd_n[-index,]
wbcd_train_labels <- wbcd[index, 1]
wbcd_test_labels <- wbcd[-index, 1]

library(class)

wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test,
                      cl = wbcd_train_labels, k = 21)

gmodels::CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
                    prop.chisq = FALSE)


# k fold 

knn.cv를 잘 사용해서 해보시길..


# finding best k

accuracy <- c()

for(k in 1:nrow(wbcd_train)){

    wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test,

                       cl = wbcd_train_labels, k = k)

    accuracy <- c(accuracy, sum(wbcd_test_pred == wbcd_test_labels)/length(wbcd_test_pred))

}
which.max(accuracy)