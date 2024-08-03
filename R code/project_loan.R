loan <- read.csv("train.csv")
loan_train <- read.csv("train.csv")
loan_test <- read.csv("test.csv")
loan
View(loan)
library(dplyr)
library(ggplot2)
library(MASS)
library(stringr)
library(psych)
library(stats)
## 데이터 전처리 과정1:  문자열 변수 -> 범주형 변수로 변형 작업업
loan$대출목적 <- as.factor(loan$대출목적)
loan$대출기간 <- as.factor(loan$대출기간)
loan$주택소유상태 <- as.factor(loan$주택소유상태)
loan$대출등급 <- as.factor(loan$대출등급);


##범주형변수들을 group by 함수를 이용한 각 변수들의 대출목적, 근로기간, 주택소유상태 파악
loan %>% group_by(대출목적) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

loan %>% group_by(근로기간) %>% 
  summarise(n=n()) %>% 
  arrange(desc(-근로기간));

loan %>% group_by(주택소유상태) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) 

loan %>% group_by(최근_2년간_연체_횟수) %>% 
  summarise(n=n())
## mortgage, rent에 비해 any 데이터는 1개로 너무 작음 

loan %>% arrange(-부채_대비_소득_비율)
##이상값 찾는 과정
boxplot(loan$대출금액)
boxplot(loan$연간소득)
boxplot(loan$부채_대비_소득_비율)##이상값 하나가 너무 커서 제거가 필요함.

##이상치 데이터 삭제
loan <- loan %>% filter(loan$부채_대비_소득_비율 != 9999) ##결측값
loan <- loan %>% filter(loan$주택소유상태 != "ANY") ## 이상치로 판단

# 요인분석을 위한 수치형 데이터로 변환 (ID, 대출기간, 근로기간, 대출목적, 주택소유상태)
loan$ID <- as.numeric(gsub("TRAIN_","",loan$ID))

loan$대출기간 <- as.numeric(gsub(" months","", loan$대출기간))



loan$근로기간 <- ifelse(loan$근로기간 == "<1 year" | loan$근로기간 == "< 1 year", "0 year",
                    ifelse(loan$근로기간 == "1 year" | loan$근로기간 == "1 years", "1 year",
                           ifelse(loan$근로기간 == "3 years" | loan$근로기간 == "3", "3 years",
                                  ifelse(loan$근로기간 == "10+ years" | loan$근로기간 == "10+years", "10 years", 
                                  ifelse(loan$근로기간 == "Unknown", NA,loan$근로기간
                                  )))))
extract_numeric <- function(x) {
  as.numeric(str_extract(x, "\\d+"))
}
loan$근로기간 <- extract_numeric(loan$근로기간)

loan$주택소유상태 <- ifelse(loan$주택소유상태 == "MORTGAGE", 1,
                      ifelse(loan$주택소유상태 =="RENT", 2, 3))

loan$대출목적 <- ifelse(loan$대출목적 == "부채 통합", 1,
                    ifelse(loan$대출목적 == "신용 카드", 2,
                    ifelse(loan$대출목적 == "주택 개선", 3,
                    ifelse(loan$대출목적 == "주요 구매", 4,
                    ifelse(loan$대출목적 == "의료",5,
                    ifelse(loan$대출목적 == "자동차",6,
                    ifelse(loan$대출목적 == "소규모 사업", 7,
                    ifelse(loan$대출목적 == "이사", 8,
                    ifelse(loan$대출목적 == "휴가",9,
                    ifelse(loan$대출목적 == "주택",10,
                    ifelse(loan$대출목적 == "재생 에너지", 11, 12)))))))))))

loan$연체여부 <- ifelse(loan$최근_2년간_연체_횟수 >= 1, 1, 0)
loan$연체여부2 <- ifelse(loan$최근_2년간_연체_횟수>= 2, 1, 0)## 1번은 봐주는 경우
loan <- subset(loan, select = -c(근로기간))
loan2 <- subset(loan, select = -c(ID, 주택소유상태, 대출목적, 최근_2년간_연체_횟수,  대출등급, 연체여부, 연체여부2))

loan %>% group_by(연체여부2) %>% summarise(n=n())
6822/(89470+6822)
## principal 함수 사용 -> 주성분분석을 기반으로 함. 
## 변수들 사이의 공분산 행렬을 통해 주성분을 추출하는 방법

factor_analysis <- principal(loan2, nfactors = 4, rotate = "varimax", scores = TRUE)

factor_analysis
summary(factor_analysis)

## 판별함수 도출, 판별분석
View(factor_analysis)
## 종속변수: 0과 1로 되어있는 채무 이행자: 1, 채무 미이행자: 0으로 나눈 변수
## 최근 2년간 연체 횟수를 갚는 사람과 안 갚는 사람이라는 판별분석의 종속변수로 나타내보자. 



factor_scores <- data.frame(factor_analysis$scores[, 1:4])
dependent_variable <- loan$연체여부2
discriminant_model <- lda(dependent_variable ~ ., data = factor_scores)
discriminant_model
predictions <- predict(discriminant_model)
predicted_classes <- predictions$class 
actual_classes <- factor(loan$연체여부2)
accuracy <- mean(predicted_classes == actual_classes)
accuracy
install.packages("caret")
library(caret)
predicted_classes <- predictions$class 
actual_classes <- factor(loan$연체여부2)

# 혼동 행렬 생성
conf_matrix <- confusionMatrix(predicted_classes, actual_classes)

# 정확도, 정밀도, 재현율, F1 점수 출력
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value'] # 정밀도
recall <- conf_matrix$byClass['Sensitivity'] # 재현율
f1_score <- 2 * (precision * recall) / (precision + recall) # F1 점수

# 결과 출력
cat("Accuracy: ", accuracy, "\n")
cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("F1 Score: ", f1_score, "\n")

loan_test <- read.csv("test.csv")

loan_test$대출목적 <- as.factor(loan_test$대출목적)
loan_test$대출기간 <- as.factor(loan_test$대출기간)
loan_test$주택소유상태 <- as.factor(loan_test$주택소유상태)

loan_test %>%  group_by(근로기간) %>%  summarise(n=n())

loan_test <- loan_test %>% filter(loan_test$부채_대비_소득_비율 != 9999) ##결측값
loan_test <- loan_test %>% filter(loan_test$주택소유상태 != "ANY") ## 이상치로 판단

loan_test$ID <- as.numeric(gsub("TRAIN_","",loan_test$ID))

loan_test$대출기간 <- as.numeric(gsub(" months","", loan_test$대출기간))

loan_test <- loan_test %>% rename('부채/소득' = '부채_대비_소득_비율')

loan_test$근로기간 <- ifelse(loan_test$근로기간 == "<1 year" | loan_test$근로기간 == "< 1 year", "0 year",
                      ifelse(loan_test$근로기간 == "1 year" | loan_test$근로기간 == "1 years", "1 year",
                      ifelse(loan_test$근로기간 == "3 years" | loan_test$근로기간 == "3", "3 years",
                      ifelse(loan_test$근로기간 == "10+ years" | loan_test$근로기간 == "10+years", "10 years", 
                      ifelse(loan_test$근로기간 == "Unknown", NA,loan_test$근로기간)))))
extract_numeric <- function(x) {
  as.numeric(str_extract(x, "\\d+"))
}
loan_test$근로기간 <- extract_numeric(loan_test$근로기간)

loan_test$주택소유상태 <- ifelse(loan_test$주택소유상태 == "MORTGAGE", 1,
                           ifelse(loan_test$주택소유상태 =="RENT", 2, 3))

loan_test$대출목적 <- ifelse(loan_test$대출목적 == "부채 통합", 1,
                      ifelse(loan_test$대출목적 == "신용 카드", 2,
                      ifelse(loan_test$대출목적 == "주택 개선", 3,
                      ifelse(loan_test$대출목적 == "주요 구매", 4,
                      ifelse(loan_test$대출목적 == "의료",5,
                      ifelse(loan_test$대출목적 == "자동차",6,
                      ifelse(loan_test$대출목적 == "소규모 사업", 7,
                      ifelse(loan_test$대출목적 == "이사", 8,
                      ifelse(loan_test$대출목적 == "휴가",9,
                      ifelse(loan_test$대출목적 == "주택",10,
                      ifelse(loan_test$대출목적 == "재생 에너지", 11, 12)))))))))))




loan_test$연체여부 <- ifelse(loan_test$최근_2년간_연체_횟수 >= 1, 1, 0)
loan_test$연체여부2 <- ifelse(loan_test$최근_2년간_연체_횟수>= 2, 1, 0)## 1번은 봐주는 경우
loan_test <- subset(loan_test, select = -c(근로기간))
loan_test2 <- subset(loan_test, select = -c(ID, 주택소유상태,  대출목적, 최근_2년간_연체_횟수, 연체여부, 연체여부2))

factor_analysis_test <- principal(loan_test2, nfactors = 4, rotate = "varimax", scores = TRUE)

factor_analysis_test

## 종속변수: 0과 1로 되어있는 채무 이행자: 0, 채무 미이행자: 1으로 나눈 변수
## 최근 2년간 연체 횟수를 갚는 사람과 안 갚는 사람이라는 판별분석의 종속변수로 나타내보자. 


dependent_variable_test <- loan_test$연체여부2

#훈련 데이터의 요인분석을 활용한 평가 데이터의 요인분석. 
factor_scores_test <- predict(factor_analysis, 
                              loan_test2[,c('대출금액', '대출기간', '연간소득', 
                                            '부채_대비_소득_비율', '총계좌수', 
                                            '총상환원금', '총상환이자', '총연체금액', 
                                            '연체계좌수')])[, 1:4]

factor_scores_test <- data.frame(factor_scores_test)
head(factor_scores_test)

discriminant_model_test <- lda(dependent_variable_test ~ ., data = factor_scores_test)
discriminant_model_test
# 선형 판별분석 모델을 사용하여 테스트 데이터 예측
predictions <- predict(discriminant_model_test, newdata = factor_scores_test)
# 예측된 클래스
predicted_classes <- predictions$class
# 실제값 추출
actual_classes <- factor(loan_test$연체여부2)

accuracy <- mean(predicted_classes == actual_classes)
accuracy


# 혼동 행렬 생성
conf_matrix <- confusionMatrix(predicted_classes, actual_classes)

# 정확도, 정밀도, 재현율, F1 점수 출력
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value'] # 정밀도
recall <- conf_matrix$byClass['Sensitivity'] # 재현율
f1_score <- 2 * (precision * recall) / (precision + recall) # F1 점수

# 결과 출력
cat("Accuracy: ", accuracy, "\n")
cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("F1 Score: ", f1_score, "\n")



