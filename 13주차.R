library(carData)
data("TitanicSurvival")

# 생존여부 이진 변수화
TitanicSurvival$survived_bin <- ifelse(TitanicSurvival$survived == "yes", 1, 0)

# 로지스틱 회귀 적합 (NA 있는 행 자동 제거)
model_logit <- glm(survived_bin ~ sex + age + passengerClass,
                   data = TitanicSurvival, family = binomial())

# 모델에 사용된 데이터 확인
used_data <- model_logit$model

# 예측값 계산
pred_prob <- predict(model_logit, newdata = used_data, type = "response")

# ROC 분석
library(pROC)
roc_obj <- roc(used_data$survived_bin, pred_prob)
plot(roc_obj, main = "ROC Curve for Titanic Survival Prediction")
auc(roc_obj)
