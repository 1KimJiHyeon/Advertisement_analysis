library("dplyr")
library("ggplot2")
library('plotly')

advertising = read.csv("ad.csv")
head(advertising)
summary(advertising)
#sales는 의존적인 변수이고 TV,radio,newspaper은 독립적인 변수이다.

#1. 데이터의 특성 파악

#1-1. 각 데이터의 산점도

# Tv 광고가 많을수록 sales가 높다는 것이다. 이 선의 경사는 판매량이다.
qplot( x = TV, y = sales, data = advertising)+ geom_point()+ geom_smooth(method = "lm", formula = y ~ x)

# radio 광고가 많을수록 sales가 높다는 것이다. 이 선의 경사는 판매량이다.
qplot( x = radio, y = sales, data = advertising)+ geom_point()+ geom_smooth(method = "lm", formula = y ~ x)

# newspaper 광고가 많을수록 sales가 높다는 것이다. 이 선의 경사는 판매량이다.
qplot( x = newspaper, y = sales, data = advertising)+ geom_point()+ geom_smooth(method = "lm", formula = y ~ x)


# 1-2. 데이터의 관계가 선형인지 확인

# TV와 판매의 관계는 선형이다.
advertising %>%
  ggplot(aes(x = TV, y = sales)) +
  geom_point(colour = "red", shape = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black")

# Radio 판매의 관계는 선형이다.
advertising %>%
  ggplot(aes(x = radio, y = sales)) +
  geom_point(colour = "red", shape = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black")

# Newspaper 판매의 관계는 선형이다.
advertising %>%
  ggplot(aes(x = newspaper, y = sales)) +
  geom_point(colour = "red", shape = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black")

# 1-3. 종속 변수에 대한 가장 강력한 상관 변수 두 개 사이에 3D 산점도를 작성한다. 
# radio와 sales, TV와 sales의 상관관계가 긍정적으로 연관되어 있음을 나타낸다. radio와 TV 광고가 증가함에 따라, sales는 증가한다.
plot_ly(advertising, x = ~radio, y = ~TV, z = ~sales, marker = list(size = 5)) 


#2. 독립 변수와 종속변수 사이의 상관관계 확인 
# TV와 판매의 상관관계는 1 0.7822244로 상당히 강하며, 라디오와 판매의 상관관계는 0.5762226로 상관관계가 보통이고, 신문지와 판매의 상관관계는 0.228299로 상관관계가 약하다.

advertising %>%
  summarize(r_TV = cor(TV, sales),
            r_Radio = cor(radio, sales),
            r_Newspaper = cor(newspaper, sales))

#3. 선형 회귀 모델 생성
#3-1. 단순 선형 회귀 모델

#TV 광고가 sales에 미치는 영향 
lmTV = lm(sales ~ TV, data = advertising)


#radio와 광고가 sales에 미치는 영향 
lmRadio <- lm(sales ~ radio, data = advertising)

# newspaper 광고가 sales에 미치는 영향 
lmNewspaper <- lm(sales ~ newspaper, data = advertising)

# 회귀 분석 결과 확인

# TV의 p-value 값은 < 2.2e-16로 0.05보다 많이 작으므로 대립가설을 채택한다.즉 독립 변수(TV)는 종속 변수(sales)에 영향을 준다(통계적으로 유의미하다).Adjusted R-squared가 0.6099이므로 100에서 60정도 해당한다.
# TV의 예상 판매량 = 7.03259+0.04754*TV +ei
summary(lmTV)

# TV가 1000일때 예상 판매량 = 54.56923
predict(object = lmTV, newdata = data.frame(TV = 1000))


# Radio의 p-value 값은< 2.2e-16로 0.05보다 많이 작으므로 대립가설을 채택한다.즉 독립 변수(Radio)는 종속 변수(sales)에 영향을 준다(통계적으로 유의미하다).Adjusted R-squared가 0.3287이므로 100에서 32정도 해당한다.
# radio의 예상 판매량 = 9.3116+0.2025*radio +ei
summary(lmRadio)

# radio가 1000일때 예상 판매량 = 211.8074
predict(object = lmRadio, newdata = data.frame(radio= 1000))

# Newspaper의 p-value 값은 0.001148로 0.05보다 작으므로 대립가설을 채택한다.즉 독립 변수(newspaper)는 종속 변수(sales)에 영향을 준다(통계적으로 유의미하다).Adjusted R-squared가 0.04733이므로 100에서 4정도 해당한다.
# Newspaper은 TV나 radio에 비해 p-value가 높은 편이라서 TV와 radio에 비해 sales에 관련이 크지 않다. 하지만 TV와 radio는sales과 관련이 크다.
# Newspaper의 예상 판매량 = 12.35141+0.05469*newspaper +ei
summary(lmNewspaper)

# newspaper가 1000일때 예상 판매량 = 67.04451
predict(object = lmNewspaper, newdata = data.frame(newspaper = 1000))

# 3-2. 다중 선형 회귀 모형

#TV와 Newspaper를 독립 변수로 두고 종속 변수(sales)에 어떤 영향을 주는지 알아보았다.
TvNewspaper_sales <- lm(sales ~ newspaper + TV ,data= advertising )

# Newspaper+TV의 p-value 값은  < 2.2e-16이며 0.05보다 작으므로 대립가설을 채택한다.즉 독립 변수(Newspaper + TV)는 종속 변수(sales)에 영향을 준다(통계적으로 유의미하다).Adjusted R-squared가 0.6422이므로 100에서 64정도 해당한다.
# TV와 Newspaper를 합한 예상 판매량 = 5.774948 + 0.044219 * (newspaper) + 0.046901 * (TV)
summary(TvNewspaper_sales)

# TV가 1000와 newspaper가 1000일때 예상 판매량 = 96.89559
predict(object = TvNewspaper_sales, newdata = data.frame(TV = 1000 , newspaper = 1000))


#Radio와 Newspaper를 독립 변수로 두고 종속 변수(sales)에 어떤 영향을 주는지 알아보았다.
RadioNewspaper_sales <- lm(sales ~ newspaper + radio ,data= advertising )

# Newspaper+Radio의 p-value 값은  < 2.2e-16이며 0.05보다 작으므로 대립가설을 채택한다.즉 독립 변수(Newspaper + Radio)는 종속 변수(sales)에 영향을 준다(통계적으로 유의미하다).Adjusted R-squared가 0.3259이므로 100에서 32정도 해당한다.
# Radid와 Newspaper를 합한 예상 판매량 = 9.188920 + 0.006644 * (newspaper) + 0.199045 * (radio)
summary(RadioNewspaper_sales)

# radio가 1000와 newspaper가 1000일때 예상 판매량 = 214.8777
predict(object = RadioNewspaper_sales, newdata = data.frame(radio= 1000, newspaper = 1000))

#상관 변수가 가장 낮았던 newspaper을 빼고 TV와 radio를 독립 변수로 두고 종속 변수에 어떤 영향을 주는지 알아보았다.
TvRadio_sales <- lm(sales ~ radio + TV ,data= advertising )

# radio+TV의 p-value 값은  < 2.2e-16이며 0.05보다 작으므로 대립가설을 채택한다.즉 독립 변수(radio + TV)는 종속 변수(sales)에 영향을 준다(통계적으로 유의미하다).Adjusted R-squared가 0.8962이므로 100에서 89정도 해당한다.
# radio+TV의 Adjusted R-squared가  0.8962로  radio , tv , newspaper을 따로 하는 것 보다 크기때문에 더 좋다.
# TV와 radio를 합한 예상 판매량 = 2.92110 + 0.18799 * (radio) + 0.04575 * (TV)
summary(TvRadio_sales)

# TV가 1000와 radio가 1000일때 예상 판매량 = 236.6701
predict(object = TvRadio_sales, newdata = data.frame(TV = 1000 , radio= 1000))


# TV와 radio, TV와 radio의 상호작용이 sales에 미치는 영향
TvandRadio_sales <- lm(sales ~ radio * TV ,data= advertising )

summary(TvandRadio_sales)

predict(object = TvandRadio_sales, newdata = data.frame(TV = 1000 , radio= 1000))


#모든 관련 독립 변수 및 종속 변수를 사용하여 다중 선형 회귀 모델을 생성

all_sales <- lm( sales ~ TV+newspaper+radio,data= advertising)

#radio+TV+newspaper의 p-value 값은  < 2.2e-16이며 0.05보다 작으므로 대립가설을 채택한다. 즉 독립 변수(radio + TV+newspaper)는 종속 변수(sales)에 영향을 준다(통계적으로 유의미하다).Adjusted R-squared가 0.8956이므로 100에서 89정도 해당한다.
#TV와 radio와 newspaper을 합한 예상 판매량  = 3.0052094 + 0.1883832 * (radio) + 0.0457759 * (TV) -0.0012433 * (newspaper)
summary(all_sales)

# radio가 1000와 TV가 1000와 newspaper가 1000일때 예상 판매량 = 236.1961
predict(object = all_sales, newdata = data.frame(TV = 1000 , radio= 1000, newspaper = 1000))




#회귀 분석 결과를 그래프로 확인

#모든 관련 독립 변수의 회귀 분석 결과 그래프
par(mfrow=c(2,2))
plot(all_sales)

# Tv에 따른 판매량의 회귀 분석 결과 그래프
par(mfrow=c(2,2))
plot(lmTV)

#radio에 따른 판매량의 회귀 분석 결과 그래프
par(mfrow=c(2,2))
plot(lmRadio)

# newspaper에 따른 판매량의 회귀 분석 결과 그래프
par(mfrow=c(2,2))
plot(lmNewspaper)

# Tv와 Radio광고에 따른 판매량의 회귀 분석 결과 그래프
par(mfrow=c(2,2))
plot(TvRadio_sales)

# Tv와 Newspaper광고에 따른 판매량의 회귀 분석 결과 그래프
par(mfrow=c(2,2))
plot(TvNewspaper_sales)

# Newspaper와 Radio광고에 따른 판매량의 회귀 분석 결과 그래프
par(mfrow=c(2,2))
plot(RadioNewspaper_sales)

# TV와 radio, TV와 radio의 상호작용 광고에 따른 판매량의 회귀 분석 결과 그래프
par(mfrow=c(2,2))
plot(TvandRadio_sales)

#4. 가장 정확도가 큰 예측 모델인 TvandRadio_sale의 예측 수행, 예측된 값에 대한 검정 수행
set.seed(100)

trainingRowIndex = sample(seq_len(nrow(advertising)), 0.6 * nrow(advertising))
trainingData = advertising[trainingRowIndex, ]
testingData = advertising[-trainingRowIndex, ]


lmMod = lm(sales ~ radio*TV ,data= trainingData)

distPred = predict (lmMod, testingData)

actuals_preds = data.frame(cbind(actuals = testingData$sales , predict = distPred))
head(actuals_preds)

correlation_accuracy = cor(actuals_preds)
correlation_accuracy
#actuals와 predict는 0.977672 관련이 있음

