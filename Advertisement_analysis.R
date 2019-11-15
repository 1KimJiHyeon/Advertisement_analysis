library("dplyr")
library("ggplot2")
library('plotly')

advertising = read.csv("ad.csv")
head(advertising)
summary(advertising)
#sales�� �������� �����̰� TV,radio,newspaper�� �������� �����̴�.

#1. �������� Ư�� �ľ�

#1-1. �� �������� ������

# Tv ������ �������� sales�� ���ٴ� ���̴�. �� ���� ���� �Ǹŷ��̴�.
qplot( x = TV, y = sales, data = advertising)+ geom_point()+ geom_smooth(method = "lm", formula = y ~ x)

# radio ������ �������� sales�� ���ٴ� ���̴�. �� ���� ���� �Ǹŷ��̴�.
qplot( x = radio, y = sales, data = advertising)+ geom_point()+ geom_smooth(method = "lm", formula = y ~ x)

# newspaper ������ �������� sales�� ���ٴ� ���̴�. �� ���� ���� �Ǹŷ��̴�.
qplot( x = newspaper, y = sales, data = advertising)+ geom_point()+ geom_smooth(method = "lm", formula = y ~ x)


# 1-2. �������� ���谡 �������� Ȯ��

# TV�� �Ǹ��� ����� �����̴�.
advertising %>%
  ggplot(aes(x = TV, y = sales)) +
  geom_point(colour = "red", shape = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black")

# Radio �Ǹ��� ����� �����̴�.
advertising %>%
  ggplot(aes(x = radio, y = sales)) +
  geom_point(colour = "red", shape = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black")

# Newspaper �Ǹ��� ����� �����̴�.
advertising %>%
  ggplot(aes(x = newspaper, y = sales)) +
  geom_point(colour = "red", shape = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black")

# 1-3. ���� ������ ���� ���� ������ ��� ���� �� �� ���̿� 3D �������� �ۼ��Ѵ�. 
# radio�� sales, TV�� sales�� ������谡 ���������� �����Ǿ� ������ ��Ÿ����. radio�� TV ������ �����Կ� ����, sales�� �����Ѵ�.
plot_ly(advertising, x = ~radio, y = ~TV, z = ~sales, marker = list(size = 5)) 


#2. ���� ������ ���Ӻ��� ������ ������� Ȯ�� 
# TV�� �Ǹ��� �������� 1 0.7822244�� ����� ���ϸ�, ������ �Ǹ��� �������� 0.5762226�� ������谡 �����̰�, �Ź����� �Ǹ��� �������� 0.228299�� ������谡 ���ϴ�.

advertising %>%
  summarize(r_TV = cor(TV, sales),
            r_Radio = cor(radio, sales),
            r_Newspaper = cor(newspaper, sales))

#3. ���� ȸ�� �� ����
#3-1. �ܼ� ���� ȸ�� ��

#TV ������ sales�� ��ġ�� ���� 
lmTV = lm(sales ~ TV, data = advertising)


#radio�� ������ sales�� ��ġ�� ���� 
lmRadio <- lm(sales ~ radio, data = advertising)

# newspaper ������ sales�� ��ġ�� ���� 
lmNewspaper <- lm(sales ~ newspaper, data = advertising)

# ȸ�� �м� ��� Ȯ��

# TV�� p-value ���� < 2.2e-16�� 0.05���� ���� �����Ƿ� �븳������ ä���Ѵ�.�� ���� ����(TV)�� ���� ����(sales)�� ������ �ش�(��������� ���ǹ��ϴ�).Adjusted R-squared�� 0.6099�̹Ƿ� 100���� 60���� �ش��Ѵ�.
# TV�� ���� �Ǹŷ� = 7.03259+0.04754*TV +ei
summary(lmTV)

# TV�� 1000�϶� ���� �Ǹŷ� = 54.56923
predict(object = lmTV, newdata = data.frame(TV = 1000))


# Radio�� p-value ����< 2.2e-16�� 0.05���� ���� �����Ƿ� �븳������ ä���Ѵ�.�� ���� ����(Radio)�� ���� ����(sales)�� ������ �ش�(��������� ���ǹ��ϴ�).Adjusted R-squared�� 0.3287�̹Ƿ� 100���� 32���� �ش��Ѵ�.
# radio�� ���� �Ǹŷ� = 9.3116+0.2025*radio +ei
summary(lmRadio)

# radio�� 1000�϶� ���� �Ǹŷ� = 211.8074
predict(object = lmRadio, newdata = data.frame(radio= 1000))

# Newspaper�� p-value ���� 0.001148�� 0.05���� �����Ƿ� �븳������ ä���Ѵ�.�� ���� ����(newspaper)�� ���� ����(sales)�� ������ �ش�(��������� ���ǹ��ϴ�).Adjusted R-squared�� 0.04733�̹Ƿ� 100���� 4���� �ش��Ѵ�.
# Newspaper�� TV�� radio�� ���� p-value�� ���� ���̶� TV�� radio�� ���� sales�� ������ ũ�� �ʴ�. ������ TV�� radio��sales�� ������ ũ��.
# Newspaper�� ���� �Ǹŷ� = 12.35141+0.05469*newspaper +ei
summary(lmNewspaper)

# newspaper�� 1000�϶� ���� �Ǹŷ� = 67.04451
predict(object = lmNewspaper, newdata = data.frame(newspaper = 1000))

# 3-2. ���� ���� ȸ�� ����

#TV�� Newspaper�� ���� ������ �ΰ� ���� ����(sales)�� � ������ �ִ��� �˾ƺ��Ҵ�.
TvNewspaper_sales <- lm(sales ~ newspaper + TV ,data= advertising )

# Newspaper+TV�� p-value ����  < 2.2e-16�̸� 0.05���� �����Ƿ� �븳������ ä���Ѵ�.�� ���� ����(Newspaper + TV)�� ���� ����(sales)�� ������ �ش�(��������� ���ǹ��ϴ�).Adjusted R-squared�� 0.6422�̹Ƿ� 100���� 64���� �ش��Ѵ�.
# TV�� Newspaper�� ���� ���� �Ǹŷ� = 5.774948 + 0.044219 * (newspaper) + 0.046901 * (TV)
summary(TvNewspaper_sales)

# TV�� 1000�� newspaper�� 1000�϶� ���� �Ǹŷ� = 96.89559
predict(object = TvNewspaper_sales, newdata = data.frame(TV = 1000 , newspaper = 1000))


#Radio�� Newspaper�� ���� ������ �ΰ� ���� ����(sales)�� � ������ �ִ��� �˾ƺ��Ҵ�.
RadioNewspaper_sales <- lm(sales ~ newspaper + radio ,data= advertising )

# Newspaper+Radio�� p-value ����  < 2.2e-16�̸� 0.05���� �����Ƿ� �븳������ ä���Ѵ�.�� ���� ����(Newspaper + Radio)�� ���� ����(sales)�� ������ �ش�(��������� ���ǹ��ϴ�).Adjusted R-squared�� 0.3259�̹Ƿ� 100���� 32���� �ش��Ѵ�.
# Radid�� Newspaper�� ���� ���� �Ǹŷ� = 9.188920 + 0.006644 * (newspaper) + 0.199045 * (radio)
summary(RadioNewspaper_sales)

# radio�� 1000�� newspaper�� 1000�϶� ���� �Ǹŷ� = 214.8777
predict(object = RadioNewspaper_sales, newdata = data.frame(radio= 1000, newspaper = 1000))

#��� ������ ���� ���Ҵ� newspaper�� ���� TV�� radio�� ���� ������ �ΰ� ���� ������ � ������ �ִ��� �˾ƺ��Ҵ�.
TvRadio_sales <- lm(sales ~ radio + TV ,data= advertising )

# radio+TV�� p-value ����  < 2.2e-16�̸� 0.05���� �����Ƿ� �븳������ ä���Ѵ�.�� ���� ����(radio + TV)�� ���� ����(sales)�� ������ �ش�(��������� ���ǹ��ϴ�).Adjusted R-squared�� 0.8962�̹Ƿ� 100���� 89���� �ش��Ѵ�.
# radio+TV�� Adjusted R-squared��  0.8962��  radio , tv , newspaper�� ���� �ϴ� �� ���� ũ�⶧���� �� ����.
# TV�� radio�� ���� ���� �Ǹŷ� = 2.92110 + 0.18799 * (radio) + 0.04575 * (TV)
summary(TvRadio_sales)

# TV�� 1000�� radio�� 1000�϶� ���� �Ǹŷ� = 236.6701
predict(object = TvRadio_sales, newdata = data.frame(TV = 1000 , radio= 1000))


# TV�� radio, TV�� radio�� ��ȣ�ۿ��� sales�� ��ġ�� ����
TvandRadio_sales <- lm(sales ~ radio * TV ,data= advertising )

summary(TvandRadio_sales)

predict(object = TvandRadio_sales, newdata = data.frame(TV = 1000 , radio= 1000))


#��� ���� ���� ���� �� ���� ������ ����Ͽ� ���� ���� ȸ�� ���� ����

all_sales <- lm( sales ~ TV+newspaper+radio,data= advertising)

#radio+TV+newspaper�� p-value ����  < 2.2e-16�̸� 0.05���� �����Ƿ� �븳������ ä���Ѵ�. �� ���� ����(radio + TV+newspaper)�� ���� ����(sales)�� ������ �ش�(��������� ���ǹ��ϴ�).Adjusted R-squared�� 0.8956�̹Ƿ� 100���� 89���� �ش��Ѵ�.
#TV�� radio�� newspaper�� ���� ���� �Ǹŷ�  = 3.0052094 + 0.1883832 * (radio) + 0.0457759 * (TV) -0.0012433 * (newspaper)
summary(all_sales)

# radio�� 1000�� TV�� 1000�� newspaper�� 1000�϶� ���� �Ǹŷ� = 236.1961
predict(object = all_sales, newdata = data.frame(TV = 1000 , radio= 1000, newspaper = 1000))




#ȸ�� �м� ����� �׷����� Ȯ��

#��� ���� ���� ������ ȸ�� �м� ��� �׷���
par(mfrow=c(2,2))
plot(all_sales)

# Tv�� ���� �Ǹŷ��� ȸ�� �м� ��� �׷���
par(mfrow=c(2,2))
plot(lmTV)

#radio�� ���� �Ǹŷ��� ȸ�� �м� ��� �׷���
par(mfrow=c(2,2))
plot(lmRadio)

# newspaper�� ���� �Ǹŷ��� ȸ�� �м� ��� �׷���
par(mfrow=c(2,2))
plot(lmNewspaper)

# Tv�� Radio������ ���� �Ǹŷ��� ȸ�� �м� ��� �׷���
par(mfrow=c(2,2))
plot(TvRadio_sales)

# Tv�� Newspaper������ ���� �Ǹŷ��� ȸ�� �м� ��� �׷���
par(mfrow=c(2,2))
plot(TvNewspaper_sales)

# Newspaper�� Radio������ ���� �Ǹŷ��� ȸ�� �м� ��� �׷���
par(mfrow=c(2,2))
plot(RadioNewspaper_sales)

# TV�� radio, TV�� radio�� ��ȣ�ۿ� ������ ���� �Ǹŷ��� ȸ�� �м� ��� �׷���
par(mfrow=c(2,2))
plot(TvandRadio_sales)

#4. ���� ��Ȯ���� ū ���� ���� TvandRadio_sale�� ���� ����, ������ ���� ���� ���� ����
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
#actuals�� predict�� 0.977672 ������ ����
