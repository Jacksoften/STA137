# Author: Yunzhe Li
# Date: 10/25
# Description:
# Analysis of online game - League of Legends searched on Google
# Data provider: Google Trend
setwd("./file")
data = readRDS("league")
num = as.numeric(data[,1])
dat = data[,2]
lol_t = ts(num, start = c(2009,11), freq = 12)
plot(lol_t, main = "League of Legends",
     sub = "made by Yunzhe Li", xlab = "time", ylab = "frequnce")

# polynomial fitting

# checking different models
models = lapply(1:6, function(x) lm(lol_t~poly(t, degree = x)))
sapply(models, AIC)
sapply(models, BIC)

# choose power 5
t = 1:length(num)
fitmodel = lm(lol_t~poly(t, degree = 5))
plot(t, lol_t, type= "l", xlab = "", ylab = "" )
lines(fitmodel$fitted.values)

# try to de-seaonality
de_trend = lol_t-fitmodel$fitted.values
de_trend_matrix = matrix(c(rep(NA,10),de_trend,rep(NA,2)), ncol = 12, byrow = TRUE)
trend_mean = apply(t(de_trend_matrix),1,function(x) mean(x, na.rm = TRUE))
seasonality_matrix = rep(trend_mean, nrow(de_trend_matrix)-1)
plot(t, seasonality_matrix, type = "l")
de_seasonality = de_trend - seasonality_matrix

plot(t, lol_t, type= "l", xlab = "", ylab = "" )
lines(seasonality_matrix + fitmodel$fitted.values, col = "red")

plot(t, de_seasonality, type = "l")

# By looking at the graph, we realize that there are some peak happens seasonally.
# After searching online, we realize that those might be caused by world championship
# which happends at sep. to oct. The championship started from 2011
# S1: Jun. # S2: Oct. # S3: Sep-Oct. # S4: Sep-Oct # S5: Oct # S6: Oct # S7: Oct

# design matrix.
championShip = c(sprintf("201%d-09", 3:4), sprintf("201%d-10", c(2:3,5:7)))
champ_indices = as.vector(sapply(championShip, function(x) grep(x,dat)))
num[champ_indices]
first_col = rep(0,length(num))
first_col[champ_indices] = 1
selected_model = lm(lol_t~first_col+poly(t,degree = 5))
AIC(selected_model)
BIC(selected_model)

plot(t, lol_t, type= "l", xlab = "", ylab = "" )
lines(selected_model$fitted.values, col = "blue")

# de-trend
res = lol_t - selected_model$fitted.values

# de-seasonality
de_trend_matrix2 = matrix(c(rep(NA,10),res,rep(NA,2)), ncol = 12, byrow = TRUE)
trend_mean2 = apply(t(de_trend_matrix2),1,function(x) mean(x, na.rm = TRUE))
seasonality2 = rep(trend_mean2, nrow(de_trend_matrix2)-1)
plot(t, seasonality2, type = "l")
de_seasonality2 = res - seasonality2

plot(t, lol_t, type= "l", xlab = "", ylab = "" )
lines(selected_model$fitted.values+seasonality2, col = "blue")

plot(t, de_seasonality2, type = 'l')

# try to put the dummy variable into the seasonality part.


# checking the normality of the residule
qqnorm(res) # checking the usage of Q-Q plot
qqline(res)
hist(res, breaks = 20)

qqnorm(de_seasonality2)
qqline(de_seasonality2)
hist(de_seasonality2, breaks = 20)

# checking the stationarity of the rest part
plot(res, type = "l")
par(mfrow = c(2,2))
acf(res) # MA 1
pacf(res) # AR 1

acf(de_seasonality2)
pacf(de_seasonality2)

# fitModel1 = arima(res, order = c(1,0,0))
# fitModel2 = arima(res, order = c(1,0,1))
# plot(fitModel1$residuals)
# lines(fitModel2$residuals, col = "green")
# lines(fitModel1$residuals, col = "yellow")
# fitModel1$aic
# fitModel2$aic

# using package "forecast" to get pi's and theta's
library(forecast)
fitModel = auto.arima(res)
plot(res, type = "l")
lines(fitModel$fitted, col = "red")

phi1 = fitModel$coef
names(phi1) = NULL

last_element = res[length(res)]
names(last_element) = length(res) + 1
predic = phi1*last_element
res_new = c(res, predic)
for(i in 1:10)
{
  predic = phi1*res_new[length(res_new)]
  names(predic) = length(res_new) + 1
  res_new = c(res_new, predic)
}
plot(res_new, type = "l")
# checking different models

