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

# method 1
# polynomial fitting

# checking different models
t = 1:length(num)
models = lapply(1:6, function(x) lm(lol_t~poly(t, degree = x)))
sapply(models, AIC)
sapply(models, BIC)

# choose power 5
fitmodel = lm(lol_t~poly(t, degree = 5))
plot(t, lol_t, type= "l", xlab = "", ylab = "" )
lines(fitmodel$fitted.values)

# try to de-seaonality
de_trend1 = lol_t-fitmodel$fitted.values
de_trend_matrix1 = matrix(c(rep(NA,10),de_trend1,rep(NA,2)), ncol = 12, byrow = TRUE)
trend_mean1 = apply(t(de_trend_matrix1),1,function(x) mean(x, na.rm = TRUE))
seasonality_matrix1 = rep(trend_mean1, nrow(de_trend_matrix1)-1)
plot(t, seasonality_matrix1, type = "l")
de_seasonality1 = de_trend1 - seasonality_matrix1

plot(t, lol_t, type= "l", xlab = "", ylab = "" )
lines(seasonality_matrix1 + fitmodel$fitted.values, col = "red")

# checking residules
plot(t, de_seasonality1, type = "l") # the left part looks stationary
qqnorm(de_seasonality1)
qqline(de_seasonality1)
hist(de_seasonality1)

# model selection
acf(de_seasonality1)
pacf(de_seasonality1)
arima(de_seasonality1, order = c(1,0,1))

library(forecast)
auto.arima(de_seasonality1, max.Q = 0, max.P = 0)

ma1 = arima(de_seasonality1, order = c(0,0,1)) # Xt = Zt + 0.4279*Zt-1
plot(fitted(ma1))

matrix(seasonality_matrix1, ncol = 12, byrow = TRUE)
fitted(fitmodel) + unlist(seasonality_matrix1)

# forecast


# method 2

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

plot(selected_model$fitted.values+seasonality2, type = "l")

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

# using package "forecast" to get pi's and theta's
fitModel = auto.arima(res)
plot(res, type = "l")
lines(fitModel$fitted, col = "red")

# prediction for stationary part
pred = predict(fitModel)
pred$pred
