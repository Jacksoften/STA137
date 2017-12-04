# Author: Yunzhe Li
# Date: 10/25
# Description:
# Analysis of online game - League of Legends searched on Google
# Data provider: Google Trend
setwd("class/sta137/leagueOfLegends/file/")
data = readRDS("league")
num = as.numeric(data[,1])[1:84]
num_pred = as.numeric(data[,1])[85:96]
dat = data[,2][1:84]
dat_pred = data[,2][85:96]
lol_t = ts(num, start = c(2009,11), freq = 12)
plot(lol_t, main = "League of Legends", xlab = "time", ylab = "frequnce")

# method 1
# polynomial fitting

# checking different models
t = 1:length(num)
models = lapply(1:6, function(x) lm(lol_t~poly(t, degree = x)))
sapply(models, AIC)
sapply(models, BIC)

# choose power 5
fitmodel = lm(lol_t~poly(t, degree = 5))
plot(t, lol_t, type= "l", xlab = "time", ylab = "popularity", main = "original plot with power 5 polynormial fitting line")
lines(fitmodel$fitted.values)

# try to de-seaonality
de_trend1 = lol_t-fitmodel$fitted.values
plot(de_trend1, type = 'l', main = "After Detrend")
de_trend_matrix1 = matrix(c(rep(NA,10),de_trend1,rep(NA,2)), ncol = 12, byrow = TRUE)
trend_mean1 = apply(t(de_trend_matrix1),1,function(x) mean(x, na.rm = TRUE))
seasonality_matrix1 = rep(trend_mean1, nrow(de_trend_matrix1)-1)
plot(t, seasonality_matrix1, type = "l", main = "Seasonality")
de_seasonality1 = de_trend1 - seasonality_matrix1

plot(t, lol_t, type= "l", xlab = "", ylab = "", main = "Treand With Seasonality")
lines(seasonality_matrix1 + fitmodel$fitted.values, col = "red")

# checking residules
plot(t, de_seasonality1, type = "l") # the left part looks stationary
qqnorm(de_seasonality1)
qqline(de_seasonality1)
hist(de_seasonality1, main = "residual")

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
championShip = c(sprintf("201%d-09", 3:4), sprintf("201%d-10", c(2:3,5:6)))
champ_indices = as.vector(sapply(championShip, function(x) grep(x,dat)))
num[champ_indices]
first_col = rep(0,length(num))
first_col[champ_indices] = 1
selected_model = lm(lol_t~first_col+poly(t,degree = 5))
selected_model_fixed = lm(lol_t~first_col+poly(t,degree = 5, raw = TRUE)) 
# raw = TRUE means not change the basis to orthonormal basis
AIC(selected_model)
BIC(selected_model)

plot(t, lol_t, type= "l", xlab = "time", ylab = "freq", main = "Power 5 Polynormial Fitting with Spikes")
lines(selected_model$fitted.values)

# plot(selected_model$fitted.values, type = 'l')

# de-trend
res = lol_t - selected_model$fitted.values

# de-seasonality
plot(res, type="l", main = "After detrend")
de_trend_matrix2 = matrix(c(rep(NA,10),res,rep(NA,2)), ncol = 12, byrow = TRUE)
trend_mean2 = apply(t(de_trend_matrix2),1,function(x) mean(x, na.rm = TRUE))
seasonality2 = rep(trend_mean2, nrow(de_trend_matrix2)-1)
plot(t, seasonality2, type = "l", main = "Seasonality")
de_seasonality2 = res - seasonality2

plot(t, lol_t, type= "l", xlab = "", ylab = "", main = "Adding seasonality")
lines(selected_model$fitted.values+seasonality2, col = "blue")

# plot(selected_model$fitted.values+seasonality2, type = "l")

plot(t, de_seasonality2, type = 'l')

plot(t, res, type = 'l')
lines(t,de_seasonality2, col = "blue")
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
par(mfrow = c(1,1))
acf(res, main="ACF") # MA 1
pacf(res, main="PACF") # AR 1

acf(de_seasonality2)
pacf(de_seasonality2)

# using package "forecast" to get pi's and theta's
par(mfrow = c(1,1))
library(forecast)
fitModel = auto.arima(res)
plot(res, type = "l", main = "Stationary Series with ARMA estimation")
lines(fitModel$fitted, col = "red")

# prediction for stationary part
pred_s = forecast(fitModel, h=12, level = c(0.95))
rand_pred = data.frame(t = pred_s$mean, t = pred_s$lower, t = pred_s$upper)

# oct 2017 is another championship month
new_time = (length(num)+1):(length(num)+12)
new_data = data.frame(t = new_time)

spike_vector = c(rep(0,11),1) # Oct. is championship month, which is the last one in prediciton month

trymodel = lm(lol_t~first_col+t+I(t^2)+I(t^3)+I(t^4)+I(t^5))
M = matrix(c(rep(1,12), spike_vector, new_time, new_time^2, new_time^3, new_time^4, new_time^5), ncol = 7)
V = matrix(trymodel$coefficients)
smooth_pred = M%*%V
whole_pred = sapply(1:3, function(k) rand_pred[,k]+smooth_pred)


plot(t, lol_t, type= "l", xlab = "", ylab = "", xlim = c(0,100), main = "Prediction")
lines(new_time, whole_pred[,1], col="blue")
lines(new_time, whole_pred[,2], col="red")
lines(new_time, whole_pred[,3], col="red")

lines(new_time, num_pred, col = "brown")

# not a good example
# some = auto.arima(num)
# plot(forecast(some, h=12))



# spectral analysis
plot(t,res,type='l')
new_res = num - trymodel$fitted.values
spec = spec.pgram(new_res, taper = 0, log = "no", main = "Raw Periodogram")
index = which(spec$spec>40)[-2]
abline(v=spec$freq[index[1]], lty = 2)
abline(v=spec$freq[index[2]], lty = 2)


my_freq = spec$freq[index]
lower = index - 1
upper = index + 1
new_index = c(index, lower, upper)
sum(spec$spec[new_index])/sum(spec$spec)


cos = sapply(my_freq, function(x) cos(2*pi*t*(x)))
sin = sapply(my_freq, function(x) sin(2*pi*t*(x)))

reg = lm(res~cos[,1]+sin[,1]+cos[,2]+sin[,2])
plot(reg$fitted.values, type = 'l')

plot(t,res,type='l', main = "Stationary series with estimation")
lines(reg$fitted.values, type = 'l', col = "red")


# prediction
new_coef = reg$coefficients
b0 = new_coef[1]
A1 = new_coef[2]
B1 = new_coef[3]
A2 = new_coef[4]
B2 = new_coef[5]
new_pred = b0 + A1*cos(2*pi*new_time*spec$freq[index[1]]) +
               B1*sin(2*pi*new_time*spec$freq[index[1]]) +
               A2*cos(2*pi*new_time*spec$freq[index[2]]) +
               B2*sin(2*pi*new_time*spec$freq[index[2]])
plot(t,res,type = 'l', xlim = c(0,100))
lines(t, reg$fitted.values, col = "blue")
lines(new_time, new_pred, col="red")

whole_pred_new = new_pred + smooth_pred

plot(t, lol_t, type= "l", xlim = c(0,100), main = "Predictoin", xlab = "time", ylab = "freq")
# lines(new_time, whole_pred[,1], col="blue")
lines(new_time, whole_pred_new, col="red")
lines(new_time, num_pred)

plot(t, selected_model_fixed$fitted.values+reg$fitted.values, type = 'l')
plot(t, lol_t, type = "l")
lines(t, selected_model_fixed$fitted.values+reg$fitted.values, type = 'l', col = "red")

# further prediction
new_time_more = c(new_time, new_time + 12)

pred_s_more = forecast(fitModel, h=24, level = c(0.95))
rand_pred_more = data.frame(t = pred_s_more$mean, t = pred_s_more$lower, t = pred_s_more$upper)

M_more = matrix(c(rep(1,24), rep(spike_vector,2), new_time_more, new_time_more^2, new_time_more^3, new_time_more^4, new_time_more^5), ncol = 7)
V_more = matrix(trymodel$coefficients)
smooth_pred_more = M_more%*%V_more
whole_pred_more = sapply(1:3, function(k) rand_pred_more[,k]+smooth_pred_more)

new_pred_more = b0 + A1*cos(2*pi*new_time_more*spec$freq[index[1]]) +
  B1*sin(2*pi*new_time_more*spec$freq[index[1]]) +
  A2*cos(2*pi*new_time_more*spec$freq[index[2]]) +
  B2*sin(2*pi*new_time_more*spec$freq[index[2]])
whole_pred_new_more = new_pred_more + smooth_pred_more


plot(t, lol_t, type= "l", xlim = c(0,110), main = "further Predictoin", xlab = "time", ylab = "freq")
# lines(new_time, whole_pred[,1], col="blue")
lines(new_time_more, whole_pred_new_more, col="red")
lines(new_time, num_pred)


plot(t, lol_t, type= "l", xlab = "", ylab = "", xlim = c(0,110), main = "further Prediction")
lines(new_time_more, whole_pred_more[,1], col="blue")
lines(new_time_more, whole_pred_more[,2], col="red")
lines(new_time_more, whole_pred_more[,3], col="red")

lines(new_time, num_pred)
