  # Author: Yunzhe Li
# Date: 10/25
# Description:
# Analysis of online game - League of Legends searched on Google
# Data provider: Google Trend
setwd("leagueOfLegends/")
data = readRDS("league")
num = as.numeric(data[,1])
dat = data[,2]
lol_t = ts(num, start = c(2009,11), freq = 12)
plot(lol_t, main = "League of Legends",
     sub = "made by Yunzhe Li", xlab = "time", ylab = "frequnce")

# polynomial fitting
t = 1:length(num)
fitmodel = lm(lol_t~poly(t, degree = 5))
plot(t, lol_t, type= "l", xlab = "", ylab = "" )
lines(fitmodel$fitted.values)
AIC(fitmodel)


models = lapply(1:6, function(x) lm(lol_t~poly(t, degree = x)))
sapply(models, AIC)
sapply(models, BIC)


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


# checking the normality of the residule
qqnorm(res)
qqline(res)
hist(res, breaks = 20)

# checking the stationarity of the rest part
plot(res, type = "l")
acf(res) # MA 1
pacf(res) # AR 1

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
