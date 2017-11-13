  # Author: Yunzhe Li
# Date: 10/25
# Description:
# Analysis of online game - League of Legends searched on Google
# Data provider: Google Trend


setwd("/home/yunzheli/class/sta137/leagueOfLegends/")

lol = read.table("leagueoflegends.csv", stringsAsFactors = FALSE, skip = 3)
# dummy variable for championship month

# clean data
# seperate string with ','
sep_lol = lapply(lol, function(x) strsplit(x, ","))

# unlist
sep_lol = unlist(sep_lol)

# choose numbers
lol_num = as.numeric(sep_lol[2*(1:(length(sep_lol)/2))])
lol_date = sep_lol[2*(1:(length(sep_lol)/2)) - 1]
lol_t = ts(lol_num, start = 2004, end = 2017, freq = 12)
plot(lol_t, main = "League of Legends", sub = "made by Yunzhe Li", xlab = "time", ylab = "frequnce")


# From wiki, League of Legends was released at October 27, 2009
# by looking at the plot, I am only interested in the data start from 2009-11
# or in another words, the data which is larger than 10

index = grep("2009-10", unlist(lol))
lol_num_filtered = lol_num[-(1:index)]
lol_date_filtered = lol_date[-(1:index)]
names(lol_date_filtered) = NULL
lol_t_filtered = ts(lol_num_filtered, start = c(2009,11), freq = 12)
plot(lol_t_filtered, main = "League of Legends",
     sub = "made by Yunzhe Li", xlab = "time", ylab = "frequnce")

# polynomial fitting
t = 1:length(lol_num_filtered)
fitmodel = lm(lol_t_filtered~poly(t, degree = 5))
plot(t, lol_t_filtered, type= "l", xlab = "", ylab = "" )
lines(fitmodel$fitted.values)
AIC(fitmodel)


models = lapply(1:6, function(x) lm(lol_t_filtered~poly(t, degree = x)))
sapply(models, AIC)
sapply(models, BIC)


# By looking at the graph, we realize that there are some peak happens seasonally.
# After searching online, we realize that those might be caused by world championship
# which happends at sep. to oct. The championship started from 2011
# S1: Jun. # S2: Oct. # S3: Sep-Oct. # S4: Sep-Oct # S5: Oct # S6: Oct # S7: Oct


# design matrix.
championShip = c(sprintf("201%d-09", 3:4), sprintf("201%d-10", c(2:3,5:7)))
champ_indices = as.vector(sapply(championShip, function(x) grep(x,lol_date_filtered)))
lol_num_filtered[champ_indices]
first_col = rep(0,length(lol_num_filtered))
first_col[champ_indices] = 1
selected_model = lm(lol_t_filtered~first_col+poly(t,degree = 5))
AIC(selected_model)
BIC(selected_model)

plot(t, lol_t_filtered, type= "l", xlab = "", ylab = "" )
lines(selected_model$fitted.values, col = "red")
lines(new_x, col = "blue")

# de-trend
res = X - new_x

# checking the normality of the residule
qqnorm(res)
qqline(res)

# checking the stationarity of the rest part
plot(res, type = "l")
acf(res)
pacf(res)

