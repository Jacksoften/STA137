# Author: Yunzhe Li
# Date: 10/25
# Description:
# Analysis of online game - League of Legends searched on Google
# Data provider: Google Trend


setwd("/home/yunzheli/class/sta137")

lol = read.table("leagueoflegends.csv", stringsAsFactors = FALSE, skip = 3)
lol = lol[[1]]
# dummy variable for championship month

# clean data
# seperate string with ','
sep_lol = lapply(lol, function(x) strsplit(x, ","))

# unlist
sep_lol = unlist(sep_lol)

# choose numbers
lol_num = sep_lol[2*(1:(length(sep_lol)/2))]
lol_t = ts(lol_num, start = 2004, end = 2017, freq = 12)
plot(lol_t, main = "League of Legends", sub = "made by Yunzhe Li", xlab = "time", ylab = "frequnce")

# by looking at the plot, I am only interested in the data after 2010
# or in another words, the data which is larger than 10

filters = which(as.numeric(lol_num) > 10)
lol[filters]

lol_num_filtered = as.numeric(lol_num[filters])
lol_t_filtered = ts(lol_num_filtered, start = 2010, freq = 12)
plot(lol_t_filtered, main = "League of Legends", sub = "made by Yunzhe Li", xlab = "time", ylab = "frequnce")

t = 1:length(lol_num_filtered)
fitmodel = lm(lol_t_filtered~poly(t, degree = 5))
plot(t, lol_t_filtered, type= "l", xlab = "", ylab = "" )
lines(fitmodel$fitted.values)

diff_value = lol_num_filtered - qtmodel$fitted.values
plot(t, diff_value, type = "l")

acf(lol_t_filtered)

# due to the length, we cannot make a matrix for this data set
# so I decide to elimilate the first 10 data points
updated_data = lol_t_filtered[-(1:10)]
Y = matrix(updated_data, ncol = 12, byrow = TRUE)
new_t = 1 : length(updated_data)
new_model = lm(updated_data~poly(new_t, degree = 3))
M = matrix(new_model$fitted.values, ncol = 12, byrow = TRUE)

Y - M
mu = apply(t(Y-M), 1, function(x) sum(x)/7)
MU = matrix(rep(mu,7), ncol = 12, byrow = TRUE)
plot(rep(mu, 12), type = "l")
mu_bar = mean(mu)
S = MU + mu_bar
res = Y - M - S
plot(as.vector(res))
lines(as.vector(res))

