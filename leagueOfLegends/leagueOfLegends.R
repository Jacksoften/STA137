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
sapply(models, function(x) lines(x$fitted))



# By looking at the graph, we realize that there are some peak happens seasonally.
# After searching online, we realize that those might be caused by world championship
# which happends at sep. to oct. The championship started from 2011
# S1: Jun. # S2: Oct. # S3: Sep-Oct. # S4: Sep-Oct # S5: Oct # S6: Oct # S7: Oct


# design matrix.
championShip = c(sprintf("201%d-09", 3:4), sprintf("201%d-10", (2:7)), "2011-06")
champ_indices = as.vector(sapply(championShip, function(x) grep(x,lol_date_filtered)))
lol_num_filtered[champ_indices]
first_col = rep(0,length(lol_num_filtered))
first_col[champ_indices] = 1
design_M = matrix(c(first_col, 
                    rep(1,length(lol_num_filtered)),
                    t,
                    t^2,
                    t^3),
                  ncol  = 5)
X = as.matrix(lol_num_filtered)
new_x = design_M %*% solve(t(design_M) %*% design_M) %*% t(design_M) %*% X
lines(new_x, col = "blue")

leftover = X - new_x
plot(leftover, type = "l")
acf(leftover)

##########################################################
# there is not a very obvious seasonality, we may not consider it.

# the approach below is not good
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

