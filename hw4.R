#  hw 4 for time series class
# (1) Download the file strikes.datPreview the documentView in a new window from the Data 
# folder. It contains the number of strikes per year in the U.S. between 1951 and 1980. 
# Plot the data and apply the two-sided moving average filters with q=2,4,5 to the data, 
# closely following the R commands introduced in Section 1.3. of the Lecture Notes. 
# 
# (2) To familiarize yourselves more with the various methods of trend removal, please 
# go to https://trends.google.com/trends/ (Links to an external site.)Links to an external
# site. and search the phrase "time series analysis". Download the available CSV file and 
# apply any of the three methods of Section 1.3 to your data.

# 1.
# set working directory
getwd()
setwd("/home/yunzheli/class/sta137/Data/")

# read data
strikes_raw = read.table(file = "strikes.dat")

# convert to time series data
strikes = ts(strikes_raw, start = 1951)

# set time variable
t = 1 : length(strikes)

# origin plot
plot(strikes, xlab = "", ylab = "", main = "", type = "l")

# set filter
ma2 = filter(strikes, sides = 2, rep(1,5) / 5)
ma4 = filter(strikes, sides = 2, rep(1,9) / 9)
ma5 = filter(strikes, sides = 2, rep(1,11) / 11)

lines(t, ma2, col = "red")
lines(t, ma4)
lines(t, ma5)



# 2.
data = read.table("multiTimeline.csv", stringsAsFactors = FALSE, fill = TRUE, skip = 3)
data = data[[1]]

# clean data
# seperate string with ','
sep_data = lapply(data, function(x) strsplit(x, ","))

# unlist
sep_data = unlist(sep_data)

# choose numbers
tsdata = sep_data[2*(1:(length(sep_data)/2))]
date_data = sep_data[2*(1:(length(sep_data)/2)) - 1]
tsdata = as.numeric(tsdata)
date_data = as.Date(date_data)

# set t
t = 1 : length(tsdata)
# plot
plot(t, tsdata, xlab = "", ylab = "", main = "")

# another way to build time series model
plot(tsdata~date_data, xlab = "", ylab = "", main = "Data is from 2016-10-09 to 2017-10-01")

# method 1: Least squares estimation
# try polynomial function with different parameters
# based on the scatter plot we choose to start with
# quadratic regression

qtfit = lm(tsdata~poly(t,2,raw = TRUE))
lines(qtfit$fitted.values)
cbfit = lm(tsdata~poly(t,3,raw = TRUE))
lines(cbfit$fitted.values)
random = lm(tsdata~poly(t,6,raw=TRUE))
lines(random$fitted.values)
# Q: how should we choose the regression model when quadratic and cubic do not have
# much difference, but it goes larger when power becomes higher

# method 2: Smoothing with Moving Averages
# a) two sides
ma_two_sides_2 = filter(tsdata, sides = 2, filter = rep(1,5)/5)
plot(t, ma_two_sides_2, xlab = "", ylab = "", main = "", type = "l")
ma_two_sides_5 = filter(tsdata, sides = 2, filter = rep(1,11)/11)
lines(t, ma_two_sides_5)
# b) one side
ma_one_side_2 = filter(tsdata, sides = 1, rep(1,3) / 3)
lines(t, ma_one_side_2)

# method 3: Differencing
d1 = diff(tsdata)
d2 = diff(d1)
par(mfrow = c(1, 2))
plot.ts(d1, xlab = "", ylab = "")
plot.ts(d2, xlab = "", ylab = "")
