#  hw 4 for time series class

# 1.
getwd()
setwd("/home/yunzheli/class/sta137/Data/")
strikes_raw = read.table(file = "strikes.dat")
strikes = ts(strikes_raw, start = 1951)
t = 1 : length(strikes)

ma2 = filter(strikes, sides = 2, rep(1,5) / 5)
ma4 = filter(strikes, sides = 2, rep(1,9) / 9)
ma5 = filter(strikes, sides = 2, rep(1,11) / 11)
plot(t, ma2, xlab = "", ylab = "", main = "", type = "l")
lines(t, ma4)
lines(t, ma5)

# 2.
data = read.table("multiTimeline.csv", stringsAsFactors = FALSE, fill = TRUE, skip = 3)
# clean data
tsdata = lapply(data[[1]], function(x) strsplit(x, ","))
tsdata = unlist(tsdata)
tsdata = tsdata[2*(1:(length(tsdata)/2))]
tsdata = as.numeric(tsdata)
t = 1 : length(tsdata)
plot(t, tsdata)