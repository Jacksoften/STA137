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
data = data[[1]]

# clean data
# seperate string with ','
tsdata = lapply(data, function(x) strsplit(x, ","))

# unlist
tsdata = unlist(tsdata)

# choose numbers
tsdata = tsdata[2*(1:(length(tsdata)/2))]
tsdata = as.numeric(tsdata)

# set t
t = 1 : length(tsdata)

# plot
plot(t, tsdata)
