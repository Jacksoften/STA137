# sta 137 time series
curve(1 + x + x^2, from = 0, to = 100)

curve(1 + x + x^2 + x^3, from = 0, to = 100)


# chapter notes
spots = read.table("/home/yunzheli/class/sta137/sunspots.dat")
spots = ts(spots, start = 1700, frequency = 1)
plot(spots, xlab = "time", ylab = "", main = "Number of sunspots")

setwd("/home/yunzheli/class/sta137/Data/")

lynx = read.table("lynx.dat")
lynx_ts = ts(lynx, start = 1821, frequency = 1)
plot(lynx_ts, xlab = 'time', ylab = '', main = "Number of trapped lynx")

bills03 = read.table("bills03.dat", fill = TRUE, skip = 10)
bills06 = read.table("bills06.dat", fill = TRUE, skip = 10)
bills12 = read.table("bills12.dat", fill = TRUE, skip = 10)

par(mfrow = c(3,1))
plot.ts(bills03, xlab = "(a)", ylab = "", main = "Yields of 3-month Treasury Bills")
plot.ts(bills06, xlab = "(b)", ylab = "", main = "Yields of 6-month Treasury Bills")
plot.ts(bills12, xlab = "(c)", ylab = "", main = "Yields of 12-month Treasury Bills")


read.table("")