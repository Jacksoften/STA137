# 137 time series hw3

# read data
lake = read.table(file = "/home/yunzheli/class/sta137/lake.dat")

# find the trend line

# convert data to R time series objects
lakeModel = ts(lake, start = 1985)

# set variabe t
t = 1 : length(lakeModel)

# simple linear regression
lsfit = lm(lakeModel~t)

# plotting
plot(t, lakeModel, xlab = "", ylab = "", main = "", type = "l")
lines(lsfit$fit, col = "blue")

ma2 = filter(lakeModel, sides = 2, filter = rep(1,5) / 5) # 5 = 2*2 + 1, q = 2
ma10 = filter(lakeModel, sides=2, rep(1,21)/21) # 21 = 2*10 + 1, q = 10
ma35 = filter(lakeModel, sides=2, rep(1,71)/71) # 71 = 2*35 + 1, q = 35
plot(t, ma2, xlab = "", ylab = "", type = "l")
lines(t, ma10)  
lines(t, ma35)

# with quadratic polynomials
qdfit = lm(lakeModel~t + I(t^2))
plot(t, lakeModel, xlab = "", ylab = "", main = "", type = "l")
lines(qdfit$fit)
# alternative method
# try = lm(lakeModel~poly(t,2,raw = TRUE))
# lines(try$fitted.values)


# with cubic polynomials
cbfit = lm(lakeModel~I(t) + I(t^2) + I(t^3))
lines(cbfit$fit)
summary(qdfit)

# spencer's 15-point moving average
a =c(-3, -6, -5, 3, 21, 46, 67, 74, 67, 46, 21, 3, -5, -6, -3)/320
s15 = filter(data, sides=2, a) # incomplete code, when data is unknown.

