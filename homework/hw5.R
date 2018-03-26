# Date: Oct. 10 
# Download the file accidents.datPreview the documentView in a new window. 
# It contains the monthly accidental deaths in the United States from 1973 to 1978. 
# Analyze the data by performing the following tasks.
# (a) Plot the data;
# (b) Find seasonality estimates for s_t in the decomposition
# Y_t=m_t+s_t+X_t using method 1 or 2 from Section 1.4;
# (c) Plot the deseasonalized data Y_t-\hat{s}_t;
# (d) Fit a parabola by least squares to the deseasonalized data
# and use it as estimate of the trend m_t;
# (e) Plot the residuals \hat{X}_t=Y_t-\hat{m}_t-\hat{s}_t.


setwd("/home/yunzheli/class/sta137/Data/")
accidents = read.table("accidents.dat", stringsAsFactors = FALSE)
accidents = ts(accidents, start = 1973, freq = 12)

# (a) plot the data
plot(accidents, ylab = "", type = 'l')
# obvious seasonality but not obvious trend
# Q: should we use sine/cosine as trend for this kind of data?
# A: it is very rare to have the data that fit sine/consine line perfectly



# (b) Find seasonality estimates for s_t in the decomposition
# Y_t=m_t+s_t+X_t using method 1 or 2 from Section 1.4;

# method 1: small trend method
d = 12
N = length(accidents)/d
# estimation of trend for every year
m = sapply(1:N, function(x) sum(accidents[(x-1)*d + 1:d])/d)
# estimation of seasonality for each month
s = sapply(1:d, function(x) sum(accidents[x + ((1:N) -1)*d] - m[1:N]) / N)
# verify
sum(s) #  -2.472689e-12, small enough to be considered as 0,
       # error is due to computation of floating numbers

# seasonality
plot(ts(rep(s,N), start = 1973, freq = 12), type = 'l', xlab = "", ylab = "")


# method 2: moving average estimation

# convert original data to a matrix
Y = matrix(accidents, ncol = 12, byrow = TRUE)

# moving average
m_hat = filter(accidents, sides = 2, c(1/2, rep(1,11), 1/2)/12)
M = matrix(m_hat, ncol = 12, byrow = TRUE)


MU = apply(t(Y - M),1, function(x) sum(x, na.rm = TRUE) / (N-1))
MU_hat = matrix(rep(MU, N), ncol = d, byrow = TRUE)

# rearrange the trend
new_S = MU_hat - mean(MU)
new_M = M + mean(MU)

# note: for converting a matrix to a vector and by rows
# we need to use function as.vector(t(Matrix))

# sesonality
new_S_t = ts(as.vector(t(new_S)), start = 1973, freq = 12 )
plot(new_S_t)

# new trend
new_M_t = ts(as.vector(t(new_M)), start = 1973, freq = 12 )
plot(accidents, ylab = "", type = 'l')
lines(new_M_t, col = "green")

# (c) Plot the deseasonalized data Y_t-\hat{s}_t;
ds_Y = Y - new_S
ds_Y_t = ts(as.vector(t(ds_Y)), start = 1973, freq = 12)
plot.ts(res_t, xlab = "",ylab = "")

# Q why there are 0.5
acf(accidents)
# A: because the frequency is 12

# (d) Fit a parabola by least squares to the deseasonalized data
# and use it as estimate of the trend m_t;
t = 1:length(ds_Y_t)
pbfit = lm(ds_Y_t~I(t)+I(t^2)+I(t^3))
plot.ts(res_t, xlab = "",ylab = "")

plot(pbfit$fitted.values, type = "l")

# (e) Plot the residuals \hat{X}_t=Y_t-\hat{m}_t-\hat{s}_t.
res = Y - new_S - new_M
res_t = ts(as.vector(t(res)), start = 1973, freq = 12)
plot(res_t, xlab="", ylab="")
