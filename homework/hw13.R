# Date: 10/26/2017

#Simulate AR(1) processes X_t = \phi X_{t-1} + Z_t, t=1, ..., 1000,
# for standard normally distributed WN Z_t and \phi = -.9, ..., 0, ..., +.9. 
# (1) Plot the simulated time series.# 
# (2) Plot the sample ACF using the R function acf. 
# (3) Compute CI's for the mean \mu = 0 and compare.
set.seed(1995)
Z = rnorm(1000,0,1)
x = NULL
x[1] = 1
for(i in 2:1000)
{
  x[i] = x[i-1] + Z[i]
}
plot(x, type = 'l')
acf(x)
