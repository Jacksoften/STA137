# Date: Oct. 10 
# Download the file accidents.datPreview the documentView in a new window. It contains the monthly accidental deaths in the United States from 1973 to 1978. Analyze the data by performing the following tasks.
# (a) Plot the data;
# (b) Find seasonality estimates for s_t in the decomposition Y_t=m_t+s_t+X_t using method 1 or 2 from Section 1.4;
# (c) Plot the deseasonalized data Y_t-\hat{s}_t;
# (d) Fit a parabola by least squares to the deseasonalized data and use it as estimate of the trend m_t;
# (e) Plot the residuals \hat{X}_t=Y_t-\hat{m}_t-\hat{s}_t.


setwd("/home/yunzheli/class/sta137/Data/")
accidents = read.table("accidents.dat", stringsAsFactors = FALSE)
accidents = ts(accidents, 1973, freq = 12)

# (a) plot the data
plot(accidents, ylab = "")

# (b) Find seasonality estimates for s_t in 
# the decomposition Y_t=m_t+s_t+X_t using method 1 or 2 from Section 1.4;