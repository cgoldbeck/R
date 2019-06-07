# Load required libraries
library(WaveletComp)
library(readr)
library(ggplot2)
library(reshape2)
library(signal)

# Outline: Apply a wavelet transformation to raw ERP funcitonal data in order to form time frequency surfaces
# that we can then apply smoothing techniques. Here, for each ERP in each condition, window, group, and frequency band, 
# apply wavelet transformation and collect 2D surface as a single vector

# Import raw 'Shape' data to be decomposed (very large)
load("~/Masters Report/Data.RData")

# Different groups to consider: Learning condition, trial no. window start, trial no. window end, 
# autism group, Delta frq., Theta frq.
# This matrix contains all the different combinations we wish to examine 
M <- cbind(c('Expected', 5, 35, 'TD', 'XTDE20D', 'XTDE20T'),
           c('Expected', 15, 45, 'TD', 'XTDE30D', 'XTDE30T'),
           c('Expected', 25, 55, 'TD', 'XTDE40D', 'XTDE40T'),
           c('Unexpected', 5, 35, 'TD', 'XTDU20D', 'XTDU20T'),
           c('Unexpected', 15, 45, 'TD', 'XTDU30D', 'XTDU30T'),
           c('Unexpected', 25, 55, 'TD', 'XTDU40D', 'XTDU40T'),
           c('Expected', 5, 35, 'ASD', 'XASDE20D', 'XASDE20T'),
           c('Expected', 15, 45, 'ASD', 'XASDE30D', 'XASDE30T'),
           c('Expected', 25, 55, 'ASD', 'XASDE40D', 'XASDE40T'),
           c('Unexpected', 5, 35, 'ASD', 'XASDU20D', 'XASDU20T'),
           c('Unexpected', 15, 45, 'ASD', 'XASDU30D', 'XASDU30T'),
           c('Unexpected', 25, 55, 'ASD', 'XASDU40D', 'XASDU40T'))

# We want to filter the ERP via a Butterworth filter
# First we remove the low frequency below 1.25 Hz
# Then split the new ERP at 4 Hz (Delta and Theta bands)
bfH <- butter(3, .01, type = 'high') # removed low frq. noise
bfH4 <- butter(3, .032, type = 'high') # split data at 4 hrz above
bfL4 <- butter(3, .032, type = 'low') # split data at 4 hrz below

#Apply filters
df$valueH <- filter(bfH, df$value)
df$valueL4 <- filter(bfL4, df$valueH)
df$valueH4 <- filter(bfH4, df$valueH)

t <- seq(-100, 896, length.out = 250) # time sampling rate
ts <- seq(1, 250, by = 3) # time order

# Loop through desired conditions (M matrix)
for (j in 1:12){
  X <- df[which(df$condition==M[1, j] & df$trial %in% c(M[2, j]:M[3, j]) & df$Group==M[4, j]), ] # ERPs of interest
  Delta <- as.data.frame(matrix(0, nrow = 84*31)) # Initialize Delta Matrix to hold results
  Theta <- as.data.frame(matrix(0, nrow = 84*31)) # Initialize Theta Matrix to hold results
  
  # Apply Wavelet transformation on Delta frequency 
  for (i in 1:(nrow(X)/250)){
    my.data <- data.frame(x = X$valueL4[(1 + 250*(i - 1)):(250 + 250*(i - 1))])
    my.wt <- analyze.wavelet(my.data, "x",
                             loess.span = 0,
                             dt = 1/250, dj = 1/6,
                             lowerPeriod = 1/16,
                             upperPeriod = 2,
                             make.pval = F)
    
    # We invert the row order because the output is period and we want frequency
    # We also stanck the rows to get a single vector instead of a matrix
    Delta <- cbind(Delta, stack(as.data.frame(t((Mod(my.wt$Wave)^2)[31:1, ts])))[, 1])
  }
  
  # Apply Wavelet transformation on Thata frequency 
  for (i in 1:(nrow(X)/250)){
    my.data <- data.frame(x = X$valueH4[(1 + 250*(i - 1)):(250 + 250*(i - 1))])
    my.wt <- analyze.wavelet(my.data, "x",
                             loess.span = 0,
                             dt = 1/250, dj = 1/6,
                             lowerPeriod = 1/16,
                             upperPeriod = 2,
                             make.pval = F)
    
    # We invert the row order because the output is period and we want frequency
    # We also stanck the rows to get a single vector instead of a matrix
    Theta <- cbind(Theta, stack(as.data.frame(t((Mod(my.wt$Wave)^2)[31:1, ts])))[, 1])
  }
  # Accumulate results in pre assinged matrix M
  assign(M[5, j], Delta)
  assign(M[6, j], Theta)
}

# Next step, apply smoothing techniques to reduce noise (see master's report)