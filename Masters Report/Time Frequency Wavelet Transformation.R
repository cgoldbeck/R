library(WaveletComp)
library(readr)
library(ggplot2)
library(reshape2)
library(signal)
# Outline: For each ERP in each condition, window, group, and frequency band, apply wavelet transformation

# Import raw 'Shape' data to be decomposed (very large)
load("~/Masters Report/Data.RData")

# Different groups to consider: Condition, trial window start, trial window end, group, Delta frq., Theta frq.
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

# Filter data 
bfH <- butter(3, .01, type = 'high') # removed low frq. noise
bfH4 <- butter(3, .032, type = 'high') # split data at 4 hrz above
bfL4 <- butter(3, .032, type = 'low') # split data at 4 hrz below
df$valueH <- filter(bfH, df$value)
df$valueL4 <- filter(bfL4, df$valueH)
df$valueH4 <- filter(bfH4, df$valueH)
t <- seq(-100, 896, length.out = 250) # time sampling
ts <- seq(1, 250, by = 3)
start.time <- Sys.time()
for (j in 1:12){
  X <- df[which(df$condition==M[1, j] & df$trial %in% c(M[2, j]:M[3, j]) & df$Group==M[4, j]), ]
  Delta <- as.data.frame(matrix(0, nrow = 84*31))
  Theta <- as.data.frame(matrix(0, nrow = 84*31))
  for (i in 1:(nrow(X)/250)){
    my.data <- data.frame(x = X$valueL4[(1 + 250*(i - 1)):(250 + 250*(i - 1))])
    my.wt <- analyze.wavelet(my.data, "x",
                             loess.span = 0,
                             dt = 1/250, dj = 1/6,
                             lowerPeriod = 1/16,
                             upperPeriod = 2,
                             make.pval = F)
    Delta <- cbind(Delta, stack(as.data.frame(t((Mod(my.wt$Wave)^2)[31:1, ts])))[, 1])
  }
  for (i in 1:(nrow(X)/250)){
    my.data <- data.frame(x = X$valueH4[(1 + 250*(i - 1)):(250 + 250*(i - 1))])
    my.wt <- analyze.wavelet(my.data, "x",
                             loess.span = 0,
                             dt = 1/250, dj = 1/6,
                             lowerPeriod = 1/16,
                             upperPeriod = 2,
                             make.pval = F)
    Theta <- cbind(Theta, stack(as.data.frame(t((Mod(my.wt$Wave)^2)[31:1, ts])))[, 1])
  }
  assign(M[5, j], Delta)
  assign(M[6, j], Theta)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken