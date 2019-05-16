# Outline: Generates PDF plots of eigen surfaces differences created in PCA.R

# Set directory for output PDFs
setwd("~/Desktop/Masters Report/Double Filtered")

# Time step
t <- seq(-100, 896, length.out = 250)

# Set min and max surface values so plots are on same scale 
m1 <- max(XASDE20D_e1 - XASDU20D_e1, XASDE30D_e1 - XASDU30D_e1, XASDE40D_e1 - XASDU40D_e1,
          XTDE20D_e1 - XTDU20D_e1, XTDE30D_e1 - XTDU30D_e1, XTDE40D_e1 - XTDU40D_e1,
          XASDE20T_e1 - XASDU20T_e1, XASDE30T_e1 - XASDU30T_e1, XASDE40T_e1 - XASDU40T_e1,
          XTDE20T_e1 - XTDU20T_e1, XTDE30T_e1 - XTDU30T_e1, XTDE40T_e1 - XTDU40T_e1)
m2 <- min(XASDE20D_e1 - XASDU20D_e1, XASDE30D_e1 - XASDU30D_e1, XASDE40D_e1 - XASDU40D_e1,
          XTDE20D_e1 - XTDU20D_e1, XTDE30D_e1 - XTDU30D_e1, XTDE40D_e1 - XTDU40D_e1,
          XASDE20T_e1 - XASDU20T_e1, XASDE30T_e1 - XASDU30T_e1, XASDE40T_e1 - XASDU40T_e1,
          XTDE20T_e1 - XTDU20T_e1, XTDE30T_e1 - XTDU30T_e1, XTDE40T_e1 - XTDU40T_e1)


s <- XASDE20D_e1 - XASDU20D_e1
colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
pdf('ASD20DPowerDiff.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE20 Delta PowerDiff PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()

s <- XASDE30D_e1 - XASDU30D_e1
colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
pdf('ASD30DPowerDiff.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE30 Delta PowerDiff PC 1')
abline(v=70,lty=2)
abline(v=210,lty=2)
dev.off()

s <- XASDE40D_e1 - XASDU40D_e1
colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
pdf('ASD40DPowerDiff.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE40 Delta PowerDiff PC 1')
abline(v=70,lty=2)
abline(v=210,lty=2)
dev.off()

s <- XASDE20T_e1 - XASDU20T_e1
colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
pdf('ASD20TPowerDiff.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period)[1:15], as.matrix(s)[, 1:15], levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE20 Theta PowerDiff PC 1')
abline(v=70,lty=2)
abline(v=210,lty=2)
dev.off()

s <- XASDE30T_e1 - XASDU30T_e1
colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
pdf('ASD30TPowerDiff.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period)[1:15], as.matrix(s)[, 1:15], levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE30 Theta PowerDiff PC 1')
abline(v=70,lty=2)
abline(v=210,lty=2)
dev.off()

s <- XASDE40T_e1 - XASDU40T_e1
colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
pdf('ASD40TPowerDiff.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period)[1:15], as.matrix(s)[, 1:15], levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE40 Theta PowerDiff PC 1')
abline(v=70,lty=2)
abline(v=210,lty=2)
dev.off()

s <- XTDE20D_e1 - XTDU20D_e1
colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
pdf('TD20DPowerDiff.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE20 Delta PowerDiff PC 1')
abline(v=70,lty=2)
abline(v=210,lty=2)
dev.off()

s <- XTDE30D_e1 - XTDU30D_e1
colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
pdf('TD30DPowerDiff.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE30 Delta PowerDiff PC 1')
abline(v=70,lty=2)
abline(v=210,lty=2)
dev.off()

s <- XTDE40D_e1 - XTDU40D_e1
colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
pdf('TD40DPowerDiff.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE40 Delta PowerDiff PC 1')
abline(v=70,lty=2)
abline(v=210,lty=2)
dev.off()

s <- XTDE20T_e1 - XTDU20T_e1
colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
pdf('TD20TPowerDiff.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period)[1:15], as.matrix(s)[, 1:15], levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE20 Theta PowerDiff PC 1')
abline(v=70,lty=2)
abline(v=210,lty=2)
dev.off()

s <- XTDE30T_e1 - XTDU30T_e1
colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
pdf('TD30TPowerDiff.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period)[1:15], as.matrix(s)[, 1:15], levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE30 Theta PowerDiff PC 1')
abline(v=70,lty=2)
abline(v=210,lty=2)
dev.off()

s <- XTDE40T_e1 - XTDU40T_e1
colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
pdf('TD40TPowerDiff.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period)[1:15], as.matrix(s)[, 1:15], levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE40 Theta PowerDiff PC 1')
abline(v=70,lty=2)
abline(v=210,lty=2)
dev.off()