# Outline: Generates PDF plots of eigen surfaces and directions created in PCA.R

# Load eigen data
load("~/Masters Report/Eigen Surfaces and Power.RData")
# Set directory for output PDFs
setwd("~/Masters Report/Double Filtered")

# Time step
t <- seq(-100, 896, length.out = 250)

# Set min and max surface values so plots are on same scale 
m1 <- max(XASDE20D_e1, XASDE30D_e1, XASDE40D_e1, XASDU20D_e1, XASDU30D_e1, XASDU40D_e1,
          XTDE20D_e1, XTDE30D_e1, XTDE40D_e1, XTDU20D_e1, XTDU30D_e1, XTDU40D_e1,
          XASDE20T_e1, XASDE30T_e1, XASDE40T_e1, XASDU20T_e1, XASDU30T_e1, XASDU40T_e1,
          XTDE20T_e1, XTDE30T_e1, XTDE40T_e1, XTDU20T_e1, XTDU30T_e1, XTDU40T_e1)
m2 <- min(XASDE20D_e1, XASDE30D_e1, XASDE40D_e1, XASDU20D_e1, XASDU30D_e1, XASDU40D_e1,
          XTDE20D_e1, XTDE30D_e1, XTDE40D_e1, XTDU20D_e1, XTDU30D_e1, XTDU40D_e1,
          XASDE20T_e1, XASDE30T_e1, XASDE40T_e1, XASDU20T_e1, XASDU30T_e1, XASDU40T_e1,
          XTDE20T_e1, XTDE30T_e1, XTDE40T_e1, XTDU20T_e1, XTDU30T_e1, XTDU40T_e1)

#XASDE20D
s <- XASDE20D_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE20DDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE20 Low Direction PC 1')

dev.off()
s <- XASDE20D_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE20DPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE20 Low Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- XASDE20D_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE20DDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE20 Low Direction PC 2')
dev.off()
s <- XASDE20D_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE20DPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE20 Low Power PC 2')
dev.off()
v <- XASDE20D_v/sum(XASDE20D_v)
pdf('ASDE20DPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'ASDE20 Low PC')
dev.off()

#XASDE20T
s <- XASDE20T_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE20TDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE20 High Direction PC 1')
dev.off()
s <- XASDE20T_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE20TPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), (as.matrix(s)), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE20 High Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- (-1)*XASDE20T_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE20TDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE20 High Direction PC 2')
dev.off()
s <- (-1)*XASDE20T_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE20TPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE20 High Power PC 2')
dev.off()
v <- XASDE20T_v/sum(XASDE20T_v)
pdf('ASDE20TPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'ASDE20 High PC')
dev.off()

#XASDE30D
s <- XASDE30D_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE30DDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE30 Low Direction PC 1')
dev.off()
s <- XASDE30D_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE30DPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE30 Low Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- XASDE30D_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE30DDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE30 Low Direction PC 2')
dev.off()
s <- XASDE30D_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE30DPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE30 Low Power PC 2')
dev.off()
v <- XASDE30D_v/sum(XASDE30D_v)
pdf('ASDE30DPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'ASDE30 Low PC')
dev.off()

#XASDE30T
s <- XASDE30T_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE30TDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE30 High Direction PC 1')
dev.off()
s <- XASDE30T_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE30TPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), (as.matrix(s)), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE30 High Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- (-1)*XASDE30T_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE30TDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE30 High Direction PC 2')
dev.off()
s <- (-1)*XASDE30T_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE30TPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE30 High Power PC 2')
dev.off()
v <- XASDE30T_v/sum(XASDE30T_v)
pdf('ASDE30TPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'ASDE30 High PC')
dev.off()

#XASDE40D
s <- XASDE40D_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE40DDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE40 Low Direction PC 1')
dev.off()
s <- XASDE40D_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE40DPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE40 Low Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- XASDE40D_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE40DDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE40 Low Direction PC 2')
dev.off()
s <- XASDE40D_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE40DPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE40 Low Power PC 2')
dev.off()
v <- XASDE40D_v/sum(XASDE40D_v)
pdf('ASDE40DPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'ASDE40 Low PC')
dev.off()

#XASDE40T
s <- XASDE40T_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE40TDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE40 High Direction PC 1')
dev.off()
s <- XASDE40T_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE40TPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), (as.matrix(s)), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE40 High Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- (-1)*XASDE40T_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE40TDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE40 High Direction PC 2')
dev.off()
s <- (-1)*XASDE40T_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDE40TPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDE40 High Power PC 2')
dev.off()
v <- XASDE40T_v/sum(XASDE40T_v)
pdf('ASDE40TPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'ASDE40 High PC')
dev.off()

#XASDU20D
s <- XASDU20D_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU20DDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU20 Low Direction PC 1')
dev.off()
s <- XASDU20D_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU20DPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU20 Low Power PC 1')

abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- XASDU20D_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU20DDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU20 Low Direction PC 2')
dev.off()
s <- XASDU20D_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU20DPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU20 Low Power PC 2')
dev.off()
v <- XASDU20D_v/sum(XASDU20D_v)
pdf('ASDU20DPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'ASDU20 Low PC')
dev.off()

#XASDU20T
s <- XASDU20T_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU20TDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU20 High Direction PC 1')
dev.off()
s <- XASDU20T_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU20TPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), (as.matrix(s)), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU20 High Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- (-1)*XASDU20T_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU20TDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU20 High Direction PC 2')
dev.off()
s <- (-1)*XASDU20T_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU20TPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU20 High Power PC 2')
dev.off()
v <- XASDU20T_v/sum(XASDU20T_v)
pdf('ASDU20TPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'ASDU20 High PC')
dev.off()

#XASDU30D
s <- XASDU30D_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU30DDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU30 Low Direction PC 1')
dev.off()
s <- XASDU30D_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU30DPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU30 Low Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- XASDU30D_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU30DDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU30 Low Direction PC 2')
dev.off()
s <- XASDU30D_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU30DPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU30 Low Power PC 2')
dev.off()
v <- XASDU30D_v/sum(XASDU30D_v)
pdf('ASDU30DPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'ASDU30 Low PC')
dev.off()

#XASDU30T
s <- XASDU30T_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU30TDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU30 High Direction PC 1')
dev.off()
s <- XASDU30T_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU30TPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), (as.matrix(s)), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU30 High Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- (-1)*XASDU30T_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU30TDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU30 High Direction PC 2')
dev.off()
s <- (-1)*XASDU30T_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU30TPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU30 High Power PC 2')
dev.off()
v <- XASDU30T_v/sum(XASDU30T_v)
pdf('ASDU30TPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'ASDU30 High PC')
dev.off()

#XASDU40D
s <- XASDU40D_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU40DDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU40 Low Direction PC 1')
dev.off()
s <- XASDU40D_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU40DPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU40 Low Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- XASDU40D_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU40DDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU40 Low Direction PC 2')
dev.off()
s <- XASDU40D_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU40DPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU40 Low Power PC 2')
dev.off()
v <- XASDU40D_v/sum(XASDU40D_v)
pdf('ASDU40DPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'ASDU40 Low PC')
dev.off()

#XASDU40T
s <- XASDU40T_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU40TDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU40 High Direction PC 1')
dev.off()
s <- XASDU40T_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU40TPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), (as.matrix(s)), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU40 High Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- XASDU40T_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU40TDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU40 High Direction PC 2')
dev.off()
s <- XASDU40T_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('ASDU40TPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'ASDU40 High Power PC 2')
dev.off()
v <- XASDU40T_v/sum(XASDU40T_v)
pdf('ASDU40TPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'ASDU40 High PC')
dev.off()

#XTDE20D
s <- XTDE20D_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE20DDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE20 Low Direction PC 1')
dev.off()
s <- XTDE20D_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE20DPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE20 Low Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- (-1)*XTDE20D_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE20DDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE20 Low Direction PC 2')
dev.off()
s <- (-1)*XTDE20D_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE20DPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE20 Low Power PC 2')
dev.off()
v <- XTDE20D_v/sum(XTDE20D_v)
pdf('TDE20DPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'TDE20 Low PC')
dev.off()

#XTDE20T
s <- XTDE20T_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE20TDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE20 High Direction PC 1')
dev.off()
s <- XTDE20T_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE20TPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), (as.matrix(s)), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE20 High Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- (-1)*XTDE20T_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE20TDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE20 High Direction PC 2')
dev.off()
s <- (-1)*XTDE20T_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE20TPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE20 High Power PC 2')
dev.off()
v <- XTDE20T_v/sum(XTDE20T_v)
pdf('TDE20TPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'TDE20 High PC')
dev.off()

#XTDE30D
s <- XTDE30D_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE30DDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE30 Low Direction PC 1')
dev.off()
s <- XTDE30D_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE30DPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE30 Low Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- (-1)*XTDE30D_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE30DDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE30 Low Direction PC 2')
dev.off()
s <- (-1)*XTDE30D_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE30DPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE30 Low Power PC 2')
dev.off()
v <- XTDE30D_v/sum(XTDE30D_v)
pdf('TDE30DPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'TDE30 Low PC')
dev.off()

#XTDE30T
s <- XTDE30T_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE30TDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), (as.matrix(s)), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE30 High Direction PC 1')
dev.off()
s <- XTDE30T_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE30TPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE30 High Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- (-1)*XTDE30T_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE30TDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE30 High Direction PC 2')
dev.off()
s <- (-1)*XTDE30T_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE30TPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE30 High Power PC 2')
dev.off()
v <- XTDE30T_v/sum(XTDE30T_v)
pdf('TDE30TPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'TDE30 High PC')
dev.off()

#XTDE40D
s <- XTDE40D_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE40DDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE40 Low Direction PC 1')
dev.off()
s <- XTDE40D_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE40DPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE40 Low Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- XTDE40D_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE40DDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE40 Low Direction PC 2')
dev.off()
s <- XTDE40D_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE40DPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE40 Low Power PC 2')
dev.off()
v <- XTDE40D_v/sum(XTDE40D_v)
pdf('TDE40DPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'TDE40 Low PC')
dev.off()

#XTDE40T
s <- XTDE40T_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE40TDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE40 High Direction PC 1')
dev.off()
s <- XTDE40T_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE40TPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), (as.matrix(s)), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE40 High Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- (-1)*XTDE40T_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE40TDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE40 High Direction PC 2')
dev.off()
s <- (-1)*XTDE40T_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDE40TPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDE40 High Power PC 2')
dev.off()
v <- XTDE40T_v/sum(XTDE40T_v)
pdf('TDE40TPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'TDE40 High PC')
dev.off()

#XTDU20D
s <- XTDU20D_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU20DDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU20 Low Direction PC 1')
dev.off()
s <- XTDU20D_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU20DPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU20 Low Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- (-1)*XTDU20D_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU20DDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU20 Low Direction PC 2')
dev.off()
s <- (-1)*XTDU20D_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU20DPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU20 Low Power PC 2')
dev.off()
v <- XTDU20D_v/sum(XTDU20D_v)
pdf('TDU20DPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'TDU20 Low PC')
dev.off()

#XTDU20T
s <- XTDU20T_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU20TDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU20 High Direction PC 1')

dev.off()
s <- XTDU20T_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU20TPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), (as.matrix(s)), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU20 High Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- (-1)*XTDU20T_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU20TDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU20 High Direction PC 2')
dev.off()
s <- (-1)*XTDU20T_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU20TPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU20 High Power PC 2')
dev.off()
v <- XTDU20T_v/sum(XTDU20T_v)
pdf('TDU20TPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'TDU20 High PC')
dev.off()

#XTDU30D
s <- XTDU30D_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU30DDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU30 Low Direction PC 1')
dev.off()
s <- XTDU30D_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU30DPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU30 Low Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- XTDU30D_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU30DDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU30 Low Direction PC 2')
dev.off()
s <- XTDU30D_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU30DPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU30 Low Power PC 2')
dev.off()
v <- XTDU30D_v/sum(XTDU30D_v)
pdf('TDU30DPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'TDU30 Low PC')
dev.off()

#XTDU30T
s <- XTDU30T_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU30TDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU30 High Direction PC 1')
dev.off()
s <- XTDU30T_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU30TPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), (as.matrix(s)), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU30 High Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- (-1)*XTDU30T_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU30TDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU30 High Direction PC 2')
dev.off()
s <- (-1)*XTDU30T_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU30TPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU30 High Power PC 2')
dev.off()
v <- XTDU30T_v/sum(XTDU30T_v)
pdf('TDU30TPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'TDU30 High PC')
dev.off()

#XTDU40D
s <- XTDU40D_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU40DDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU40 Low Direction PC 1')
dev.off()
s <- XTDU40D_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU40DPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU40 Low Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- XTDU40D_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU40DDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU40 Low Direction PC 2')
dev.off()
s <- XTDU40D_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU40DPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU40 Low Power PC 2')
dev.off()
v <- XTDU40D_v/sum(XTDU40D_v)
pdf('TDU40DPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'TDU40 Low PC')
dev.off()

#XTDU40T
s <- XTDU40T_d1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU40TDirectionPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU40 High Direction PC 1')
dev.off()
s <- XTDU40T_e1


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU40TPowerPC1.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), (as.matrix(s)), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU40 High Power PC 1')
abline(v=70,lty=2) 
abline(v=210,lty=2)
dev.off()
s <- (-1)*XTDU40T_d2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU40TDirectionPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU40 High Direction PC 2')
dev.off()
s <- (-1)*XTDU40T_e2


colfunc <- colorRampPalette(c("dark violet", "red"))
if (m2 < 0){
  colfunc <- colorRampPalette(c("steel blue", "dark violet", "red"))
}
pdf('TDU40TPowerPC2.pdf')
filled.contour(t[ts], 1/rev(my.wt$Period), as.matrix(s), levels = seq(m2, m1, length.out = 11), 
               nlevels = 11, col = colfunc(10), xlab = 'Time', ylab = 'Frequency', main = 'TDU40 High Power PC 2')
dev.off()
v <- XTDU40T_v/sum(XTDU40T_v)
pdf('TDU40TPC.pdf')
plot(v[1:20], type = 'o', ylab = 'Percent Variation Explained', xlab = 'PC #', main = 'TDU40 High PC')
dev.off()


