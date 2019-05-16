# Outline: Performs eigen decomposition of wavelet transformed ERPs in created in Time Frequency Wavelet Transformation.R
# Outputs eigen surface (eigen vector with power), eigen direction (just eigen vector), and eigen value to be used for plotting
# Runs over XASDE20D, XASDE20T - ... - XTDU40D, XTDU40T
# Creates Eigen Surfaces and Power.RData

start.time <- Sys.time()
# For XASDE20D
Y <- XASDE20D
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XASDE20D_e1 <- s1*sqrt(YYE$values[1])
XASDE20D_d1 <- s1

XASDE20D_v <- YYE$values

# For XASDE20T
Y <- XASDE20T
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XASDE20T_e1 <- s1*sqrt(YYE$values[1])
XASDE20T_d1 <- s1

XASDE20T_v <- YYE$values

# For XASDE30D
Y <- XASDE30D
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XASDE30D_e1 <- s1*sqrt(YYE$values[1])
XASDE30D_d1 <- s1

XASDE30D_v <- YYE$values

# For XASDE30T
Y <- XASDE30T
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XASDE30T_e1 <- s1*sqrt(YYE$values[1])
XASDE30T_d1 <- s1

XASDE30T_v <- YYE$values

# For XASDE40D
Y <- XASDE40D
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XASDE40D_e1 <- s1*sqrt(YYE$values[1])
XASDE40D_d1 <- s1

XASDE40D_v <- YYE$values

# For XASDE40T
Y <- XASDE40T
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XASDE40T_e1 <- s1*sqrt(YYE$values[1])
XASDE40T_d1 <- s1

XASDE40T_v <- YYE$values

# For XASDU20D
Y <- XASDU20D
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XASDU20D_e1 <- s1*sqrt(YYE$values[1])
XASDU20D_d1 <- s1

XASDU20D_v <- YYE$values

# For XASDU20T
Y <- XASDU20T
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XASDU20T_e1 <- s1*sqrt(YYE$values[1])
XASDU20T_d1 <- s1

XASDU20T_v <- YYE$values

# For XASDU30D
Y <- XASDU30D
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XASDU30D_e1 <- s1*sqrt(YYE$values[1])
XASDU30D_d1 <- s1

XASDU30D_v <- YYE$values

# For XASDU30T
Y <- XASDU30T
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XASDU30T_e1 <- s1*sqrt(YYE$values[1])
XASDU30T_d1 <- s1

XASDU30T_v <- YYE$values

# For XASDU40D
Y <- XASDU40D
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XASDU40D_e1 <- s1*sqrt(YYE$values[1])
XASDU40D_d1 <- s1

XASDU40D_v <- YYE$values

# For XASDU40T
Y <- XASDU40T
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XASDU40T_e1 <- s1*sqrt(YYE$values[1])
XASDU40T_d1 <- s1

XASDU40T_v <- YYE$values

# For XTDE20D
Y <- XTDE20D
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XTDE20D_e1 <- s1*sqrt(YYE$values[1])
XTDE20D_d1 <- s1

XTDE20D_v <- YYE$values

# For XTDE20T
Y <- XTDE20T
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XTDE20T_e1 <- s1*sqrt(YYE$values[1])
XTDE20T_d1 <- s1

XTDE20T_v <- YYE$values

# For XTDE30D
Y <- XTDE30D
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XTDE30D_e1 <- s1*sqrt(YYE$values[1])
XTDE30D_d1 <- s1

XTDE30D_v <- YYE$values

# For XTDE30T
Y <- XTDE30T
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XTDE30T_e1 <- s1*sqrt(YYE$values[1])
XTDE30T_d1 <- s1

XTDE30T_v <- YYE$values

# For XTDE40D
Y <- XTDE40D
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XTDE40D_e1 <- s1*sqrt(YYE$values[1])
XTDE40D_d1 <- s1

XTDE40D_v <- YYE$values

# For XTDE40T
Y <- XTDE40T
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XTDE40T_e1 <- s1*sqrt(YYE$values[1])
XTDE40T_d1 <- s1

XTDE40T_v <- YYE$values

# For XTDU20D
Y <- XTDU20D
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XTDU20D_e1 <- s1*sqrt(YYE$values[1])
XTDU20D_d1 <- s1

XTDU20D_v <- YYE$values

# For XTDU20T
Y <- XTDU20T
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XTDU20T_e1 <- s1*sqrt(YYE$values[1])
XTDU20T_d1 <- s1

XTDU20T_v <- YYE$values

# For XTDU30D
Y <- XTDU30D
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]


s1 <- as.data.frame(matrix(0, nrow = 84))


for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
  
}
s1 <- s1[, -1]


XTDU30D_e1 <- s1*sqrt(YYE$values[1])
XTDU30D_d1 <- s1

XTDU30D_v <- YYE$values

# For XTDU30T
Y <- XTDU30T
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]
s1 <- as.data.frame(matrix(0, nrow = 84))
for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
  
}
s1 <- s1[, -1]
XTDU30T_e1 <- s1*sqrt(YYE$values[1])
XTDU30T_d1 <- s1
XTDU30T_v <- YYE$values

# For XTDU40D
Y <- XTDU40D
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]
s1 <- as.data.frame(matrix(0, nrow = 84))
for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
}
s1 <- s1[, -1]
XTDU40D_e1 <- s1*sqrt(YYE$values[1])
XTDU40D_d1 <- s1
XTDU40D_v <- YYE$values

# For XTDU40T
Y <- XTDU40T
Y<-Y[, -1]
YY <- as.matrix(Y) %*% t(as.matrix(Y))
YYE <- eigen(YY)
e1 <- (-1)*YYE$vectors[, 1]
s1 <- as.data.frame(matrix(0, nrow = 84))
for (i in 1:31){
  s1 <- cbind(s1, e1[(1 + 84*(i-1)):(84 + 84*(i-1))])
}
s1 <- s1[, -1]
XTDU40T_e1 <- s1*sqrt(YYE$values[1])
XTDU40T_d1 <- s1
XTDU40T_v <- YYE$values

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken