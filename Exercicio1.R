###############################################################
# 1. GRÁFICO SIMPLES
rm(list=ls())

x <- seq(-10, 10, 1)
y <- x^2
plot(x, y, type='b', main="Parábola")

###############################################################
# 2. FUNÇÃO SENO
x <- seq(0, 2*pi, 0.1)
y <- sin(x)

plot(x, y, type='l', col='blue', main="Seno")
points(x, y, col='red')

###############################################################
# 3. GRÁFICO 3D
rm(list=ls())
library(plot3D)

seqi <- seq(-25, 25, 2)
seqj <- seq(-25, 25, 2)

M <- matrix(0, nrow=length(seqi), ncol=length(seqj))

for (i in 1:length(seqi)) {
  for (j in 1:length(seqj)) {
    M[i, j] <- -seqi[i]^3 + seqj[j]^3 + 6*seqi[i]*seqj[j]
  }
}

persp3D(seqi, seqj, M, contour=TRUE)

###############################################################
# 4. SÉRIE TEMPORAL
data(AirPassengers)
summary(AirPassengers)
plot(AirPassengers, main="AirPassengers")

###############################################################
# 5. IRIS
rm(list=ls())
data(iris)

summary(iris)
plot(iris)

image(as.matrix(dist(iris)))

###############################################################
# 6. DADOS SINTÉTICOS
rm(list=ls())

xc1 <- rnorm(30) * 0.5 + 2
xc2 <- rnorm(30) * 0.5 + 4

plot(xc1, rep(0, length(xc1)), col='red', xlim=c(0,6), ylim=c(0,1))
points(xc2, rep(0, length(xc2)), col='blue')

xl <- c(xc1, xc2)
image(1:60, 1:60, as.matrix(dist(xl)))

###############################################################
# 7. CLASSIFICADOR 1D
rm(list=ls())

pdfvar <- function(x, u, s) {
  (1/(sqrt(2*pi)*s)) * exp(-((x-u)^2/(2*s^2)))
}

classificax <- function(x, m1, s1, m2, s2) {
  ifelse(pdfvar(x,m1,s1) > pdfvar(x,m2,s2), 1, 0)
}

xc1 <- rnorm(30) * 0.5 + 2
xc2 <- rnorm(30) * 0.5 + 4

plot(xc1, rep(0, length(xc1)), col='red', xlim=c(0,6), ylim=c(0,1))
points(xc2, rep(0, length(xc2)), col='blue')

m1 <- mean(xc1); d1 <- sd(xc1)
m2 <- mean(xc2); d2 <- sd(xc2)

xt <- seq(0, 6, 0.1)

yt1 <- pdfvar(xt, m1, d1)
yt2 <- pdfvar(xt, m2, d2)

lines(xt, yt1, col='red')
lines(xt, yt2, col='blue')

classe <- classificax(xt, m1, d1, m2, d2)

lines(xt, classe, col='black', lwd=2)

###############################################################
# 8. CLASSIFICADOR 2D
rm(list=ls())

library(plot3D)

pdf2var <- function(x1, x2, m1, s1, m2, s2, ro) {
  (1/(2*pi*s1*s2*sqrt(1-ro^2))) *
    exp(-(1/(2*(1-ro^2))) *
          (((x1-m1)/s1)^2 -
             (2*ro*(x1-m1)*(x2-m2))/(s1*s2) +
             ((x2-m2)/s2)^2))
}

nc <- 100

xc1 <- matrix(rnorm(nc*2), ncol=2) * 0.5 + 2
xc2 <- matrix(rnorm(nc*2), ncol=2) * 0.5 + 4

plot(xc1[,1], xc1[,2], col='red', xlim=c(0,6), ylim=c(0,6))
points(xc2[,1], xc2[,2], col='blue')

m11 <- mean(xc1[,1]); d11 <- sd(xc1[,1])
m12 <- mean(xc1[,2]); d12 <- sd(xc1[,2])

m21 <- mean(xc2[,1]); d21 <- sd(xc2[,1])
m22 <- mean(xc2[,2]); d22 <- sd(xc2[,2])

seqi <- seq(0, 10, 0.5)
seqj <- seq(0, 10, 0.5)

M <- matrix(0, nrow=length(seqi), ncol=length(seqj))

for (i in 1:length(seqi)) {
  for (j in 1:length(seqj)) {
    p1 <- pdf2var(seqi[i], seqj[j], m11, d11, m12, d12, 0)
    p2 <- pdf2var(seqi[i], seqj[j], m21, d21, m22, d22, 0)
    M[i,j] <- ifelse(p1 > p2, 1, 0)
  }
}

persp3D(seqi, seqj, M, main="Classificação 2D")

