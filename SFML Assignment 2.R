library(ISwR)
data(thuesen)

I <-! is.na(thuesen[,"short.velocity"])
Y<-thuesen[I,"short.velocity"]
X<-thuesen[I,"blood.glucose"]

# compute the coefficients $\beta_1$
beta_1 = cov(X,Y) / var(X)
beta_1

beta_0 = Y - beta_1*X
beta_0

t.test(X,Y)

rm(list=ls())
library(MASS)

# initial values for n, (sigma_w) and beta
n<-3 # number of input variables
p<-n+1
beta<-seq(2,p+1) # beta =(2,3,...,n+2)
sd.w<-5  

# generating data D_N
N<-100 # number of samples
X<-array(runif(N*n,min=-20,max=20),c(N,n))
X<-cbind(array(1,c(N,1)),X)


R<-100#00 # number of iterations
beta.hat<-array(0,c(p,R))
var.hat.w<-numeric(R)
Y.hat<-array(NA,c(R,N))
