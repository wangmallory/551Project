#' 3. Initial Modeling
#' As part of Stats 551 Final Project
#' 
#' For predicting CO2, we may want to try Linear Regression and Time Series with Bayesian to start

# Library
library(tidyverse)
library(ggplot2)
library(reshape)
library(rstan)
library(rstanarm)
library(glmnet)
set.seed(4444)

# Data 
us_df = read.csv("us_df.csv")
us_df = us_df[which(as.numeric(us_df$year) >= 1970 & as.numeric(us_df$year) <= 2014),]

# First, we start with a fitting a linear model inc, urban, gdpg, and eng_imp are not significant
# and when taken out, the R^2 increases
us.lm = lm(co2 ~ .- inc - urban - gdpg - eng_imp, data = us_df)
us.lm$coefficients

us_df = us_df[,-which(names(us_df) %in% c("pop","gdp","gdi","inc"))]

us_lm_stan = summary(stan_glm(co2 ~ ., data = us_df))

#' Initiate variables -- prior is midpoint of range
S = 5000
X = as.matrix(us_df[,-2])
y = us_df[,"co2"]
n = dim(X)[1]
p = dim(X)[2]
# Prior
g = n
beta0 = c(midrange(us_df$co2), 0)
sigma0 = rbind(c(0.25, 0), c(0, 0.1))
nu0 = 2
s20 = 1
set.seed(4444)
inv = solve
# Run linear regression 
Hg <- (g/(g+1)) * X%*%solve(t(X)%*%X)%*%t(X)
SSRg <- t(y)%*%(diag(1,nrow=n)-Hg)%*%y

s2 <- 1/rgamma(S,(nu0+n)/2, (nu0*s20+SSRg)/2)

Vb <- g*solve(t(X)%*%X)/(g+1)
Eb <- Vb%*%t(X)%*%y

E <- matrix(rnorm(S*p, 0, sqrt(s2)),S,p)
beta <- t(t(E%*%chol(Vb))+c(Eb))

lpy.X <- function(y,X, g=length(y), nu0=1, s20=try(summary(lm(y~1+X))$sigma^2,silent = TRUE)){
  n<-dim(X)[1]
  p<-dim(X)[2]
  if(p==0){Hg <- 0; s20 <- mean(y^2)}
  if(p>0){Hg <- (g/(g+1))*X%*%solve(t(X)%*%X)%*%t(X)}
  SSRg <- t(y)%*%(diag(1,nrow=n)-Hg)%*%y
  -0.5*(n*log(pi)+p*log(1+g)+(nu0+n)*log(nu0*s20+SSRg)-nu0*log(nu0*s20))+lgamma((nu0+n)/2)-lgamma(nu0/2)
}
z <- rep(1,dim(X)[2])
lpy.c<-lpy.X(y,X[,z==1,drop=FALSE])
S<-10000
Z<-matrix(NA,S,dim(X)[2])
for(s in 1:S){
  for(j in sample(1:dim(X)[2])){
    zp <- z
    zp[j]<-1-zp[j]
    lpy.p<-lpy.X(y,X[,zp==1,drop=FALSE])
    r<-(lpy.p-lpy.c)*(-1)^(zp[j]==0)
    z[j]<-rbinom(1,1,1/(1+exp(-r)))
    if(z[j]==zp[j]){lpy.c<-lpy.p}
  }
  Z[s,]<-z
}

colSums(Z)/S

t(apply(colSums(Z)/S, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.5, 0.975)))

