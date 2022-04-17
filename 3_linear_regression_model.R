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
library(resample)
library(coda)
set.seed(4444)

# Data 
us_df = read.csv("us_df.csv")
us_df = us_df[which(as.numeric(us_df$year) >= 1970 & as.numeric(us_df$year) <= 2014),]

# First, we start with a fitting a linear model inc, urban, gdpg, and eng_imp are not significant
# and when taken out, the R^2 increases
us.lm = lm(co2 ~ .- inc - urban - gdpg - eng_imp, data = us_df)
us.lm$coefficients

us_lm_stan = summary(stan_glm(co2 ~ ., data = us_df))

us_df = us_df[,-which(names(us_df) %in% c("pop","gdp","gdi","inc"))]

# Linear Regression - only at CO2 and time
S = 5000
X = cbind(rep(1, dim(us_df)[1]), seq(1, dim(us_df)[1], by = 1))
n = dim(X)[1]
p = dim(X)[2]
# Prior
beta0 = c(midrange(us_df$co2), 0)
sigma0 = rbind(c(0.25, 0), c(0, 0.1))
nu0 = 1
s20 = 0.25
set.seed(4444)
inv = solve
# Run linear regression gibbs sampling and obtain a posterior predictive distribution
usco2_pred = apply(t(us_df[,2]), MARGIN = 1, function(y) {
  
  # Store samples
  BETA = matrix(nrow = S, ncol = length(beta0))
  SIGMA = numeric(S)
  
  # Starting values - just use prior values?
  beta = c(midrange(us_df$co2), 0)
  s2 = 0.7^2
  
  # Gibbs sampling algorithm from 9.2.1
  for (s in 1:S) {
    # 1a) Compute V and m
    V = inv(inv(sigma0) + (t(X) %*% X) / s2)
    m = V %*% (inv(sigma0) %*% beta0 + (t(X) %*% y) / s2)
    
    # 1b) sample beta
    beta = mvrnorm(1, m, V)
    
    # 2a) Compute SSR(beta) (specific formula from 9.1)
    ssr = (t(y) %*% y) - (2 * t(beta) %*% t(X) %*% y) + (t(beta) %*% t(X) %*% X %*% beta)
    
    # 2b) sample s2
    s2 = 1 / rgamma(1, (nu0 + n) / 2, (nu0 * s20 + ssr) / 2)
    BETA[s, ] = beta
    SIGMA[s] = s2
  }
  
  # Now sample posterior predictive - two weeks later
  xpred = c(1, 56)
  YPRED = rnorm(S, BETA %*% xpred, sqrt(SIGMA))
  
  YPRED
})

mean(usco2_pred) # 2024 CO2 emissions prediction using only time 

# Check how good the model is
N = 1000
pred_errors = t(sapply(1:N, function(i) {
  y = us_df$co2
  X = as.matrix(us_df[,-2])
  ytrain = y[1:35]
  Xtrain = X[1:35, ]
  ytest = y[-c(1:35)]
  Xtest = X[-c(1:35), ]
  
  # OLS
  beta_ols = inv(t(Xtrain) %*% Xtrain) %*% t(Xtrain) %*% ytrain
  beta_ols
  
  y_ols = Xtest %*% beta_ols
  
  pred_error_ols = sum((ytest - y_ols)^2) / length(ytest)
  
  # Bayes
  y = ytrain
  X = Xtrain
  
  n = dim(X)[1]
  p = dim(X)[2]
  
  g = n
  nu0 = 2
  s20 = 1
  
  S = 1000
  
  Hg = (g / (g + 1)) * X %*% inv(t(X) %*% X) %*% t(X)
  SSRg = t(y) %*% (diag(1, nrow = n) - Hg) %*% y
  
  s2 = 1 / rgamma(S, (nu0 + n) / 2, (nu0 * s20 + SSRg) / 2)
  Vb = g * inv(t(X) %*% X) / (g + 1)
  Eb = Vb %*% t(X) %*% y
  
  E = matrix(rnorm(S * p, 0, sqrt(s2)), S, p)
  beta = t(t(E %*% chol(Vb)) + c(Eb))
  
  beta_bayes = as.matrix(colMeans(beta))
  
  y_bayes = Xtest %*% beta_bayes
  
  pred_error_bayes = sum((ytest - y_bayes)^2) / length(ytest)
  c(pred_error_ols, pred_error_bayes)
})) %>% as.data.frame
colnames(pred_errors) = c('ols', 'bayes')
pred_diff = pred_errors %>% transmute(`bayes - ols` = bayes - ols)
ggplot(pred_diff, aes(x = `bayes - ols`)) +
  geom_density() +
  geom_vline(xintercept = 0, lty = 2)
mean(pred_errors$bayes < pred_errors$ols)


# Linear Regression: Model Selection
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
# Run linear regression with g-prior
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

X = X*Z[1:dim(X)[1],]
Hg <- (g/(g+1)) * X%*%solve(t(X)%*%X)%*%t(X)
SSRg <- t(y)%*%(diag(1,nrow=n)-Hg)%*%y

s2 <- 1/rgamma(S,(nu0+n)/2, (nu0*s20+SSRg)/2)

Vb <- g*solve(t(X)%*%X)/(g+1)
Eb <- Vb%*%t(X)%*%y

E <- matrix(rnorm(S*p, 0, sqrt(s2)),S,p)
beta <- t(t(E%*%chol(Vb))+c(Eb))

t(apply(beta, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.5, 0.975)))
signif = apply(beta, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.5, 0.975)) %>%
  apply(MARGIN = 2, FUN = function(y) !(y[1] < 0 && 0 < y[3]))
beta_df = as.data.frame(beta) %>%
  gather(key = 'variable', val = 'coefficient') %>%
  mutate(signif = signif[variable])
ggplot(beta_df, aes(x = variable, y = coefficient, color = signif)) +
  stat_summary(fun = mean, fun.min = function(y) quantile(y, probs = c(0.025)), fun.max = function(y) quantile(y, probs = c(0.975))) +
  geom_hline(yintercept = 0, lty = 2) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

beta_bayes = as.matrix(colMeans(beta))
y_bayes = Xtest %*% beta_bayes
bayes_df = data.frame(
  observed = ytest,
  predicted = y_bayes
)
ggplot(bayes_df, aes(x = observed, y = predicted)) +
  geom_point() +
  geom_smooth(method = 'lm')

# beta_df$id = rep(1:10000,6)
# beta_wide = reshape(beta_df, idvar = "id", timevar = "variable", direction = "wide")


