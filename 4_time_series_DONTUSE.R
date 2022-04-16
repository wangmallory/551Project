#' 4. Try Time Series Bayes
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
library(TTR)
set.seed(4444)

# Function
rabsts <- function(data, season.num, season.dur, pred.horizon) {
  ##get trend or/and seasonal state
  if(season.num==0){
    ss <- bsts::AddLocalLinearTrend(list(), data)}
  else{
    ss <- bsts::AddLocalLinearTrend(list(), data)
    ss <- bsts::AddSeasonal(ss, data, nseasons = season.num, season.duration = season.dur)}
  
  ##build Bayesial model
  bsts.model <- bsts::bsts(data, state.specification = ss, niter = 666, ping=0, seed=1000)
  
  ##predict
  result<-bsts::predict.bsts(bsts.model, horizon = pred.horizon, burn = SuggestBurn(0.1, bsts.model), quantiles = c(.025, .975))
  pred<-data.frame(result$mean,result$median)
  return(pred)
}

# Data 
us_df = read.csv("us_df.csv")
us_df = us_df[which(as.numeric(us_df$year) >= 1970 & as.numeric(us_df$year) <= 2014),]

# Time object
usco2_ts = ts(us_df[,1:2])
decompose(usco2_ts) # co2 does not have seasonality

# Try Bayesian with function
y<-rabsts(us_df$co2,season.num=0,pred.horizon=3) #If no season, set season.num=0.
y

Y = usco2_ts[,2]
y = log10(Y)

### Run the bsts model
ss <- AddLocalLinearTrend(list(), y)
bsts.model <- bsts(y, state.specification = ss, niter = 500, ping=0, seed=4444)

burn <- SuggestBurn(0.1, bsts.model)

p <- predict.bsts(bsts.model, horizon = 12, burn = burn, quantiles = c(.025, .975))

d2 <- data.frame(
  # fitted values and predictions
  c(10^as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),])+y),  
    10^as.numeric(p$mean)),
  # actual data and dates 
  as.numeric(us_df$co2),
  as.Date(time(us_df$co2)))
names(d2) <- c("Fitted", "Actual", "Date")

posterior.interval <- cbind.data.frame(10^as.numeric(p$interval[1,]),
                                       10^as.numeric(p$interval[2,]), 
                                       subset(d2, year(Date)>1959)$Date)
names(posterior.interval) <- c("LL", "UL", "Date")

d3 <- left_join(d2, posterior.interval, by="Date")
ggplot(data=d3, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("1959-12-01")), linetype=2) + 
  geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
  ggtitle(paste0("BSTS -- Holdout MAPE = ", round(100*MAPE,2), "%")) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
