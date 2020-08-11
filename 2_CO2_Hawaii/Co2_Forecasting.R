library(fBasics)
library(forecast) 

# Set the folder (path) that contains this R file as the working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

# *** MULTIPLICATIVE SARIMA MODELS ***
data<-read.csv("Datos CO2.csv",header=TRUE,sep=",",dec=".")
y<-data[,4] 

#y=(log(y))
# ts.plot(y)
nlags=120

# par(mfrow=c(2,1))
# acf(y,nlags)
# pacf(y,nlags)  

s=12       # Monthly Data

# nsdiffs(y,m=s,test=c("ocsb"))  # seasonal differences?
# ndiffs(y, alpha=0.05, test=c("adf")) # regular differences?

# estimate the SAR and analyze the estimated parameters. Compare with the Seasonal Difference
# fit<-arima(y,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=s)) 
# fit

# ts.plot(fit$residuals)
# 
# par(mfrow=c(2,1))
# acf(fit$residuals,nlags)
# pacf(fit$residuals,nlags)    
# 
# ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
# nsdiffs(fit$residuals, m=s,test=c("ocsb")) # seasonal differences?

#Box.test(fit$residuals,lag=36)

# shapiro.test(fit$residuals)  

# 0.01 for the first series
# 1 for the second series

# hist(fit$residuals,prob=T,ylim=c(0,10),xlim=c(mean(fit$residuals)-3*sd(fit$residuals),mean(fit$residuals)+3*sd(fit$residuals)),col="red")
# lines(density(fit$residuals),lwd=2)
# mu<-mean(fit$residuals)
# sigma<-sd(fit$residuals)
# x<-seq(mu-3*sigma,mu+3*sigma,length=100)
# yy<-dnorm(x,mu,sigma)
# lines(x,yy,lwd=2,col="blue")

#FORECASTING 
n<-length(y)

n.estimation<-612 # 
n.forecasting<-n-n.estimation # 120 observations
horizontes<-1 # number of periods ahead

predicc<-matrix(0,nrow=n.forecasting,ncol=horizontes)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE<-matrix(0,nrow=horizontes,ncol=1)
MAPE<-matrix(0,nrow=horizontes,ncol=1)

for (Periods_ahead in 1:horizontes) {
  for (i in 1:n.forecasting) {
    aux.y<-y[i:(n.estimation-Periods_ahead+i)]; #Change 1 to i for Rolling method
    fit<-arima(aux.y,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=s)); #Logs to AUX
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- y.pred$pred[Periods_ahead]; #Exp to Pred Y
  }
  error<-real-predicc[,Periods_ahead];
  MSFE[Periods_ahead]<-mean(error^2);
  MAPE[Periods_ahead]<-mean(abs(error/real)) *100;
}


y[613:732]
predicc[,1]

#new <- c(y,y.pred$pred)# real data + predicted values
new <- c(y[1:612],predicc[,1]) # real data + predicted values

plot.ts(new,main="Predictions",col=3,lwd=2) # time series plot
lines(y[1:612],col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4),
       bty="n",lwd=2)

MSFE
MAPE
