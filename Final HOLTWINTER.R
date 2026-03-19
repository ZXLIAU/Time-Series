data<-read.csv("TWD_JPY Historical Data1.csv")
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
data.ts<-ts(data$Close,frequency=260,start=c(2017,1))
TWD_JPY_ts <- xts(data$Close, order.by=data$Date)
plot(TWD_JPY_ts)
# initial value #
#l0(level)
l0<-mean(data$Close)
#b0 (trend)
D1<-mean(data$Close[1:258])
D5<-mean(data$Close[1563:1802])
q<-5
s<-1802
b0<-(D5-D1)/((q-1)*s) # slope between the average of the last available full period q and the first by WInter
#s0(seasonal)
Sm<-261
S0<-D1-((Sm+1)/2)*b0  # moving average of the period
#install.packages("forecast")
library(forecast)
decomposed<-decompose(data.ts)
autoplot(decomposed)
summary(decomposed)
# additive seasonal & additive trend
## use TSE:Holt-Winters Methods ##

hw.fit2 = HoltWinters(data.ts,alpha=0.8,beta=0.2,gamma=0.8,seasonal=c('additive'),l.start=l0,b.start=b0,s.start=s0)
MSE<-hw.fit2$SSE/length(hw.fit2$x)
MSE

plot(hw.fit2,xlim=c(2017,2024),ylab="Closing Price",main = "TWD_JPY Holt Winters forecasting")  #downward
hw.for<-forecast(hw.fit2,h=260,level=c(80,95))
plot(hw.for)
plot(hw.for,ylim=c(3,6))
summary(hw.for)
accuracy(hw.for)
checkresiduals(hw.for$residuals) #Ljung-Box test Ho: is a WN =>p-value<0.05 reject Ho 
Box.test(data.ts,type = "Ljung-Box")
#####################################
resd<-diff(data.ts)
hw.fit3 = HoltWinters(resd,alpha=0.8,beta=0.2,gamma=0.8,seasonal=c('additive'),l.start=l0,b.start=b0,s.start=S0)
MSE<-hw.fit3$SSE/length(hw.fit3$x)
MSE

hw.fo<-forecast(hw.fit3,h=260,level=c(80,95))
plot(hw.fo)
plot(hw.fo,ylim=c(-1,1))
summary(hw.fo)
accuracy(hw.fo)
checkresiduals(hw.fit3)
###########################################################
## AIC(Akaike information criterion) ##
#AIC of Holt-Winters Methods = 
# AIC is an estimator of prediction error 
#and thereby relative quality of statistical models for a given set of data
# smallest better and negative not sure


'''
#whats represent?
accuracy(hw.fit1)

##another method
fit1 <- ets(data.ts, model="MAA", damped=FALSE)
fcast1 <- forecast(fit1, h=24)
plot(fcast1)
summary(fcast1)
'''


