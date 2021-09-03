#import dataset
library(MASS)
library(tseries)
library(readxl)
library(forecast)
library(ggplot2)
Tingkat_Inflasi <- read_excel("~/Magang/Artikel/ARIMA/Tingkat Inflasi.xlsx")
View(Tingkat_Inflasi)
data <- ts(Tingkat_Inflasi$Inflasi, start=2014, frequency = 12)
head(data)
data
#lihat pola datanya
plot(data)
#uji stasioneritas
adf.test(data)

adf.test(datax[1:12])
lmd=BoxCox.lambda(data)
lmd
dataln=log(data+1)
datax=ts(dataln,start = c(2014,1),frequency = 12)
plot(datax)
BoxCox.lambda(datax)
datatr=(((data+1)^lmd)-1)/lmd
BoxCox.lambda(datatr)

par(mfrow=c(1,2))
acf(datax)
pacf(datax)

model1=Arima(datax,order = c(0,0,1),include.drift = T)
printarima(model1)

model2=Arima(datax,order = c(0,0,1),include.drift = T) #signifikan
printarima(model2)

model3=Arima(datax,order = c(2,0,0),include.drift = T) #signifikan
printarima(model3)

model4=Arima(datax,order = c(4,0,0),include.drift = T)
printarima(model4)

model5=Arima(datax,order = c(3,0,0),include.drift = T) #signifikan
printarima(model5)

model6=Arima(datax,order = c(2,0,1),include.drift = T) #signifikan
printarima(model6)

m2 <- matrix(data=c('AIC','BIC',model2$aic,model2$bic), nrow=2
m3 <- c(model3$aic,model3$bic)
m3
compare = data.frame(c(m2,m3))
compare

model2
model3
model5
model6

#lihat plot acf pacf
acf(data)
pacf(data) #pake ar(2)

par(mfrow=c(1,1))
#pemodelan
model <- arima(data, order = c(1,0,0))
model
#uji diagnostik
Box.test(datax, type = 'Ljung-Box')
tsdiag(model)

resi <- model6$residuals
Box.test(resi, type = 'Ljung-Box')

Box.test(res, type = 'Ljung-Box')
res <- model$residuals


#seasonal

model=Arima(datax,order = c(3,0,0),seasonal=c(0,0,1),include.drift = T) #signifikan
printarima(model)
model


#diagnostic checking
resi = model$residuals
Box.test(resi, type='Ljung-Box')

#ramalkan
peramalan <- forecast(model, h=6)
trf.peramalan <- backtransfor_bc2(data=data,lambda=lmd, forecastt=peramalan)
trf.peramalan
autoplot(trf.peramalan)
autoplot(trf.peramalan)
autoplot(data)
autoplot(data)
peramalan
autoplot(peramalan)

#transformasi
data.trf <- trans_boxc(data=data, lambda = lmd)
plot(data.trf)
model=Arima(datax,order = c(3,0,0),seasonal=c(0,0,1),include.drift = T) #signifikan
printarima(model)
model
autoplot(trf.peramalan)
lmd
par(mfrow=c(1,1))


peramalan<-forecast::forecast(model,h=12)
peramalan
back_transform_forecast2<-backlog.c1(data=data,forecastt=peramalan)
back_transform_forecast2
plot(peramalan$residuals)
tsdiag(peramalan)
Box.test(peramalan$residuals, type='Ljung-Box')
acf(peramalan$residuals)

plot(back_transform_forecast2)
autoplot(peramalan)
datay=data

peramalan
plot(back_transform_forecast2)
plot(peramalan)
tsdiag(peramalan)
model
printarima(model)
plot(data)


######
backlog.c1<-function(data,forecastt) {
  forecastt[["mean"]]<-exp(forecastt[["mean"]])-1
  forecastt[["upper"]]<-exp(forecastt[["upper"]])-1
  forecastt[["lower"]]<-exp(forecastt[["lower"]])-1
  forecastt[["x"]]<-data
  forecastt
}


