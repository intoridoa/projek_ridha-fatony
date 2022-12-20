#Input Data dan Statistika Deskriptif
#Oceanic Nino Index (ONI)
library(xlsx)
library(tseries)
library(forecast)
library(lmtest)
data_oni <- read.xlsx("/home/ridtowa/Downloads/Tubes Statdas/Data Tubes Statdas - Kel.22.xlsx", sheetName = "ONI",header=TRUE)

indeks_oni <- data_oni$ONI

#Data Deret Waktu
indeksts_oni <- ts(indeks_oni)
#Scatter Plot Data Deret Waktu
plot(indeksts_oni, main="Grafik Oceanic Nino Index (ONI) Tahun 2012-2019",ylab="Indeks",xlab="Bulan",
     type='o') + abline(h=mean(indeks_oni),col='blue')

#Data Statistika Deskriptif
summary(indeks_oni)

#Memodelkan Data
#Uji Kestasioneran
#Uji ADF (Augmented Dickey-Fuller)
adf.test(indeksts_oni)

#Diferensiasi Model Nonstasioner
indeksts_oni_diff = diff(indeksts_oni)
plot(indeksts_oni_diff, main="Grafik 1x Diferensiasi Data Oceanic Nino Index 
     (ONI) Tahun 2012-2019", ylab="Diferensiasi", xlab="Bulan", type='o') +
  abline(h=mean(indeksts_oni_diff), col = 'blue')

#Plot ACF 1x Diferensiasi
acf(indeksts_oni_diff, main="Grafik ACF 1x Diferensiasi Data Oceanic Nino Index 
    (ONI) Tahun 2012-2019", lag.max=100)
#Plot PACF 
pacf(indeksts_oni_diff, main="Grafik PACF 1x Diferensiasi Data Oceanic Nino Index 
     (ONI) Tahun 2012-2019", lag.max=100)

#Uji ADF (Augmented Dickey-Fuller) Model Nonstasioner yang sudah Didiferensikan
adf.test(indeksts_oni_diff)

#Estimasi Model secara Manual
#ACF Tail off
#PACF Cut-off lag 1, 3, 6, dan 9

model_arima1 = arima(indeksts_oni,order = c(1,1,0))
summary(model_arima1) #aic = -104.84

model_arima2 = arima(indeksts_oni,order = c(3,1,0))
summary(model_arima2) #aic = -114.01

model_arima3 = arima(indeksts_oni,order = c(6,1,0))
summary(model_arima3) #aic = -119.79

model_arima4 = arima(indeksts_oni,order = c(9,1,0))
summary(model_arima4) #aic = -120.66

coeftest(model_arima4)

#Uji Diagnostik
#Uji Ljung-Box
checkresiduals(model_arima4)

#Perbandingan Model dan Data
data_pred_oni <- indeksts_oni - residuals(model_arima4)
ts.plot(data_pred_oni, indeksts_oni, xlab= "Bulan", ylab= "Indeks",
        col=c('red','blue'), main="Perbandingan Data Oceanic Nino Index 
     (ONI) Tahun 2012-2019 Asli dengan Model")
legend("topright",legend=c("model","asli"),col=c('red','blue'),lty = 1:1,cex=0.8)

#Prediksi 
model_arima4 = stats:: arima(indeksts_oni,order = c(9,1,0))
(prediksi_oni = forecast(model_arima4, h=12))

autoplot(prediksi_oni, main="Prediksi Data Oceanic Nino Index (ONI)",
         ylab="Index", xlab="Bulan")
