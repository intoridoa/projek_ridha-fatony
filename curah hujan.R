#Input Data dan Statistika Deskriptif
#Curah Hujan di Kabupaten Lombok Timur
library(xlsx)
library(tseries)
library(forecast)
library(lmtest)
data_ch <- read.xlsx("/home/ridtowa/Downloads/Tubes Statdas/Data Tubes Statdas - Kel.22.xlsx", sheetName = "Curah_Hujan",header=TRUE)

indeks_ch <- data_ch$Curah_Hujan

#Data Deret Waktu
indeksts_ch <- ts(indeks_ch)
#Scatter Plot Data Deret Waktu
plot(indeksts_ch, main="Grafik Curah Hujan Bulanan di Provinsi 
     DKI Jakarta Tahun 2012-2019",
     ylab="Curah Hujan (mm)", xlab="Bulan", type='o') + 
     abline(h=mean(indeksts_ch),col='blue')

#Data Statistika Deskriptif
summary(indeks_ch)

#Memodelkan Data
#Uji Kestasioneran
#Plot ACF
acf(indeksts_ch, main="Grafik ACF Data Curah Hujan Bulanan di Provinsi 
     DKI Jakarta Tahun 2012-2019", lag.max=100)
#Plot PACF
pacf(indeksts_ch, main="Grafik PACF Data Curah Hujan Bulanan di Provinsi 
     DKI Jakarta Tahun 2012-2019", lag.max=100)

#Uji ADF (Augmented Dickey-Fuller)
adf.test(indeksts_ch)
plot(indeksts_ch, main="Grafik Data Curah Hujan Bulanan di Provinsi 
     DKI Jakarta Tahun 2012-2019",
     ylab="Curah Hujan (mm)", xlab="Bulan", type='o') + abline(h=mean(indeksts_ch),col='blue')

#Estimasi Model secara Manual
#ACF Tail off
#PACF Cut-off lag 1 dan 12
model_arima1 = arima(indeksts_ch, order =c(1,0,0))
summary(model_arima1) #aic = 1244.56

model_arima2 = arima(indeksts_ch, order =c(12,0,0))
summary(model_arima2) #aic = 1231.19

coeftest(model_arima2)

#Uji Diagnostik
checkresiduals(model_arima2)

#Perbandingan Model dan Data
data_pred_ch <- indeksts_ch - residuals(model_arima2)
ts.plot(data_pred_ch, indeksts_ch, xlab= "Bulan", ylab= "Curah Hujan (mm)",
        col=c('red','blue'), main="Perbandingan Data Curah Hujan Bulanan di Provinsi 
     DKI Jakarta Tahun 2012-2019 Asli dengan Model")
legend("topright",legend=c("model","asli"),col=c('red','blue'),lty = 1:1,cex=0.8)

#Prediksi 
model_arima2 = stats:: arima(indeksts_ch, order =c(12,0,0))
(prediksi_ch = forecast(model_arima2, h=12))

autoplot(prediksi_ch, main="Prediksi Data Curah Hujan Bulanan di Provinsi DKI Jakarta", 
         ylab="Curah Hujan (mm)", xlab="Bulan")

#Perbandingan data curah hujan dan oni
oni_std <- scale(indeks_oni, center=TRUE, scale=TRUE)
ch_std <- scale(indeks_ch, center=TRUE, scale=TRUE)
onits_std <- ts(oni_std)
chts_std <- ts(ch_std)
ts.plot(onits_std, chts_std, xlab= "Bulan",
        col=c('red','blue'), main="Perbandingan Data Oceanic Nino Index (ONI)
        dan Curah Hujan Bulanan di Provinsi 
     DKI Jakarta Tahun 2012-2019")
legend("topright",legend=c("ONI","Curah Hujan"),col=c('red','blue'),lty = 1:1,cex=0.8)