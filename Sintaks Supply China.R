# Import Library
library(ggplot2)
library(forecast)
library(tseries)
library(lmtest)
library(dplyr)


# 1 ARIMA
########## SUPPLY CHINA ##########
# Import Dataset 
data = read.csv("D:/a semester 4/metode peramalan/project akhir/pigmeat-production-tonnes.csv")
# Filter data untuk China dan batasi sampai tahun 2021
data_china <- data %>%
  filter(Entity == "China" & Year <= 2021)

# Tampilkan beberapa baris pertama dari data China
head(data_china)

# Filter data untuk China dan batasi sampai tahun 2021
data_dunia <- data %>%
  filter(Entity == "World" & Year <= 2021)

# Tampilkan beberapa baris pertama dari data China
head(data_dunia)

# Plot perbandingan produksi babi dunia dan china
data_china$Entity <- "China"
data_dunia$Entity <- "World"

data_combined <- rbind(data_china, data_dunia)

ggplot(data_combined, aes(x = Year, y = Meat..pig...00001035....Production...005510....tonnes, color = Entity)) + 
  geom_line() + 
  xlab("Tahun") + 
  ylab("Jumlah Produksi") + 
  ggtitle("Plot Jumlah Produksi Babi di China dan Dunia") +
  scale_color_manual(values = c("China" = "#FDBCB4", "World" = "#B3CDE3")) +  # Warna pastel
  theme_minimal()

data_china = ts(data_china$Meat..pig...00001035....Production...005510....tonnes, start=(1961),freq = 1)
adf.test(data_china)

##### Transformasi Data #####
##Checking the Number of Differences Required
ndiffs(log(data_china),"adf")

#Diff 1
ddif1 = diff(data_china, differences=1)
adf.test(ddif1)
autoplot(ddif1)

#Diff 2
ddif2 = diff(data_china, differences=2)
adf.test(ddif2)
autoplot(ddif2)

#Diff 1 with Log-Trans
dtrans1 = diff(log(data_china), differences=1)
adf.test(dtrans1)
autoplot(dtrans1)

#Diff 2 with Log-Trans
dtrans2 = diff(log(data_china), differences=2)
adf.test(dtrans2)
autoplot(dtrans2)


########Identifikasi Model#########
# Plot Autocorrelation Function
ggAcf(dtrans1,lag.max = 12) + ggtitle("ACF")

# Plot Partial Autocorrelation Function
ggPacf(dtrans1,lag.max = 12) + ggtitle("PACF")

# diperoleh ARIMA (1,1,1)
# Model With Constant 
c111<-Arima(data_china,order=c(1,1,1),include.constant = T, lambda = 0)
coeftest(c111)

# signifikan
c110<-Arima(data_china,order=c(1,1,0),include.constant = T, lambda = 0)
coeftest(c110)

# signifikan
c011<-Arima(data_china,order=c(0,1,1),include.constant = T, lambda = 0)
coeftest(c011)

# Model Without Constant 
tc111<-Arima(data_china,order=c(1,1,1),include.constant = F, lambda = 0)
coeftest(tc111)

# signifikan
tc110<-Arima(data_china,order=c(1,1,0),include.constant = F, lambda = 0)
coeftest(tc110)

# signifikan
tc011<-Arima(data_china,order=c(0,1,1),include.constant = F, lambda = 0)
coeftest(tc011)

#### Diagnostic Checking ####
# c110
Box.test(c110$residuals,type="Ljung") #uji autokorelasi (TERPENUHI)
Box.test((c110$residuals)^2,type="Ljung") #uji homoskedastik (TERPENUHI)
jarque.bera.test(c110$residuals) #uji normalitas (TIDAK TERPENUHI)
checkresiduals(c110)

# c011
Box.test(c011$residuals,type="Ljung") #uji autokorelasi (TERPENUHI)
Box.test((c011$residuals)^2,type="Ljung") #uji homoskedastik (TERPENUHI)
jarque.bera.test(c011$residuals) #uji normalitas (TIDAK TERPENUHI)
checkresiduals(c011)

# tc110
Box.test(tc110$residuals,type="Ljung") #uji autokorelasi (TERPENUHI)
Box.test((tc110$residuals)^2,type="Ljung") #uji homoskedastik (TERPENUHI)
jarque.bera.test(tc110$residuals) #uji normalitas (TIDAK TERPENUHI)
checkresiduals(tc110)

# tc011
Box.test(tc011$residuals,type="Ljung") #uji autokorelasi (TERPENUHI)
Box.test((tc011$residuals)^2,type="Ljung") #uji homoskedastik (TERPENUHI)
jarque.bera.test(tc011$residuals) #uji normalitas (TIDAK TERPENUHI)
checkresiduals(tc011)

#Best model selection
c110
c011
tc110
tc011

mod_c110 = data.frame(Model = "c110", LogLik = logLik(c110), AIC = AIC(c110), BIC = BIC(c110))
mod_c011 = data.frame(Model = "c011", LogLik = logLik(c011), AIC = AIC(c011), BIC = BIC(c011))
mod_tc110 = data.frame(Model = "tc110", LogLik = logLik(tc110), AIC = AIC(tc110), BIC = BIC(tc110))
mod_tc011 = data.frame(Model = "tc011", LogLik = logLik(tc011), AIC = AIC(tc011), BIC = BIC(tc011))

model_selection = rbind(mod_c110, mod_c011, mod_tc110, mod_tc011)
model_selection

#Best model
##Plot##
autoplot(c011$x, col="darkblue") + 
  autolayer(fitted(c011), series = "Data Ramalan") + 
  ylab("Jumlah Produksi") + 
  ggtitle("Plot Data Asli vs Ramalan")

forecast_china <- forecast(c011, h = 5)
accuracy(forecast_china)
autoplot(forecast_china) + autolayer(data_china, series = "Data") + xlab("Year") + ylab("Kilograms per year") + ggtitle("Forecasting Pig Meat Production in China")


# 2 Double Moving Average
# Import Dataset 
data = read.csv("D:/a semester 4/metode peramalan/project akhir/pigmeat-production-tonnes.csv")
# Filter data untuk China dan batasi sampai tahun 2021
data_china <- data %>%
  filter(Entity == "China" & Year <= 2021)

# Tampilkan beberapa baris pertama dari data China
head(data_china)
data_china = ts(data_china$Meat..pig...00001035....Production...005510....tonnes, start=(1961),freq = 1)
adf.test(data_china)


k=5 #mendefinisikan nilai k
n=length(data_china)
MA=array(NA,dim=c(n)) # inisiasi MA

# perulangan untuk forecast rata-rata bergerak tunggal
for(i in 1:n){
  MA[i+(k-1)]=mean(data_china[i:(i+(k-1))])
}
#Hitung DMA dengan orde m
m=3 #mendefinisikan nilai m
DMA=array(NA,dim=c(n)) #inisiasi DMA
# perulangan untuk forecast rata-rata bergerak ganda
for(i in 1:n){
  DMA[i+(m-1)+(k-1)]=mean(MA[(i+(k-1)):(i+(m-1)+(k-1))])
}
#Hitung koefisien at
at=array(NA,dim=c(n))
for(i in 1:n){
  at[i+(m-1)+(k-1)]=2*MA[i+(m-1)+(k-1)]-DMA[i+(m-1)+(k-1)]
}
#Hitung koefisien bt
bt=array(NA,dim=c(n))
for(i in 1:n){
  bt[i+(m-1)+(k-1)]=(2/(k-1))*(MA[i+(m-1)+(k-1)]-DMA[i+(m-1)+(k-1)])
}
# nilai prediksi
Prediksi=array(NA,dim=c(n))
for(i in 1:n){
  Prediksi[i+(m-1)+(k-1)+2]=at[i+(m-1)+(k-1)]+bt[i+(m-1)+(k-1)]*2
}
# nilai error
e=array(NA,dim=c(n))
for(i in 1:n){
  e[i]=(data_china[i+(m-1)+(k-1)+2]-Prediksi[i+(m-1)+(k-1)+2])^2
}
MSE.dma=mean(e,na.rm=TRUE) #nilai MSE, na.rm=TRUE -> missing value dihilangkan
RMSE.dma = sqrt(MSE.dma) #nilai RMSE
forecast.error.dma <- data.frame('MSE'=MSE.dma, 'RMSE'=RMSE.dma) #membentuk data frame hasil MSE dan RMSE
forecast.error.dma

# melakukan peramalan
forecast.ts=function(h,N){
  at[n-h]+bt[n-h]*N
}
# ramalan 5 (t=5) periode kedepan
t=5
ramalan=ts(forecast.ts(t-1:t,t))
ramalan

# data time series
Prediksi=ts(Prediksi, start=c(1961), freq=1)
Ramalan=ts(ramalan, start=c(2022), freq=1)
Ramalan

# plot ramalan
plot(data_china,col="black",lwd=1.0, xlab="Tahun", ylab="Demand", main="Plot Data Asli vs Ramalan DMA")
limitDate=end(data_china)[1]+(end(data_china)[2]-1)/frequency(data_china)
abline(v=limitDate ,lty=4)
lines(Ramalan,col="blue", lwd=2)
legend("topleft", c("Data Asli", "Ramalan DMA"), bty="n", lwd=2, col=c("black", "blue"))

# plot fitting
plot(data_china,col="blue",lwd=1.0, xlab="Tahun", ylab="Demand", main="Plot Data Asli vs Fitting DMA")
lines(Prediksi, col="red", lwd=2)
legend("topleft", c("Data Asli", "Hasil DMA"), bty="n", lwd=2, col=c("blue", "red"))


# 3 Double Exponential Smoothing

# Definisikan fungsi Holt dengan parameter h untuk prediksi lebih dari satu periode
holt_manual <- function(y, Alpha, Gamma, h = 1, start = y[1]){
  st_par <- y
  st_par[1] <- start # Inisiasi untuk s1
  bt_par <- y
  bt_par[1] <- y[2] - y[1] # Inisiasi untuk b1
  
  n <- length(y) # Panjang data asli
  forecast <- array(NA, dim = c(n + h)) # Vektor kosong untuk menampung ramalan
  error <- array(NA, dim = c(n)) # Vektor kosong untuk menampung error
  
  # Perulangan untuk menghitung st dan bt
  for (i in 2:n) {
    st_par[i] <- Alpha * y[i] + (1 - Alpha) * (st_par[i - 1] + bt_par[i - 1])
    bt_par[i] <- Gamma * (st_par[i] - st_par[i - 1]) + (1 - Gamma) * bt_par[i - 1]
  }
  
  # Perulangan untuk menghitung ramalan dalam periode data asli
  for (i in 1:n) {
    forecast[i] <- st_par[i] + bt_par[i]
  }
  
  # Perulangan untuk menghitung ramalan untuk periode tambahan
  for (i in 1:h) {
    forecast[n + i] <- st_par[n] + i * bt_par[n]
  }
  
  # Menghitung error
  for (i in 1:n) {
    error[i] <- (y[i] - forecast[i])^2
  }
  
  max_ln <- max(c(length(y), length(st_par), length(forecast), length(bt_par), length(error))) # Mencari variabel dengan panjang data terpanjang
  df <- data.frame(
    Data = c(y, rep(NA, max_ln - length(y))), # Menambahkan baris dengan nilai NA untuk menyamakan panjang data dengan panjang data terpanjang
    St = c(st_par, rep(NA, max_ln - length(st_par))), # Menghitung St sesuai formula yang ada
    Bt = c(bt_par, rep(NA, max_ln - length(bt_par))), # Menghitung Bt sesuai formula yang ada
    Forecast = c(forecast, rep(NA, max_ln - length(forecast))), # Menghitung nilai prediksi/peramalan
    Error = c(error, rep(NA, max_ln - length(error))) # Menghitung nilai error
  )
  
  return(df) # Menampilkan hasil ramalan
}

# Definisikan rentang nilai Alpha dan Gamma
alpha_values <- seq(0.1, 0.9, by = 0.1)
gamma_values <- seq(0.1, 0.9, by = 0.1)

# Tempat untuk menyimpan hasil RMSE
results <- data.frame(Alpha = numeric(0), Gamma = numeric(0), RMSE = numeric(0))

# Fungsi untuk menghitung RMSE
calculate_rmse <- function(errors) {
  sqrt(mean(errors, na.rm = TRUE))
}

# Loop untuk grid search
for (alpha in alpha_values) {
  for (gamma in gamma_values) {
    model <- holt_manual(data_china, Alpha = alpha, Gamma = gamma, h = 5)
    rmse <- calculate_rmse(model$Error)
    results <- rbind(results, data.frame(Alpha = alpha, Gamma = gamma, RMSE = rmse))
  }
}

# Menemukan kombinasi Alpha dan Gamma dengan RMSE terkecil
optimal_params <- results %>% filter(RMSE == min(RMSE))
optimal_params

# Menggunakan nilai Alpha dan Gamma yang optimal untuk membuat prediksi
best.holt <- holt_manual(data_china, Alpha = optimal_params$Alpha, Gamma = optimal_params$Gamma, h = 5)
RMSE.best.holt <- sqrt(mean(best.holt$Error, na.rm = TRUE))

# Fungsi untuk menampilkan hasil forecast beserta Alpha, Gamma, dan RMSE paling optimal
result.holt <- function(best.holt, RMSE.best.holt, Alpha, Gamma){
  print(best.holt)
  cat("\n", "-----------------\n", "RMSE = ", RMSE.best.holt,
      "\n", "Alpha = ", Alpha,
      "\n", "Gamma = ", Gamma)
}

result.holt(best.holt, RMSE.best.holt, optimal_params$Alpha, optimal_params$Gamma) # Hasil

##### Data Time Series #####
data.plot <- ts(best.holt$Data, start = c(1961), freq = 1) # Membuat data menjadi bentuk timeseries
forecast.holt <- ts(best.holt$Forecast, start = c(1961), freq = 1)
forecast.holt
##### Plot Data #####
plot(data.plot, type = "l", col = "blue", lwd = 2, xlab = "Tahun", ylab = "Supply China", main = "Plot Data Asli dan Ramalan 2 Parameter Holt")
lines(forecast.holt, col = "red", lwd = 2)
legend("topleft", c("Asli", "Ramalan"), bty = "n", lwd = 2, col = c("blue", "red"))

