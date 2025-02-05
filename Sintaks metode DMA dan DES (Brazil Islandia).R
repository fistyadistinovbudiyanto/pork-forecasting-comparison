# Import Library
library(ggplot2)
library(forecast)
library(tseries)
library(lmtest)
library(dplyr)

########## DEMAND BRAZIL ##########
# Import Dataset 
data<-read.csv("C:/Users/Mas Win/Downloads/per-capita-meat-consumption-by-type-kilograms-per-year.csv", header = T)

# Select Indonesia and Nigeria from data, split to two different df
data_Brazil <- data %>% filter(Entity == "Brazil")


# Tampilkan beberapa baris pertama dari data China
head(data_Brazil)

data_Brazil = ts(data_Brazil$Meat..pig...00002733....Food.available.for.consumption...0645pc....kilograms.per.year.per.capita, start=(1961),freq = 1)
adf.test(data_Brazil)


k=5 #mendefinisikan nilai k
n=length(data_Brazil)
MA=array(NA,dim=c(n)) # inisiasi MA

# perulangan untuk forecast rata-rata bergerak tunggal
for(i in 1:n){
  MA[i+(k-1)]=mean(data_Brazil[i:(i+(k-1))])
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
  e[i]=(data_Brazil[i+(m-1)+(k-1)+2]-Prediksi[i+(m-1)+(k-1)+2])^2
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
plot(data_Brazil,col="black",lwd=1.0, xlab="Tahun", ylab="Demand", main="Plot Data Asli vs Ramalan DMA")
limitDate=end(data_Brazil)[1]+(end(data_Brazil)[2]-1)/frequency(data_Brazil)
abline(v=limitDate ,lty=4)
lines(Ramalan,col="blue", lwd=2)
legend("topleft", c("Data Asli", "Ramalan DMA"), bty="n", lwd=2, col=c("black", "blue"))

# plot fitting
plot(data_Brazil,col="blue",lwd=1.0, xlab="Tahun", ylab="Demand", main="Plot Data Asli vs Fitting DMA")
lines(Prediksi, col="red", lwd=2)
legend("topleft", c("Data Asli", "Hasil DMA"), bty="n", lwd=2, col=c("blue", "red"))


# 3 Double Exponential Smoothing
Holt<-holt(data_Brazil,h=5) #langsung memunculkan nilai hasil ramalan
Holt
autoplot(Holt)
accuracy(Holt)

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
    model <- holt_manual(data_Brazil, Alpha = alpha, Gamma = gamma, h = 5)
    rmse <- calculate_rmse(model$Error)
    results <- rbind(results, data.frame(Alpha = alpha, Gamma = gamma, RMSE = rmse))
  }
}

# Menemukan kombinasi Alpha dan Gamma dengan RMSE terkecil
optimal_params <- results %>% filter(RMSE == min(RMSE))
optimal_params

# Menggunakan nilai Alpha dan Gamma yang optimal untuk membuat prediksi
best.holt <- holt_manual(data_Brazil, Alpha = optimal_params$Alpha, Gamma = optimal_params$Gamma, h = 5)
RMSE.best.holt <- sqrt(mean(best.holt$Error, na.rm = TRUE))
RMSE.best.holt
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
plot(data.plot, type = "l", col = "blue", lwd = 2, xlab = "Tahun", ylab = "Demand Brazil", main = "Plot Data Asli dan Ramalan 2 Parameter Holt")
lines(forecast.holt, col = "red", lwd = 2)
legend("topleft", c("Asli", "Ramalan"), bty = "n", lwd = 2, col = c("blue", "red"))







########## DEMAND ISLANDIA ##########
# Select Indonesia and Nigeria from data, split to two different df
data_Islandia <- data %>% filter(Entity == "Iceland")


# Tampilkan beberapa baris pertama dari data China
head(data_Islandia)

data_Islandia = ts(data_Islandia$Meat..pig...00002733....Food.available.for.consumption...0645pc....kilograms.per.year.per.capita, start=(1961),freq = 1)
adf.test(data_Islandia)


k=5 #mendefinisikan nilai k
n=length(data_Islandia)
MA=array(NA,dim=c(n)) # inisiasi MA

# perulangan untuk forecast rata-rata bergerak tunggal
for(i in 1:n){
  MA[i+(k-1)]=mean(data_Islandia[i:(i+(k-1))])
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
  e[i]=(data_Islandia[i+(m-1)+(k-1)+2]-Prediksi[i+(m-1)+(k-1)+2])^2
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
plot(data_Islandia,col="black",lwd=1.0, xlab="Tahun", ylab="Demand", main="Plot Data Asli vs Ramalan DMA")
limitDate=end(data_Islandia)[1]+(end(data_Islandia)[2]-1)/frequency(data_Islandia)
abline(v=limitDate ,lty=4)
lines(Ramalan,col="blue", lwd=2)
legend("topleft", c("Data Asli", "Ramalan DMA"), bty="n", lwd=2, col=c("black", "blue"))

# plot fitting
plot(data_Islandia,col="blue",lwd=1.0, xlab="Tahun", ylab="Demand", main="Plot Data Asli vs Fitting DMA")
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
    model <- holt_manual(data_Islandia, Alpha = alpha, Gamma = gamma, h = 5)
    rmse <- calculate_rmse(model$Error)
    results <- rbind(results, data.frame(Alpha = alpha, Gamma = gamma, RMSE = rmse))
  }
}

# Menemukan kombinasi Alpha dan Gamma dengan RMSE terkecil
optimal_params <- results %>% filter(RMSE == min(RMSE))
optimal_params

# Menggunakan nilai Alpha dan Gamma yang optimal untuk membuat prediksi
best.holt <- holt_manual(data_Islandia, Alpha = optimal_params$Alpha, Gamma = optimal_params$Gamma, h = 5)
RMSE.best.holt <- sqrt(mean(best.holt$Error, na.rm = TRUE))
RMSE.best.holt
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
plot(data.plot, type = "l", col = "blue", lwd = 2, xlab = "Tahun", ylab = "Demand Islandia", main = "Plot Data Asli dan Ramalan 2 Parameter Holt")
lines(forecast.holt, col = "red", lwd = 2)
legend("topleft", c("Asli", "Ramalan"), bty = "n", lwd = 2, col = c("blue", "red"))

