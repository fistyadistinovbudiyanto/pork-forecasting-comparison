# Import Library
library(dplyr)
library(readxl)
library(readr)
library(forecast)
library(lmtest)
library(tseries)
data <- read_csv("00. Tugas Teori/UAS PROJECT METOPER/per-capita-meat-consumption-by-type-kilograms-per-year.csv")
data

# Select Indonesia and Nigeria from data, split to two different df
data_indonesia <- data %>% filter(Entity == "Indonesia")
data_nigeria <- data %>% filter(Entity == "Nigeria")
data_indonesia

# Print column data
colnames(data_indonesia)


#Change column name that 7th column to Pig Meat Consumption
colnames(data_indonesia)[7] <- "Value"

# Take only year and Pig Meat Consumption
data_indonesia <- data_indonesia %>% select(Year, Value)
data_indonesia

# Do same with Nigeria
colnames(data_nigeria)[7] <- "Value"
data_nigeria <- data_nigeria %>% select(Year, Value)

# Change to time series data
data_indonesia_ts <- ts(data_indonesia$Value, start = 1961, end = 2021)
data_nigeria_ts <- ts(data_nigeria$Value, start = 1961, end = 2021)

# Plot the data with ggplot2
library(ggplot2)
ggplot(data = data_indonesia, aes(x = Year, y = Value)) + geom_line() + labs(title = "Pig Meat Consumption in Indonesia", x = "Year", y = "Kilograms per year")

ggplot(data = data_nigeria, aes(x = Year, y = Value)) + geom_line() + labs(title = "Pig Meat Consumption in Nigeria", x = "Year", y = "Kilograms per year")


# FORECASTING INDONESIA DATA

# Indentify Seasonality 
ggseasonplot(data_indonesia_ts, year.labels = TRUE, year.labels.left = TRUE,    year.labels.right = TRUE, xlab("Year"), ylab("Kilograms per year"), main="Seasonal plot of Pig Meat Consumption in Indonesia")

# Check stasionary data with ADF test
adf.test(data_indonesia_ts)

# Differencing ndiffs
ndiffs(data_indonesia_ts)

# Differencing
data_indonesia_diff <- diff(data_indonesia_ts)
adf.test(data_indonesia_diff)
#Result: p-value = 0.02 < 0.05, reject null hypothesis, data is stasionary
autoplot(data_indonesia_diff)

# Differencing with log
data_indonesia_diff_log <- diff(log(data_indonesia_ts))
adf.test(data_indonesia_diff_log)
#Result: p-value = 0.02 < 0.05, reject null hypothesis, data is stasionary
autoplot(data_indonesia_diff_log)

# Only one differencing with log can make data stasionary, so d = 1

# Check ACF and PACF
ggAcf(data_indonesia_diff_log, lag.max = 12) + ggtitle("ACF of Pig Meat Consumption in Indonesia (d = 1)") 

ggPacf(data_indonesia_diff_log, lag.max = 12) + ggtitle("PACF of Pig Meat Consumption in Indonesia (d = 1)")


# Identified model is ARIMA(0,1,0)

# Model Underfitting
## The underfitting would be done using for-loop:
### Function to create model name
create_model_name <- function(p, d, q, constant) {
  paste0(ifelse(constant, "c", "nc"), p, d, q)
}
results <- data.frame(Model = character(), Coef = character(), P_Value = numeric(), Conclusion = character(), stringsAsFactors = FALSE)
model_list <- list()
max_p <- 1
max_d <- 1
max_q <- 1

for (p in 0:max_p) {
  for (d in 1:max_d) {
    for (q in 0:max_q) {
      if (p == 0 && q == 0) next
      for (constant in c(TRUE, FALSE)) {
        model_name <- create_model_name(p, d, q, constant)
        model <- Arima(dt, order = c(p, d, q), include.constant = constant, lambda = 0)
        test_results <- coeftest(model)
        significant <- TRUE
        for (i in 1:nrow(test_results)) {
          coef_name <- rownames(test_results)[i]
          p_value <- round(test_results[i, "Pr(>|z|)"], 3)
          conclusion <- ifelse(p_value < 0.05, "Significant", "Not Significant")
          results <- rbind(results, data.frame(Model = model_name, Coef = coef_name, P_Value = p_value, Conclusion = conclusion, stringsAsFactors = FALSE))
          if (conclusion == "Not Significant") {
            significant <- FALSE
          }
        }
        if (significant) {
          model_list[[model_name]] <- model
        }
      }
    }
  }
}
print(results)

### Detect all significant models
all_significant_models <- data.frame(Model = names(model_list), stringsAsFactors = FALSE)
print(all_significant_models)

# Diagnostic Checking
run_diagnostic_tests <- function(model, model_name) {
  results <- data.frame(Model = character(), Diagnostic = character(), P_Value = numeric(), Result = character(), stringsAsFactors = FALSE)
  
  ## No-Autocorrelation Residuals Test
  test <- Box.test(model$residuals, type = "Ljung")
  p_value <- round(test$p.value, 3)
  result <- ifelse(p_value > 0.05, "No Autocorrelation", "Autocorrelated")
  results <- rbind(results, data.frame(Model = model_name, Diagnostic = "Autocorrelation", P_Value = p_value, Result = result))
  
  ## Homoscedasticity Test
  test <- Box.test((model$residuals)^2, type = "Ljung")
  p_value <- round(test$p.value, 3)
  result <- ifelse(p_value > 0.05, "Homoscedastic", "Heteroscedastic")
  results <- rbind(results, data.frame(Model = model_name, Diagnostic = "Homoscedasticity", P_Value = p_value, Result = result))
  
  ## Normality Test
  test <- jarque.bera.test(model$residuals)
  p_value <- round(test$p.value, 3)
  result <- ifelse(p_value > 0.05, "Normally Distributed", "Not Normally Distributed")
  results <- rbind(results, data.frame(Model = model_name, Diagnostic = "Normality Residuals", P_Value = p_value, Result = result))
  
  return(results)
}

all_diagnostics <- data.frame(Model = character(), Diagnostic = character(), P_Value = numeric(), Result = character(), stringsAsFactors = FALSE)
for (model_name in all_significant_models$Model) {
  model <- model_list[[model_name]]
  diagnostics <- run_diagnostic_tests(model, model_name)
  all_diagnostics <- rbind(all_diagnostics, diagnostics)
}
print(all_diagnostics)

# Model Selection by Log-Likelihood, AIC, and BIC
model_selection <- data.frame(Model = character(), LogLik = numeric(), AIC = numeric(), BIC = numeric(), stringsAsFactors = FALSE)
for (model_name in all_significant_models$Model) {
  model <- model_list[[model_name]]
  model_info <- data.frame(Model = model_name, 
                           LogLik = logLik(model), 
                           AIC = AIC(model), 
                           BIC = BIC(model))
  model_selection <- rbind(model_selection, model_info)
}
print(model_selection)

# GET THE BEST ORDE IS ARIMA (nc011)


# FORECASTING NIGERIA DATA

# Indentify Seasonality
ggseasonplot(data_nigeria_ts, year.labels = TRUE, year.labels.left = TRUE,    year.labels.right = TRUE, xlab("Year"), ylab("Kilograms per year"), main="Seasonal plot of Pig Meat Consumption in Nigeria")

# Check stasionary data with ADF test
adf.test(data_nigeria_ts)

# Differencing ndiffs
ndiffs(data_nigeria_ts)

# Differencing
data_nigeria_diff <- diff(data_nigeria_ts)
adf.test(data_nigeria_diff)
#Result: p-value = 0.04 < 0.05, reject null hypothesis, data is stasionary
autoplot(data_nigeria_diff)

# Differencing with loga
data_nigeria_diff_log <- diff(log(data_nigeria_ts))
adf.test(data_nigeria_diff_log)

# Only one differencing with log can make data stasionary, so d = 1

# Check ACF and PACF
ggAcf(data_nigeria_diff_log, lag.max = 12) + ggtitle("ACF of Pig Meat Consumption in Nigeria (d = 1)")

ggPacf(data_nigeria_diff_log, lag.max = 12) + ggtitle("PACF of Pig Meat Consumption in Nigeria (d = 1)")

# Identified model is ARIMA(1,1,2)

# Model Underfitting
## The underfitting would be done using for-loop:
### Function to create model name
create_model_name <- function(p, d, q, constant) {
  paste0(ifelse(constant, "c", "nc"), p, d, q)
}
results <- data.frame(Model = character(), Coef = character(), P_Value = numeric(), Conclusion = character(), stringsAsFactors = FALSE)
model_list <- list()
max_p <- 1
max_d <- 1
max_q <- 2

for (p in 0:max_p) {
  for (d in 1:max_d) {
    for (q in 0:max_q) {
      if (p == 0 && q == 0) next
      for (constant in c(TRUE, FALSE)) {
        model_name <- create_model_name(p, d, q, constant)
        model <- Arima(dt, order = c(p, d, q), include.constant = constant, lambda = 0)
        test_results <- coeftest(model)
        significant <- TRUE
        for (i in 1:nrow(test_results)) {
          coef_name <- rownames(test_results)[i]
          p_value <- round(test_results[i, "Pr(>|z|)"], 3)
          conclusion <- ifelse(p_value < 0.05, "Significant", "Not Significant")
          results <- rbind(results, data.frame(Model = model_name, Coef = coef_name, P_Value = p_value, Conclusion = conclusion, stringsAsFactors = FALSE))
          if (conclusion == "Not Significant") {
            significant <- FALSE
          }
        }
        if (significant) {
          model_list[[model_name]] <- model
        }
      }
    }
  }
}
print(results)

### Detect all significant models
all_significant_models <- data.frame(Model = names(model_list), stringsAsFactors = FALSE)
print(all_significant_models)

# Diagnostic Checking
run_diagnostic_tests <- function(model, model_name) {
  results <- data.frame(Model = character(), Diagnostic = character(), P_Value = numeric(), Result = character(), stringsAsFactors = FALSE)
  
  ## No-Autocorrelation Residuals Test
  test <- Box.test(model$residuals, type = "Ljung")
  p_value <- round(test$p.value, 3)
  result <- ifelse(p_value > 0.05, "No Autocorrelation", "Autocorrelated")
  results <- rbind(results, data.frame(Model = model_name, Diagnostic = "Autocorrelation", P_Value = p_value, Result = result))
  
  ## Homoscedasticity Test
  test <- Box.test((model$residuals)^2, type = "Ljung")
  p_value <- round(test$p.value, 3)
  result <- ifelse(p_value > 0.05, "Homoscedastic", "Heteroscedastic")
  results <- rbind(results, data.frame(Model = model_name, Diagnostic = "Homoscedasticity", P_Value = p_value, Result = result))
  
  ## Normality Test
  test <- jarque.bera.test(model$residuals)
  p_value <- round(test$p.value, 3)
  result <- ifelse(p_value > 0.05, "Normally Distributed", "Not Normally Distributed")
  results <- rbind(results, data.frame(Model = model_name, Diagnostic = "Normality Residuals", P_Value = p_value, Result = result))
  
  return(results)
}

all_diagnostics <- data.frame(Model = character(), Diagnostic = character(), P_Value = numeric(), Result = character(), stringsAsFactors = FALSE)
for (model_name in all_significant_models$Model) {
  model <- model_list[[model_name]]
  diagnostics <- run_diagnostic_tests(model, model_name)
  all_diagnostics <- rbind(all_diagnostics, diagnostics)
}
print(all_diagnostics) #nc011

# Model Selection by Log-Likelihood, AIC, and BIC
model_selection <- data.frame(Model = character(), LogLik = numeric(), AIC = numeric(), BIC = numeric(), stringsAsFactors = FALSE)
for (model_name in all_significant_models$Model) {
  model <- model_list[[model_name]]
  model_info <- data.frame(Model = model_name, 
                           LogLik = logLik(model), 
                           AIC = AIC(model), 
                           BIC = BIC(model))
  model_selection <- rbind(model_selection, model_info)
}
print(model_selection)

# GET THE BEST ORDE IS ARIMA (c112)

# FORECASTING NIGERIA DATA WITH GGPLOT2
model_nigeria <- Arima(data_nigeria_ts, order = c(1, 1, 2), include.constant = TRUE, lambda = 0)
forecast_nigeria <- forecast(model_nigeria, h = 10)
autoplot(forecast_nigeria) + autolayer(data_nigeria_ts, series = "Data") + xlab("Year") + ylab("Kilograms per year") + ggtitle("Forecasting Pig Meat Consumption in Nigeria")
accuracy(forecast_nigeria) 


data<-data_nigeria_ts
# Make dataframe for RMSE dan MSE

# FORECASTING DENGAN DOUBLE MOVING AVERAGE
k=5 #mendefinisikan nilai k
n=length(data)
MA=array(NA,dim=c(n)) # inisiasi MA

# perulangan untuk forecast rata-rata bergerak tunggal
for(i in 1:n){
  MA[i+(k-1)]=mean(data[i:(i+(k-1))])
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
  e[i]=(data[i+(m-1)+(k-1)+2]-Prediksi[i+(m-1)+(k-1)+2])^2
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

# Plot the forecast data nigeria bersandingan dengan hasil
plot(data, type="l", col="red",lwd=2,
     xlim=c(1961,2031),xlab="Tahun", ylab="Demand Indonesia", main="Plot
Data Asli dan Ramalan MA(3x5)")
lines(Prediksi, col="blue", lwd=2)
limitDate=end(Data2)[1]+(end(Data2)[2]-1)/frequency(Data2)
abline(v=limitDate ,lty=4)
lines(Ramalan,col="green", lwd=2)
legend("topleft", c("Asli", "Prediksi", "Ramalan"), bty="n",
       lwd=2, col=c("red", "blue","green"))


data<-data_indonesia_ts

# DOUBLE MOVING AVERAGE
k=5 #mendefinisikan nilai k
n=length(data)
MA=array(NA,dim=c(n)) # inisiasi MA

# perulangan untuk forecast rata-rata bergerak tunggal
for(i in 1:n){
  MA[i+(k-1)]=mean(data[i:(i+(k-1))])
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
  e[i]=(data[i+(m-1)+(k-1)+2]-Prediksi[i+(m-1)+(k-1)+2])^2
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


# DOUBLE EXPONENTIAL SMOOTHING
# Nigeria
data<-data_nigeria_ts
# membuat fungsi manual untuk peramalan 2 parameter holt dgn argumen y=data, Alpha, Gamma, dan start
holt_manual = function(y, Alpha, Gamma, start = y[1]){
  st_par = y
  st_par[1] = y[1] #inisiasi untuk s1
  bt_par = y
  bt_par[1] =  y[2] - y[1] #inisiasi untuk b1
  n = length(y)+1 #Panjang data forecast lebih satu dari data asli
  forecast = array(NA, dim=c(n)) #Vektor kosong
  n_2 = length(y) #Panjang data error, st, dan bt sama dengan panjang data asli
  error = array(NA, dim=c(n_2)) #Vektor kosong
  for (i in 2:length(y)){ #Menggunakan fungsi perulangan untuk st dengan rumus yang sama pada modul dengan t dimulai dari 2 sampai dengan nilai panjang data asli
    st_par[i] = Alpha*y[i] + (1-Alpha)*(st_par[i-1]+bt_par[i-1])
    for (i in 2:length(y)){ #Menggunakan fungsi perulangan untuk bt dengan rumus yang sama pada modul dengan t dimulai dari 2 sampai dengan nilai panjang data asli
      bt_par[i] = Gamma*(st_par[i]-st_par[i-1]) + (1-Gamma)*bt_par[i-1]
    }
  }
  for (i in 3:n){ #Menggunakan fungsi perulangan untuk forecasting dengan t dimulai dari 3 sampai n yaitu panjang data asli plus 1
    forecast[i] = bt_par[i-1] + st_par[i-1]
  }
  for (i in 3:n_2){ #Menggunakan fungsi perulangan untuk menghitung error^2 dengan t dimulai dari 3 sampai dengan nilai panjang data asli
    error[i] = (y[i] - forecast[i])^2
  }
  max_ln = max(c(length(y), length(st_par), length(forecast), length(bt_par), length(error))) #Mencari variabel dengan panjang data terpanjang
  df = data.frame(Data = c(y,rep(NA, max_ln - length(y))), #Menambahkan baris dengan nilai NA untuk menyamakan panjang data dengan panjang data terpanjang
                  St = c(st_par,rep(NA, max_ln - length(st_par))), # menghitung St sesuai formula yang ada
                  Bt = c(bt_par,rep(NA, max_ln - length(bt_par))), # menghitung Bt sesuai formula yang ada
                  Forecast = c(forecast,rep(NA, max_ln - length(forecast))), # menghitung nilai prediksi/peramalan
                  Error = c(error,rep(NA, max_ln - length(error)))) # menghitung nilai error
  df #Menampilkan hasil ramalan
}
Alpha = rep(seq(0.1,0.9, by = 0.1), times = 9) #Membuat vektor nilai alpha
Gamma = rep(seq(0.1,0.9, by = 0.1), each = 9) #Membuat vektor nilai gamma
RMSE = NA #Vektor kosong
for (i in seq_along(Alpha)){ #Fungsi perulangan untuk mencari RMSE dari kombinasi nilai alpha dan gamma
  for (i in seq_along(Gamma)){
    param = holt_manual(data, Alpha[i], Gamma[i])
    RMSE[i] = sqrt(mean(param$Error,na.rm=TRUE))
  }
}
Tabel.RMSE.Holt = data.frame(Alpha, Gamma, RMSE) #Membuat data frame yang memuat Alpha, Gamma, dan RMSE yg bersesuaian
Tabel.RMSE.Holt
Min.RMSE = Tabel.RMSE.Holt[which.min(Tabel.RMSE.Holt$RMSE),] #Mencari kombinasi nilai alpha dan gamma dengan RMSE terkecil
Min.RMSE

best.holt = holt_manual(data, 0.9, 0.1) #Kombinasi alpha dan gamma dengan RMSE terkecil
RMSE.best.holt = sqrt(mean(best.holt$Error,na.rm=TRUE))
# fungsi untuk menampilkan hasil forecast beserta Alpha, Gamma, dan RMSE paling optimal
result.holt = function(best.holt, RMSE.best.holt, Alpha, Gamma){
  print(best.holt)
  cat("\n", "-----------------\n", "RMSE = ", RMSE.best.holt,
      "\n", "Alpha = ", Alpha,
      "\n", "Gamma = ", Gamma)
}
result.holt(best.holt, RMSE.best.holt, 0.9, 0.1) #Hasil

# Indonesia
data<-data_indonesia_ts
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
    model <- holt_manual(data, Alpha = alpha, Gamma = gamma, h = 5)
    rmse <- calculate_rmse(model$Error)
    results <- rbind(results, data.frame(Alpha = alpha, Gamma = gamma, RMSE = rmse))
  }
}

# Menemukan kombinasi Alpha dan Gamma dengan RMSE terkecil
optimal_params <- results %>% filter(RMSE == min(RMSE))
optimal_params

# Menggunakan nilai Alpha dan Gamma yang optimal untuk membuat prediksi
best.holt <- holt_manual(data, Alpha = optimal_params$Alpha, Gamma = optimal_params$Gamma, h = 5)
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


# Indonesia
data<-data_indonesia_ts
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
    model <- holt_manual(data, Alpha = alpha, Gamma = gamma, h = 5)
    rmse <- calculate_rmse(model$Error)
    results <- rbind(results, data.frame(Alpha = alpha, Gamma = gamma, RMSE = rmse))
  }
}

# Menemukan kombinasi Alpha dan Gamma dengan RMSE terkecil
optimal_params <- results %>% filter(RMSE == min(RMSE))
optimal_params

# Menggunakan nilai Alpha dan Gamma yang optimal untuk membuat prediksi
best.holt <- holt_manual(data, Alpha = optimal_params$Alpha, Gamma = optimal_params$Gamma, h = 5)
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
plot(data.plot, type = "l", col = "blue", lwd = 2, xlab = "Tahun", ylab = "Demand Indonesia", main = "Plot Data Asli dan Ramalan 2 Parameter Holt")
lines(forecast.holt, col = "red", lwd = 2)
legend("topleft", c("Asli", "Ramalan"), bty = "n", lwd = 2, col = c("blue", "red"))


# Indonesia
data<-data_nigeria_ts
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
    model <- holt_manual(data, Alpha = alpha, Gamma = gamma, h = 5)
    rmse <- calculate_rmse(model$Error)
    results <- rbind(results, data.frame(Alpha = alpha, Gamma = gamma, RMSE = rmse))
  }
}

# Menemukan kombinasi Alpha dan Gamma dengan RMSE terkecil
optimal_params <- results %>% filter(RMSE == min(RMSE))
optimal_params

# Menggunakan nilai Alpha dan Gamma yang optimal untuk membuat prediksi
best.holt <- holt_manual(data, Alpha = optimal_params$Alpha, Gamma = optimal_params$Gamma, h = 5)
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
plot(data.plot, type = "l", col = "blue", lwd = 2, xlab = "Tahun", ylab = "Demand Nigeria", main = "Plot Data Asli dan Ramalan 2 Parameter Holt")
lines(forecast.holt, col = "red", lwd = 2)
legend("topleft", c("Asli", "Ramalan"), bty = "n", lwd = 2, col = c("blue", "red"))

