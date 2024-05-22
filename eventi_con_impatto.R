library(prophet)
library(dplyr)
library(readxl)
library(writexl)

# 12/11/2023 - 19/11/2023	Nitto Atp Finals (DONE)
data <- read_excel('forecast/forecast_mgm.xlsx')
data$yhat <- data$yhat + (data$yhat * 0.1)
data <- data[c('ds','yhat')]
colnames(data)[2] <- 'y'
data_atp <- subset(data, data$ds >= "2023-06-15" & data$ds <= "2023-11-13")
m <- prophet(data_atp, 
             growth = 'linear', 
             daily.seasonality = FALSE,
             weekly.seasonality = 15,
             changepoint.prior.scale = 1, 
             holidays.prior.scale = 0.5,
             seasonality.prior.scale = 5,
             interval.width = 0.95,
             changepoint.range = 1,
             n.changepoints = 110,
             seasonality.mode = 'additive')
future_atp <- make_future_dataframe(m, periods = 14)
forecast_atp <- predict(m, future_atp)
prophet_plot_components(m, forecast_atp)

# plotting data
atp_true <- subset(data, data$ds >= "2023-06-15" & data$ds <= "2023-11-27")
plot(atp_true$ds, atp_true$y, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti', main = 'Atp Finals')

lines(atp_true$ds, atp_true$y, type = 'l', col = 'red')
lines(atp_true$ds, forecast_atp$yhat, type = 'l', col = 'green')

legend("topright", legend = c("True", "Prediction"), 
       col = c("red", "green"), lty = 1)
abline(v = atp_true$ds[151], col = "blue", lwd = 2, lty = 2)
abline(v = atp_true$ds[165], col = "blue", lwd = 2, lty = 2)

# 24/11/2023 - 26/11/2023	Milano Games Week (DONE)
data_mgw <- subset(data, data$ds >= "2023-06-15" & data$ds <= "2023-11-25")
m <- prophet(data_mgw, 
             growth = 'linear', 
             daily.seasonality = FALSE,
             weekly.seasonality = 15,
             changepoint.prior.scale = 1, 
             holidays.prior.scale = 0.5,
             seasonality.prior.scale = 5,
             interval.width = 0.95,
             changepoint.range = 1,
             n.changepoints = 110,
             seasonality.mode = 'additive')
future_mgw <- make_future_dataframe(m, periods = 8)
forecast_mgw <- predict(m, future_mgw)
prophet_plot_components(m, forecast_mgw)

# plotting data
mgw_true <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2023-12-03")
plot(mgw_true$ds, mgw_true$y, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti', main = 'Games Week')

lines(mgw_true$ds, mgw_true$y, type = 'l', col = 'red')
lines(mgw_true$ds, forecast_mgw$yhat, type = 'l', col = 'green')

legend("topright", legend = c("True", "Prediction"), 
       col = c("red", "green"), lty = 1)
abline(v = mgw_true$ds[163], col = "blue", lwd = 2, lty = 2)
abline(v = mgw_true$ds[171], col = "blue", lwd = 2, lty = 2)

# 04/04/2024 - 31/05/2024	Coppa eFotball (CHECK)
data <- read_excel('forecast/forecast_isygift2.xlsx')
data$yhat <- data$yhat + (data$yhat * 0.1)
data <- data[c('ds','yhat')]
colnames(data)[2] <- 'y'
data_ef <- subset(data, data$ds >= "2023-06-15" & data$ds <= "2024-04-05")
m <- prophet(data_ef, 
             growth = 'linear', 
             daily.seasonality = FALSE,
             weekly.seasonality = 15,
             changepoint.prior.scale = 0.5, 
             holidays.prior.scale = 0.5,
             seasonality.prior.scale = 0.1,
             interval.width = 0.95,
             changepoint.range = 0.8,
             n.changepoints = 150,
             seasonality.mode = 'multiplicative')
future_ef <- make_future_dataframe(m, periods = 41)
forecast_ef <- predict(m, future_ef)
prophet_plot_components(m, forecast_ef)

# plotting data
ef_true <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2024-05-31")
plot(ef_true$ds, ef_true$y, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti', main = 'Coppa Efootball')

lines(ef_true$ds, ef_true$y, type = 'l', col = 'red')
lines(ef_true$ds, forecast_ef$yhat, type = 'l', col = 'green')

legend("topright", legend = c("True", "Prediction"), 
       col = c("red", "green"), lty = 1)
abline(v = ef_true$ds[295], col = "blue", lwd = 2, lty = 2)
abline(v = ef_true$ds[336], col = "blue", lwd = 2, lty = 2)