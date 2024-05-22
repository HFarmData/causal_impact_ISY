library(prophet)
library(dplyr)
library(readxl)


data_prophet <- read_excel("2024_ISP_TEMPLATE CAUSAL IMPACT_1605.xlsx", sheet = "Conti")
data_prophet <- data_prophet %>% 
  mutate(ds = Data,
         y = `Numero di conti sottoscritti`,
         ds = Data,
         cap = 2200,
         floor = 0)

data_prophet <- data_prophet[c(3,4,5,6)]

# 09/09/2023 - 10/09/2023	PlugMi (DONE)
subdata_plugmi <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2023-09-10")
m <- prophet(growth = 'linear', 
             daily.seasonality = FALSE,
             weekly.seasonality = 2,
             changepoint.prior.scale = 0.9, 
             holidays.prior.scale = 0.1,
             seasonality.prior.scale = 2,
             interval.width = 0.95,
             changepoint.range = 1,
             n.changepoints = 150,
             seasonality.mode = 'additive', fit = FALSE)
m <- add_country_holidays(m, country_name = 'IT')
m <- add_seasonality(m, name='monthly', fourier.order = 5, period = 30.5, prior.scale = 0.5)
m <- fit.prophet(m, subdata_plugmi)
future_plugmi <- make_future_dataframe(m, periods = 8)
forecast_plugmi <- predict(m, future_plugmi)

# plotting data
plugmi_true <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2023-09-18")
plot(plugmi_true$ds, plugmi_true$y, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti', main = 'Plugmi')

lines(plugmi_true$ds, plugmi_true$y, type = 'l', col = 'red')
lines(plugmi_true$ds, forecast_plugmi$yhat, type = 'l', col = 'green')

legend("topright", legend = c("True", "Prediction"), 
       col = c("red", "green"), lty = 1)
abline(v = plugmi_true$ds[87], col = "blue", lwd = 2, lty = 2)
abline(v = plugmi_true$ds[95], col = "blue", lwd = 2, lty = 2)

# 01/11/2023 - 05/11/2023	Lucca Comics (DONE)
subdata_lucca <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2023-11-02")
m <- prophet(subdata_lucca, 
             growth = 'linear', 
             daily.seasonality = FALSE,
             weekly.seasonality = 15,
             changepoint.prior.scale = 1, 
             holidays.prior.scale = 0.5,
             seasonality.prior.scale = 5,
             interval.width = 0.95,
             changepoint.range = 0.95,
             n.changepoints = 150,
             seasonality.mode = 'multiplicative', fit = FALSE)
m <- add_country_holidays(m, country_name = 'IT')
m <- add_seasonality(m, name='monthly', fourier.order = 5, period = 30.5, prior.scale = 0.5)
m <- fit.prophet(m, subdata_lucca)
future_lucca <- make_future_dataframe(m, periods = 11)
forecast_lucca <- predict(m, future_lucca)

# plotting data
lucca_true <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2023-11-13")
plot(lucca_true$ds, lucca_true$y, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti', main = 'Lucca Comics')

lines(lucca_true$ds, lucca_true$y, type = 'l', col = 'red')
lines(lucca_true$ds, forecast_lucca$yhat, type = 'l', col = 'green')

legend("topright", legend = c("True", "Prediction"), 
       col = c("red", "green"), lty = 1)
abline(v = lucca_true$ds[140], col = "blue", lwd = 2, lty = 2)
abline(v = lucca_true$ds[151], col = "blue", lwd = 2, lty = 2)

# 12/11/2023 - 19/11/2023	Nitto Atp Finals (DONE)
subdata_atp <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2023-11-13")
m <- prophet(subdata_atp, 
             growth = 'linear', 
             daily.seasonality = FALSE,
             weekly.seasonality = 15,
             changepoint.prior.scale = 1.5, 
             holidays.prior.scale = 0.5,
             seasonality.prior.scale = 5,
             interval.width = 0.95,
             changepoint.range = 1,
             n.changepoints = 110,
             seasonality.mode = 'additive', fit = FALSE)
m <- add_country_holidays(m, country_name = 'IT')
m <- add_seasonality(m, name='monthly', fourier.order = 2, period = 30.5, prior.scale = 0.5)
m <- fit.prophet(m, subdata_atp)
future_atp <- make_future_dataframe(m, periods = 14)
forecast_atp <- predict(m, future_atp)

# plotting data
atp_true <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2023-11-27")
plot(atp_true$ds, atp_true$y, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti', main = 'Atp Finals')

lines(atp_true$ds, atp_true$y, type = 'l', col = 'red')
lines(atp_true$ds, forecast_atp$yhat, type = 'l', col = 'green')

legend("topright", legend = c("True", "Prediction"), 
       col = c("red", "green"), lty = 1)
abline(v = atp_true$ds[151], col = "blue", lwd = 2, lty = 2)
abline(v = atp_true$ds[165], col = "blue", lwd = 2, lty = 2)

# 24/11/2023 - 26/11/2023	Milano Games Week (DONE)
subdata_mgw <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2023-11-25")
m <- prophet(subdata_mgw, 
             growth = 'linear', 
             daily.seasonality = FALSE,
             weekly.seasonality = 15,
             changepoint.prior.scale = 1, 
             holidays.prior.scale = 0.5,
             seasonality.prior.scale = 5,
             interval.width = 0.95,
             changepoint.range = 1,
             n.changepoints = 110,
             seasonality.mode = 'additive', fit = FALSE)
m <- add_country_holidays(m, country_name = 'IT')
m <- add_seasonality(m, name='monthly', fourier.order = 2, period = 30.5, prior.scale = 0.5)
m <- fit.prophet(m, subdata_mgw)
future_mgw <- make_future_dataframe(m, periods = 8)
forecast_mgw <- predict(m, future_mgw)

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
subdata_ef <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2024-04-05")
m <- prophet(subdata_ef, 
             growth = 'linear', 
             daily.seasonality = FALSE,
             weekly.seasonality = 15,
             changepoint.prior.scale = 0.5, 
             holidays.prior.scale = 0.5,
             seasonality.prior.scale = 0.1,
             interval.width = 0.95,
             changepoint.range = 0.8,
             n.changepoints = 150,
             seasonality.mode = 'multiplicative', fit = FALSE)
m <- add_country_holidays(m, country_name = 'IT')
m <- add_seasonality(m, name='monthly', fourier.order = 5, period = 30.5, prior.scale = 0.5)
m <- fit.prophet(m, subdata_ef)
future_ef <- make_future_dataframe(m, periods = 41)
forecast_ef <- predict(m, future_ef)

# plotting data
ef_true <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2024-05-31")
plot(ef_true$ds, ef_true$y, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti', main = 'Coppa Efootball')

lines(ef_true$ds, ef_true$y, type = 'l', col = 'red')
lines(ef_true$ds, forecast_ef$yhat, type = 'l', col = 'green')

legend("topright", legend = c("True", "Prediction"), 
       col = c("red", "green"), lty = 1)
abline(v = ef_true$ds[295], col = "blue", lwd = 2, lty = 2)
abline(v = ef_true$ds[336], col = "blue", lwd = 2, lty = 2)

# 15/05/2024	Concerto Radio Italia (CHECK DATA)
subdata_radioita <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2024-05-15")
m <- prophet(subdata_radioita, 
             growth = 'linear', 
             daily.seasonality = FALSE,
             weekly.seasonality = 15,
             changepoint.prior.scale = 0.5, 
             holidays.prior.scale = 0.5,
             seasonality.prior.scale = 0.1,
             interval.width = 0.95,
             changepoint.range = 0.8,
             n.changepoints = 150,
             seasonality.mode = 'multiplicative', fit = FALSE)
m <- add_country_holidays(m, country_name = 'IT')
m <- add_seasonality(m, name='monthly', fourier.order = 5, period = 30.5, prior.scale = 0.5)
m <- fit.prophet(m, subdata_radioita)
future_readioita <- make_future_dataframe(m, periods = 1)
forecast_radioita <- predict(m, future_readioita)

# plotting data
radioita_true <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2024-05-17")
plot(radioita_true$ds, radioita_true$y, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti', main = 'Radio Italia')

lines(radioita_true$ds, radioita_true$y, type = 'l', col = 'red')
lines(radioita_true$ds, forecast_radioita$yhat, type = 'l', col = 'green')

legend("topright", legend = c("True", "Prediction"), 
       col = c("red", "green"), lty = 1)
abline(v = radioita_true$ds[336], col = "blue", lwd = 2, lty = 2)
