library(prophet)
library(dplyr)
library(readxl)
library(writexl)


data_prophet <- read_excel("2024_ISP_TEMPLATE CAUSAL IMPACT_1605.xlsx", sheet = "Conti")
data_prophet <- data_prophet %>% 
  mutate(ds = Data,
         y = `Numero di conti sottoscritti`,
         ds = Data,
         cap = 2200,
         floor = 0)

data_prophet <- data_prophet[c(3,4,5,6)]

# ISYSMART (DONE) - 01/07/203 - 30/09/2023
subdata_isysmart <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2023-07-02")
m <- prophet(subdata_isysmart, 
             growth = 'linear', 
             daily.seasonality = 6,
             weekly.seasonality = 10,
             changepoint.prior.scale = 0.3, 
             holidays.prior.scale = 0.8,
             seasonality.prior.scale = 0.1,
             interval.width = 0.95,
             changepoint.range = 1,
             n.changepoints = 10,
             seasonality.mode = 'additive', fit = FALSE)
m <- add_country_holidays(m, country_name = 'IT')
m <- fit.prophet(m, subdata_isysmart)
future_isysmart <- make_future_dataframe(m, periods = 70)
forecast_isysmart <- predict(m, future_isysmart)

# plotting data
isysmart_true <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2023-09-10")
plot(isysmart_true$ds, isysmart_true$y, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti', main = 'IsySmart')

lines(isysmart_true$ds, isysmart_true$y, type = 'l', col = 'red')
lines(isysmart_true$ds, forecast_isysmart$yhat, type = 'l', col = 'green')

legend("topright", legend = c("True", "Prediction"), 
       col = c("red", "green"), lty = 1)
abline(v = isysmart_true$ds[18], col = "blue", lwd = 2, lty = 2)
abline(v = isysmart_true$ds[87], col = "blue", lwd = 2, lty = 2)

write_xlsx(forecast_isysmart, 'forecast/forecast_isysmart.xlsx')

# ISYGIFT (DONE) - 09/09/2023 - 15/10/2023
subdata_isygift <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2023-09-10")
m <- prophet(subdata_isygift, 
             growth = 'linear', 
             daily.seasonality = TRUE,
             weekly.seasonality = 15,
             yearly.seasonality = FALSE,
             changepoint.prior.scale = 0.5, 
             holidays.prior.scale = 0.1,
             seasonality.prior.scale = 0.9,
             interval.width = 0.95,
             changepoint.range = 1,
             n.changepoints = 10,
             seasonality.mode = 'additive', fit = FALSE)
m <- add_country_holidays(m, country_name = 'IT')
m <- add_seasonality(m, name='monthly', fourier.order = 2, period = 30.5, prior.scale = 0.5)
m <- fit.prophet(m, subdata_isygift)
future_isygift <- make_future_dataframe(m, periods = 36)
forecast_isygift <- predict(m, future_isygift)

# plotting data
isygift_true <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2023-10-16")
plot(isygift_true$ds, isygift_true$y, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti', main = 'IsyGift')

lines(isygift_true$ds, isygift_true$y, type = 'l', col = 'red')
lines(isygift_true$ds, forecast_isygift$yhat, type = 'l', col = 'green')

legend("topright", legend = c("True", "Prediction"), 
       col = c("red", "green"), lty = 1)
abline(v = isygift_true$ds[87], col = "blue", lwd = 2, lty = 2)
abline(v = isygift_true$ds[123], col = "blue", lwd = 2, lty = 2)

write_xlsx(forecast_isygift, 'forecast/forecast_isygift.xlsx')

# ISYCASHBACK (DONE) - 15/11/2023 - 07/01/2024
subdata_isycashback <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2023-11-17")
m <- prophet(subdata_isycashback, 
             growth = 'linear', 
             daily.seasonality = TRUE,
             weekly.seasonality = 15,
             yearly.seasonality = FALSE,
             changepoint.prior.scale = 2.5, 
             holidays.prior.scale = 0.1,
             seasonality.prior.scale = 1,
             interval.width = 0.95,
             changepoint.range = 1,
             n.changepoints = 110,
             seasonality.mode = 'additive', fit = FALSE)
m <- add_country_holidays(m, country_name = 'IT')
m <- add_seasonality(m, name='monthly', fourier.order = 2, period = 30.5, prior.scale = 0.5)
m <- fit.prophet(m, subdata_isycashback)
future_isycashback <- make_future_dataframe(m, periods = 52)
forecast_isycashback <- predict(m, future_isycashback)

# plotting data
isycashback_true <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2024-01-08")
plot(isycashback_true$ds, isycashback_true$y, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti', main = 'IsyCashBack')

lines(isycashback_true$ds, isycashback_true$y, type = 'l', col = 'red')
lines(isycashback_true$ds, forecast_isycashback$yhat, type = 'l', col = 'green')

legend("topright", legend = c("True", "Prediction"), 
       col = c("red", "green"), lty = 1)
abline(v = isycashback_true$ds[155], col = "blue", lwd = 2, lty = 2)
abline(v = isycashback_true$ds[207], col = "blue", lwd = 2, lty = 2)

write_xlsx(forecast_isycashback, 'forecast/forecast_isycashback.xlsx')

# MEMBER GET MEMBER (DONE) - 21/12/2023 - 13/03/2024
pre.period <- as.Date(c("2023-06-15", "2023-12-21"))
post.period <- as.Date(c("2023-12-22", "2024-03-13"))
subdata_mgm <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2023-12-22")
m <- prophet(subdata_mgm, 
             growth = 'linear', 
             daily.seasonality = TRUE,
             weekly.seasonality = 15,
             yearly.seasonality = FALSE,
             changepoint.prior.scale = 0.5, 
             holidays.prior.scale = 0.1,
             seasonality.prior.scale = 0.1,
             interval.width = 0.95,
             changepoint.range = 0.9,
             n.changepoints = 20,
             seasonality.mode = 'additive', fit = FALSE)
m <- add_country_holidays(m, country_name = 'IT')
m <- add_seasonality(m, name='monthly', fourier.order = 1, period = 30.5, prior.scale = 0.5)
m <- fit.prophet(m, subdata_mgm)
future_mgm <- make_future_dataframe(m, periods = 83)
forecast_mgm <- predict(m, future_mgm)

# plotting data
isymgm_true <- subset(data_prophet, data_prophet$ds >= "2023-06-15" & data_prophet$ds <= "2024-03-14")
plot(isymgm_true$ds, isymgm_true$y, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti', main = 'IsyMGM')

lines(isymgm_true$ds, isymgm_true$y, type = 'l', col = 'red')
lines(isymgm_true$ds, forecast_mgm$yhat, type = 'l', col = 'green')

legend("topright", legend = c("True", "Prediction"), 
       col = c("red", "green"), lty = 1)
abline(v = isymgm_true$ds[190], col = "blue", lwd = 2, lty = 2)
abline(v = isymgm_true$ds[272], col = "blue", lwd = 2, lty = 2)

write_xlsx(forecast_mgm, 'forecast/forecast_mgm.xlsx')

# ISYGIFT2 (DONE) - 29/04/2024 - 17/06/2024
subdata_isygift2 <- forecast_isygift[c('ds', 'yhat')]
colnames(subdata_isygift2)[2] <- 'y'
subdata_isygift2 <- subdata_isygift2 %>%
  mutate(cap = rep(2200, nrow(forecast_isygift)),
         floor = rep(0, nrow(forecast_isygift)))
temp <- subset(data_prophet, data_prophet$ds > "2023-10-16" & data_prophet$ds <= "2024-04-30")
final <- rbind(subdata_isygift2, temp)
m <- prophet(final, 
             growth = 'linear', 
             daily.seasonality = 2,
             weekly.seasonality = 1,
             yearly.seasonality = FALSE,
             changepoint.prior.scale = 2, 
             holidays.prior.scale = 0.1,
             seasonality.prior.scale = 0.1,
             interval.width = 0.95,
             changepoint.range = 1,
             n.changepoints = 110,
             seasonality.mode = 'multiplicative', fit = FALSE)
m <- add_country_holidays(m, country_name = 'IT')
m <- add_seasonality(m, name='monthly', fourier.order = 1, period = 30.5, prior.scale = 1)
m <- fit.prophet(m, final)
future_isygift2 <- make_future_dataframe(m, periods = 16)
forecast_isygift2 <- predict(m, future_isygift2)

# plotting data
subset.temp <- subset(data_prophet, data_prophet$ds > "2024-04-30")
isygift2_true <- rbind(final, subset.temp)
plot(isygift2_true$ds, isygift2_true$y, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti', main = 'IsyGift2')
abline(v=as.Date("2024-04-30"), col='blue')

lines(isygift2_true$ds, isygift2_true$y, type = 'l', col = 'red')
lines(isygift2_true$ds, forecast_isygift2$yhat, type = 'l', col = 'green')

legend("topright", legend = c("True", "Prediction"), 
       col = c("red", "green"), lty = 1)
abline(v = isygift2_true$ds[321], col = "blue", lwd = 2, lty = 2)
abline(v = isygift2_true$ds[336], col = "blue", lwd = 2, lty = 2)

write_xlsx(forecast_isygift2, 'forecast/forecast_isygift2.xlsx')