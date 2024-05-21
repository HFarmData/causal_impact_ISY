#Loading Libraries
library(readxl)
library(CausalImpact)
library(dplyr)
library(zoo)
library(prophet)


# Loading Data
data <- read_excel("2024_ISP_TEMPLATE CAUSAL IMPACT_1605.xlsx", sheet = 'Conti')
str(data)
data <- data %>% 
  mutate(ds = Data,
         y = `Numero di conti sottoscritti`,
         ds = Data,
         cap = 2200,
         floor = 0)

# Extract trend from prophet
m <- prophet()
m <- add_country_holidays(m, country_name = 'IT')
m <- add_seasonality(m, name='daily', fourier.order = 14, period = 5)
m <- prophet(data, growth = 'linear', daily.seasonality = TRUE)
future <- make_future_dataframe(m, periods = 1)
forecast <- predict(m, future)
trend <- forecast$trend[-length(forecast$trend)]

# adding variables to the model (lagging)
data <- data %>%
  mutate(trend = trend)

time.points <- seq.Date(as.Date("2023-06-15"), as.Date("2024-06-15"), "days")
info <- data[c("y", "trend")]

df <- zoo(info, time.points)
head(df)

# plotting data
plot(index(df), df$y, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti')

lines(index(df), df$y, type = 'l', col = 'red')
lines(index(df), df$trend, type = 'l', col = 'green')

legend("topright", legend = c("Normal", "Trend"), 
       col = c("red", "green"), lty = 1)

# ISYSMART
pre.period <- as.Date(c("2023-06-15", "2023-07-01"))
post.period <- as.Date(c("2023-07-02", "2023-09-09"))
subdata <- subset(df, index(df) > as.Date("2023-06-15") & index(df) < as.Date("2023-09-09"))
impact_isysmart <- CausalImpact(subdata, pre.period, post.period)
plot(impact_isysmart, c('original'))
print(summary(impact_isysmart))
print(summary(impact_isysmart, "report"))

# ISYGIFT
pre.period <- as.Date(c("2023-06-15", "2023-09-09"))
post.period <- as.Date(c("2023-09-10", "2023-10-15"))
subdata <- subset(df, index(df) > as.Date("2023-06-15") & index(df) < as.Date("2023-10-15"))
impact_isygift <- CausalImpact(subdata, pre.period, post.period)
plot(impact_isygift, c('original'))
print(summary(impact_isygift))
print(summary(impact_isygift, "report"))

# ISYCASHBACK
pre.period <- as.Date(c("2023-06-15", "2023-11-15"))
post.period <- as.Date(c("2023-11-16", "2024-01-07"))
subdata <- subset(df, index(df) > as.Date("2023-06-15") & index(df) < as.Date("2024-01-07"))
impact_isycashback <- CausalImpact(subdata, pre.period, post.period)
plot(impact_isycashback, c('original'))
print(summary(impact_isycashback))
print(summary(impact_isycashback, "report"))

# MEMBER GET MEMBER
pre.period <- as.Date(c("2023-06-15", "2023-12-21"))
post.period <- as.Date(c("2023-12-22", "2024-03-13"))
subdata <- subset(df, index(df) > as.Date("2023-06-15") & index(df) < as.Date("2024-03-13"))
impact_isymgm <- CausalImpact(subdata, pre.period, post.period)
plot(impact_isymgm, c('original'))
print(summary(impact_isymgm))
print(summary(impact_isymgm, "report"))

# ISYGIFT2
pre.period <- as.Date(c("2023-06-15", "2024-04-29"))
post.period <- as.Date(c("2024-04-30", "2024-06-17"))
subdata <- subset(df, index(df) > as.Date("2023-06-15") & index(df) < as.Date("2024-06-17"))
impact_isygift2 <- CausalImpact(subdata, pre.period, post.period)
plot(impact_isygift2)
print(summary(impact_isygift2))
print(summary(impact_isygift2, "report"))
