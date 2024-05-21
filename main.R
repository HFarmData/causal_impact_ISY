#Loading Libraries
library(readxl)
library(CausalImpact)
library(dplyr)
library(zoo)


# Loading Data
data <- read_excel("2024_ISP_TEMPLATE CAUSAL IMPACT_1605.xlsx", sheet = 'Conti')
str(data)
data <- rename(data, numero_conti = `Numero di conti sottoscritti`)

data_ts <- zoo(data$numero_conti, order.by = data$Data)
trend <- rollmean(data_ts, k = 5, fill = 0)

# adding variables to the model (lagging)
data <- data %>%
  mutate(trend = trend)

time.points <- seq.Date(as.Date("2023-06-15"), as.Date("2024-06-15"), "days")
info <- data[c("numero_conti", "trend")]

df <- zoo(info, time.points)
head(df)

# plotting data
plot(data$Data, data$numero_conti, type = 'l', xlab = 'Data', ylab = 'Numero conti aperti')

lines(data$Data, data$numero_conti, type = 'l', col = 'red')
lines(data$Data, data$trend, type = 'l', col = 'green')

legend("topright", legend = c("Normal", "Trend"), 
       col = c("red", "green"), lty = 1)

# ISYSMART
pre.period <- as.Date(c("2023-06-15", "2023-07-01"))
post.period <- as.Date(c("2023-07-02", "2023-09-09"))
impact_isysmart <- CausalImpact(df, pre.period, post.period)
plot(impact_isysmart)
print(summary(impact_isysmart))
print(summary(impact_isysmart, "report"))

# ISYGIFT
pre.period <- as.Date(c("2023-06-15", "2023-09-09"))
post.period <- as.Date(c("2023-09-10", "2023-10-15"))
impact_isygift <- CausalImpact(df, pre.period, post.period)
plot(impact_isygift)
print(summary(impact_isygift))
print(summary(impact_isygift, "report"))

# ISYCASHBACK
pre.period <- as.Date(c("2023-06-15", "2023-11-15"))
post.period <- as.Date(c("2023-11-16", "2024-01-07"))
impact_isycashback <- CausalImpact(df, pre.period, post.period)
plot(impact_isycashback)
print(summary(impact_isycashback))
print(summary(impact_isycashback, "report"))

# MEMBER GET MEMBER
pre.period <- as.Date(c("2023-06-15", "2023-12-21"))
post.period <- as.Date(c("2023-12-22", "2024-03-13"))
impact_isymgm <- CausalImpact(df, pre.period, post.period)
plot(impact_isymgm)
print(summary(impact_isymgm))
print(summary(impact_isymgm, "report"))

# ISYGIFT2
pre.period <- as.Date(c("2023-06-15", "2024-04-29"))
post.period <- as.Date(c("2024-04-30", "2024-06-17"))
impact_isygift2 <- CausalImpact(df, pre.period, post.period)
plot(impact_isygift2)
print(summary(impact_isygift2))
print(summary(impact_isygift2, "report"))
