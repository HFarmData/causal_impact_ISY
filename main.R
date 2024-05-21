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
impact_isysmart <- CausalImpact(df, pre.period, post.period, model.args=list(niter=10000))
plot(impact_isysmart, c('original'))
print(summary(impact_isysmart))
print(summary(impact_isysmart, "report"))

# ISYGIFT
subset_pre <- subset(data, Data <= "2023-09-09")
nrow_pre <- nrow(subset_pre)
subset_post <- subset(data, Data > "2023-09-09" & Data <= "2023-10-22")
nrow_post <- nrow(subset_post)
subdata <- subset(data, Data <= "2023-10-22")
subdata_final <- subdata[,c(2,3)]
impact_isygift <- CausalImpact(subdata_final, c(1, nrow_pre), c(nrow_pre+1, nrow(subdata_final)))
plot(impact_isygift)
print(summary(impact_isygift))
print(summary(impact_isygift, "report"))

# ISYCASHBACK
subset_pre <- subset(data, Data <= "2023-11-15")
nrow_pre <- nrow(subset_pre)
subset_post <- subset(data, Data > "2023-11-15" & Data <= "2024-01-14")
nrow_post <- nrow(subset_post)
subdata <- subset(data, Data <= "2024-01-14")
subdata_final <- subdata[,c(2,3)]
impact_isycashback <- CausalImpact(subdata_final, c(1, nrow_pre), c(nrow_pre+1, nrow(subdata_final)))
plot(impact_isycashback)
print(summary(impact_isycashback))
print(summary(impact_isycashback, "report"))

# MEMBER GET MEMBER
subset_pre <- subset(data, Data <= "2023-12-21")
nrow_pre <- nrow(subset_pre)
subset_post <- subset(data, Data > "2023-12-21" & Data <= "2024-03-20")
nrow_post <- nrow(subset_post)
subdata <- subset(data, Data <= "2024-03-20")
subdata_final <- subdata[,c(2,3)]
impact_isymgm <- CausalImpact(subdata_final, c(1, nrow_pre), c(nrow_pre+1, nrow(subdata_final)))
plot(impact_isymgm)
print(summary(impact_isymgm))
print(summary(impact_isymgm, "report"))

# ISYGIFT2
subset_pre <- subset(data, Data <= "2024-04-29")
nrow_pre <- nrow(subset_pre)
subset_post <- subset(data, Data > "2023-04-29" & Data <= "2024-05-15")
nrow_post <- nrow(subset_post)
subdata <- subset(data, Data <= "2024-05-15")
subdata_final <- subdata[,c(2,3)]
impact_isygift2 <- CausalImpact(subdata_final, c(1, nrow_pre), c(nrow_pre+1, nrow(subdata_final)))
plot(impact_isygift2)
print(summary(impact_isygift2))
print(summary(impact_isygift2, "report"))