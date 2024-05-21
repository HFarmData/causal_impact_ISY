library(prophet)
library(dplyr)
library(readxl)


data <- read_excel("2024_ISP_TEMPLATE CAUSAL IMPACT_1605.xlsx", sheet = "Conti")
data <- data %>% 
  mutate(ds = Data,
         numero_conti = `Numero di conti sottoscritti`)

data("dt_prophet_holidays")

m <- prophet()
m <- add_country_holidays(m, country_name = 'IT')
m <- add_seasonality(m, name='daily', period)