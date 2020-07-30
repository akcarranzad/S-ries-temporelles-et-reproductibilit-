library("tidyverse")
library("lubridate")
library("forecast")
library("fpp2")

## lecture des données

hawai <- read_csv("hawai.csv")
glimpse(hawai)
CO2_hawai <- hawai %>%
  mutate(time = date_deci2mal(hawai$time, tz = "UTC"))
glimpse(CO2_hawai)
CO2_hawai %>% pull(time) %>% class()

CO2_hawai %>%
  ggplot(aes(x = time, y = CO2)) +
  geom_line()
 
##Création de la série temporelle

hawai_ts <- ts(CO2_hawai %>% dplyr::select(-time),
             start = c(1958, 3), frequency = 12)


## séparer la série en parties d'entraînement 

hawai_train <- window(hawai_ts, start = c(1958, 3), end = c(1988, 12))
hawai_test <- window(hawai_ts, start = c(1989, 1), end = c(2001, 12))

## créer un modèle prévisionnel sur les données d'entraînement

hawai_ets <- hawai_train %>% ets()
hawai_ets

autoplot(hawai_ets)

## puis projeter la prévision de CO2 atmosphérique pour comparer aux données test

hawaifc <- hawai_ets %>% forecast(h = length(hawai_test))
autoplot(hawaifc) + autolayer(hawai_test, color = "green") 

accuracy(hawaifc, hawai_ts)

## effectuer une analyse des résidus

checkresiduals(hawai_ets)
shapiro.test(residuals(hawai_ets))

## Box-Cox

hawai_BC <- hawai_train %>% ets(lambda = BoxCox.lambda(hawai_train))

hawaifc_BC <- hawai_BC %>%
  forecast(h = length(hawai_test))

autoplot(hawaifc_BC) + autolayer(hawai_test, color = "green") 

accuracy(hawaifc_BC, hawai_ts)

checkresiduals(hawai_BC)
shapiro.test(residuals(hawai_BC))
