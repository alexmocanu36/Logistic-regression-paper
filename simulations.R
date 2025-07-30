getwd()

library(dplyr)
library(ggplot2)

setwd("C:\\Users\\alexandru\\Documents\\seturi_de_date")

forecast = read.csv(
  "C:\\Users\\alexandru\\Documents\\seturi_de_date\\weather_forecast_data.csv"
)

# Creeaza o coloana noua, RainTodayBinary, care este 1 daca în coloana Rain apare textul "rain" si 0 în rest
forecast$RainTodayBinary <- ifelse(forecast$Rain == "rain", 1, 0)

head(forecast)

# Scatterplot intre Cloud_Cover si variabila binara:
# – Cloud_Cover pe axa x
# – RainTodayBinary (0 sau 1) pe axa y
forecast %>% 
  ggplot() + 
  geom_point(aes(x = Cloud_Cover, y = RainTodayBinary))

# Fit-uim un model de regresie logistica (GLM cu link logit), unde explicativa este Cloud_Cover, iar raspunsul este RainTodayBinary: RainTodayBinary ~ Cloud_Cover
forecast_glm <- glm(
  RainTodayBinary ~ Cloud_Cover,
  data   = forecast,
  family = binomial(link = "logit")
)

# Construim un nou data-frame pentru a evalua modelul in valori ale Cloud_Cover de la 0 la 100, cu pas de 0.1
glm_predictions = data.frame(
  "Cloud_Cover" = seq(0, 100, by = 0.1)
)

# Prezicem log-odds 
glm_predictions$LogOdds = predict(
  forecast_glm,
  newdata = glm_predictions,
  type = "link"
)

# Prezicem probabilitatea  p = E[RainTodayBinary | Cloud_Cover]
glm_predictions$PredictedProb = predict(
  forecast_glm,
  newdata = glm_predictions,
  type = "response"
)

# Grafic al log-odds in functie de Cloud_Cover
glm_predictions %>%
  ggplot() +
  geom_line(aes(x = Cloud_Cover, y = LogOdds)) +
  labs(
    y = "Log-odds (logit)",
    title = "Evolutia log-odds pe masura cresterii Cloud_Cover"
  )

# Grafic al probabilitatii prezise versus observatiile reale:
glm_predictions %>%
  ggplot() +
  geom_line(aes(x = Cloud_Cover, y = PredictedProb), size = 1) +
  geom_point(
    data = forecast,
    aes(x = Cloud_Cover, y = RainTodayBinary),
    color = "blue",
    alpha = 0.6
  ) +
  labs(
    y = "Probabilitatea prezisa de ploaie",
    title = "Model logistic: probabilitate prezisa vs. date observate",
    caption = "Punctele albastre sunt datele reale (0 = fara ploaie, 1 = ploaie)"
  )

# Afiseaza rezumatul modelului glm:


summary(forecast_glm)