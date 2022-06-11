
load("data/ts.RData")
library(tidyverse)
ts_df <- ts
ts_wide <- ts_df %>% filter(keyword == "National_Identity") %>%
  pivot_wider(id_cols = date,
              names_from = keyword,
              values_from = hits)
colnames(ts_wide)
ts <- ts_wide
ts$date <- as.Date(ts$date)
ts <- ts(data = ts[-1], start = c(2004, 1),
         end = c(2022, 6), frequency = 12)

library(forecast)
library(fpp2)

autoplot(stl(ts[,1], s.window = "periodic")) + 
  labs(title = "Global public interest in national identity over time",
       subtitle = "decomposed univariate time series with repeated LOESS smoothing",
       caption = "Google offers topics - a group of already clustered search terms that share the same concept or entity.
       \n These topics are language agnostic & account for spelling variations & mistakes.
       \nsee https://blog.google/products/search/15-tips-getting-most-out-google-trends/") +
  theme_bw()

ts_mod_auto_arima <- stlm(ts, s.window = "periodic", modelfunction = auto.arima)
checkresiduals(ts_mod_auto_arima)

ts %>% stlm(s.window = "periodic", modelfunction = auto.arima) %>% 
  forecast(h = 12, bootstrap = TRUE) %>% checkresiduals()
ts %>% auto.arima() %>% forecast(h = 12, bootstrap = TRUE) %>% checkresiduals()

ts %>% stlm(s.window = "periodic", modelfunction = auto.arima) %>% 
  forecast(h = 12, bootstrap = TRUE) %>% autoplot()
ts %>% auto.arima() %>% forecast(h = 12, bootstrap = TRUE) %>% autoplot()

