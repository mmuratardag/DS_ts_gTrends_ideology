
load("/data/ts.RData")

library(modeltime.gluonts)
library(tidymodels)
library(tidyverse)
library(timetk)

ts <- ts %>% filter(keyword %in% c("Integration_of_Immigrants",
                                   "Nationalism",
                                   "Populism")) %>% 
  rename(id = keyword) %>%
  select(id, date, hits)

horizon <- 12

new_data <- ts %>%
  group_by(id) %>%
  future_frame(.length_out = horizon) %>%
  ungroup()

model_fit_deepar <- deep_ar(
  id = "id",
  freq = "M",
  prediction_length = horizon,
  lookback_length = 2*horizon,
  epochs = 20
) %>%
  set_engine("gluonts_deepar") %>%
  fit(hits ~ date + id, ts)

modeltime_forecast_tbl <- modeltime_table(
  model_fit_deepar
) %>%
  modeltime_forecast(
    new_data = new_data,
    actual_data = ts,
    keep_data = TRUE
  ) %>%
  group_by(id) 

modeltime_forecast_tbl %>%
  plot_modeltime_forecast(
    .conf_interval_show = FALSE, 
    .facet_ncol = 2, 
    .facet_scales = "fixed",
    .interactive = FALSE,
    .title = "Forecast plot multivariate time series with deep learning")