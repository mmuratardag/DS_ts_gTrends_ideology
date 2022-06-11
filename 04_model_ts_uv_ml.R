
load("data/ts.RData")

library(tidymodels)
library(tidyverse)
library(lubridate)
library(modeltime)
library(timetk)

ts <- ts %>%
  filter(keyword %in% c("National_Identity")) %>%
  select(date, hits)

ts_splits <- initial_time_split(ts, prop = 0.9)

model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(hits ~ date, data = training(ts_splits))

model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(hits ~ date + as.numeric(date) + factor(month(date, label = TRUE),
                                              ordered = FALSE),
      data = training(ts_splits))

model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(hits ~ date, data = training(ts_splits))

models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_prophet
)
models_tbl

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(ts_splits))
calibration_tbl

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(ts_splits),
    actual_data = ts
  ) %>%
  plot_modeltime_forecast(
    .conf_interval_alpha = .1,
    .title = "Forecast plot of 3 models",
    .y_lab = "Search volume: public interest in national identity worldwide
    \nRelative Range is 0-100",
    .interactive = FALSE
  )

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )
