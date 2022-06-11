
load("data/ts.RData")
library(tidyverse)
ts_df <- ts
ts_wide <- ts_df %>%
  pivot_wider(id_cols = date,
              names_from = keyword,
              values_from = hits)
colnames(ts_wide)
mv_ts <- ts_wide
mv_ts$date <- as.Date(mv_ts$date)
mv_ts <- ts(data = mv_ts[-1], start = c(2004, 1), end = c(2022, 5), frequency = 12)

library(dynlm)
mod_baseline <- dynlm(d(Populism) ~ L(Populism, 1)
                      + L(Populism, 12),
                      data = mv_ts)
summary(mod_baseline)

library(easystats)
plot_model <- function(model_obj) {
  mod_obj_p <- parameters(model_obj)
  plot(mod_obj_p)
}
library(broomExtra)
model_output <- function(model_obj, model_name) {
  glance(model_obj) %>% 
    mutate(model_name = model_name) %>%
    relocate(model_name)
}

plot_model(mod_baseline)
mod_baseline_op <- model_output(mod_baseline, "Baseline")

mod_dv_st <- dynlm(d(Populism) ~ L(Populism, 1)
                   + L(Populism, 12)
                   + season(Populism)
                   + trend(Populism),
                   data = mv_ts)
summary(mod_dv_st)
plot_model(mod_dv_st)
mod_dv_st_op <- model_output(mod_dv_st, "Response - Season & Trend")

mod_mv <- dynlm(d(Populism) ~ L(Populism, 1)
                + L(Populism, 12)
                + Nationalism
                + L(Nationalism, 1)
                + L(Nationalism, 12),
                data = mv_ts)
summary(mod_mv)
plot_model(mod_mv)
mod_mv_op <- model_output(mod_dv_st, "Multivariate")

mod_mv_st <- dynlm(d(Populism) ~ L(Populism, 1)
                   + L(Populism, 12)
                   + Nationalism
                   + season(Nationalism)
                   + trend(Nationalism),
                   data = mv_ts)
summary(mod_mv_st)
plot_model(mod_mv_st)
mod_mv_st_op <- model_output(mod_dv_st, "Multivariate - Season & Trend")

mod_all_sig <- dynlm(d(Populism) ~ L(Populism, 1)
                     + L(Populism, 12)
                     + trend(Populism)
                     + Nationalism
                     + L(Nationalism, 1),
                     data = mv_ts)
summary(mod_all_sig)
plot_model(mod_all_sig)
mod_all_sig_op <- model_output(mod_all_sig, "Only significant predictors")

mod_par <- dynlm(d(Populism) ~ L(Populism, 1)
                 + Nationalism,
                 data = mv_ts)
summary(mod_par)
plot_model(mod_par)
mod_par_op <- model_output(mod_par, "Parsimonious")


rbind(mod_baseline_op, mod_dv_st_op, mod_mv_op, mod_mv_st_op, mod_all_sig_op, mod_par_op)

jtools::plot_summs(mod_baseline,     # baseline
                   mod_dv_st,        # 
                   mod_mv,           # 
                   mod_mv_st,        #
                   mod_all_sig,
                   mod_par,      # selected model
                   model.names = c("Baseline Model -- only the lags of predicted time series",
                                   "Response - Season & Trend",
                                   "Multivariate",
                                   "Multivariate - Season & Trend",
                                   "Only significant predictors",
                                   "Parsimonious model"),
                   legend.title = "Models") + 
  labs(title = "Dynamic linear models for time series",
       subtitle = "Time Series Regression | Start: 2004-01-01 - End: 2022-05-29
       \nAdjusted R-squared: 22 % for the baseline model; 30 % for the selected parsimonious model; all other models range between 36-39 %",
       caption = "Data Source: Google Trends
       \nAll time series are are Google topics and reflect relative search volumes, which are proxies for public interest to the topics
       \nSee https://blog.google/products/search/15-tips-getting-most-out-google-trends for more info
       \nL(, #) L stands for lag; # is the lagged number of months of the times series in the models")
