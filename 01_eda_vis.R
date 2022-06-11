
load("data/ts.RData")
library(tidyverse)

ts %>% group_by(keyword) %>%
  summarise(topic_mean = mean(hits),
            topic_min = min(hits),
            topic_max = max(hits),
            min_date = min(as.Date(date)),
            max_date = max(as.Date(date)))

ts %>% ggplot(aes(x = date, y = hits, color = keyword)) + 
  geom_line() +
  scale_y_continuous(expand = expansion(add = c(0,0))) +
  labs(x = "", y = "Normalized search volume with 0 - 100 range",
       title = "Global public interest",
       subtitle = "by search TOPIC",
       caption = "Google offers topics - a group of already clustered search terms that share the same concept or entity.
       \n These topics are language agnostic & account for spelling variations & mistakes.
       \nsee https://blog.google/products/search/15-tips-getting-most-out-google-trends/") +
  scale_fill_manual(values = wesanderson::wes_palette(name="GrandBudapest1")) +
  theme_bw() +
  theme(legend.position = 'top')

load("list/pulled.RData")

pulled$related_queries %>% 
  filter(related_queries == "top" & !is.na(subject) & subject!='<1') %>%
  mutate(
    keyword = case_when(
      keyword == "/m/05gd9" ~ "Nationalism",
      keyword == "/m/06473" ~ "Patriotism",
      keyword == "/m/0g5qfvp" ~ "National_Identity",
      keyword == "/m/01d_sm" ~ "Populism",
      keyword == "/m/0dd_44" ~ "Integration_of_Immigrants",
      TRUE ~ as.character(keyword)
    ),
    value = as.factor(value),
    subject = as.numeric(subject)
  ) %>%
  filter(subject > 10) %>%
  ggplot(aes(x = reorder(value, subject), y = subject)) + 
  geom_col(width = 0.3) +
  coord_flip() +
  scale_y_continuous(expand=expansion(add=c(0,10))) +
  labs(title="Google Topics -- Example related search queries",
       subtitle = "Topics are nationalism, patriotism, national identity, populism, integration of immigrants",
       caption = "Google offers topics - a group of already clustered search terms that share the same concept or entity.
       \n These topics are language agnostic & account for spelling variations & mistakes.
       \nsee https://blog.google/products/search/15-tips-getting-most-out-google-trends/",
       y="",
       x="") + theme_bw()

library(tidyquant)
library(timetk)
ts %>% group_by(keyword) %>%
  plot_time_series(date, hits, .facet_ncol = 3, .interactive = FALSE,
                   .facet_scales = "fixed",
                   .title = "Global public interest by search topic with trend lines")

ts %>% filter(keyword %in% c("Nationalism", "Patriotism")) %>%
  group_by(keyword) %>%
  plot_stl_diagnostics(
    date, hits,
    .facet_scales = "fixed",
    .frequency = "auto", .trend = "auto",
    .feature_set = c("observed", "season", "trend", "remainder"),
    .interactive = FALSE,
    .title = "Seasonal & trend decomposition for relatively high search volume topics")

ts %>% filter(keyword %in% c("National_Identity", "Populism", "Integration_of_Immigrants")) %>%
  group_by(keyword) %>%
  plot_stl_diagnostics(
    date, hits,
    .facet_scales = "fixed",
    .frequency = "auto", .trend = "auto",
    .feature_set = c("observed", "season", "trend", "remainder"),
    .interactive = FALSE,
    .title = "Seasonal & trend decomposition for relatively low search volume topics")

ts %>% 
  pivot_wider(id_cols = date,
              names_from = keyword,
              values_from = hits) %>%
  plot_acf_diagnostics(date, 
                       Integration_of_Immigrants,
                       .ccf_vars = c(Nationalism, Patriotism, Populism, National_Identity),
                       .lags = 36, .interactive = FALSE,
                       .title = "Lag diagnostics for Integration of Immigrants
                       \nwith Natioanism, Patriotism, Populism & National Identity as the cross correlation function variables")

ts %>% 
  pivot_wider(id_cols = date,
              names_from = keyword,
              values_from = hits) %>%
  select(-date) %>% 
  correlation::correlation() %>% summary() %>%
  plot() +
  labs(title = "Correlations across Search Volume",
       subtitle = "Used Google Topics are Integration of Immigrants, Nationalism, Patriotism, Populism, National Identity",
       caption = "Google Topics are used as a proxy for public interest
       \nSee: https://blog.google/products/search/15-tips-getting-most-out-google-trends/ to find out more about Google Topics") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45))

wide <- ts %>% 
  pivot_wider(id_cols = date,
              names_from = keyword,
              values_from = hits) %>%
  select(Nationalism:Integration_of_Immigrants)
wide <- wide %>%
  mutate(n_months = 1:nrow(wide)) %>%
  mutate(across(where(is.integer), as.numeric)) %>%
  as.data.frame()

names(wide)

bl <- as.matrix(data.frame(from = c("Nationalism", "Patriotism",
                                    "National_Identity", "Populism",
                                    "Integration_of_Immigrants"),
                           to = rep("n_months", 5)))
library(bnlearn)
set.seed(666)
bn_op <- tabu(wide, blacklist = bl)
library(qgraph)
qgraph(bn_op, vsize = 12, label.cex = 2)
library(parallel)
cl <- makeCluster(7)
set.seed(666)
boot_res <- boot.strength(data = wide,
                          R = 10000,
                          algorithm = "tabu",
                          algorithm.args = list(blacklist = bl),
                          cluster = cl)
avgnet_threshold <- averaged.network(boot_res, threshold = .99)
qgraph(avgnet_threshold, vsize = 12, label.cex = 5,
       title = "Causal Skeleton with Bayesian belief network")
title(cex.sub = .9, sub = "1. time passed determines public interest in national identity 2. public interest in integration of immigrants determines public interest in nationalism, which in turn determines interest in populism")
stopCluster(cl)

ts %>% 
  pivot_wider(id_cols = date,
              names_from = keyword,
              values_from = hits) %>%
  plot_acf_diagnostics(date, 
                       Populism,
                       .ccf_vars = c(Nationalism, Integration_of_Immigrants),
                       .lags = 36, .interactive = FALSE,
                       .title = "Lag diagnostics for Populism 
                       \nwith Nationalism & Integration of Immigrants as the cross correlation function variables")
