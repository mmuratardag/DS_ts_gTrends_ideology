
library(tidyverse)
library(lubridate)
library(gtrendsR)

pull_these_topics <- c("/m/05gd9",    # Nationalism
                       "/m/06473",    # Patriotism
                       "/m/0g5qfvp",  # National identity
                       "/m/01d_sm",   # Populism
                       "/m/0dd_44")   # Integration of immigrants

pulled <- gtrends(keyword = pull_these_topics,
                  geo = "",
                  time = "all",
                  gprop = "web",
                  low_search_volume = TRUE)

ts <- pulled %>% .$interest_over_time %>%
  mutate(
    keyword = case_when(
      keyword == "/m/05gd9" ~ "Nationalism",
      keyword == "/m/06473" ~ "Patriotism",
      keyword == "/m/0g5qfvp" ~ "National_Identity",
      keyword == "/m/01d_sm" ~ "Populism",
      keyword == "/m/0dd_44" ~ "Integration_of_Immigrants",
      TRUE ~ as.character(keyword)
    )
  )

save(ts, file = "data/ts.RData")
save(pulled, file = "list/pulled.RData")
