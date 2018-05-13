library(tidyverse)






vars <- c("Ad_Clicks", "Ad_Impressions", "Ad_Spend")

df2 <- df %>% 
  mutate_at(vars, funs(as.numeric(as.numeric(gsub("[^[:digit:].]", "",.)))))

lapply(df2[vars], function(x) summary(x))

lapply(df2[vars], function(x) hist(x)) # this is crude, but we can see the data is crazy skewed

df3 <- df2 %>% 
  filter(Ad_Impressions > 1000)
