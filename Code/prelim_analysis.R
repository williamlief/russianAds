source("/Users/williamlief/Documents/Research Projects/RussianAds/Code/libs_and_paths.R")
setwd(russia)

df <- read_rds("clean/fb_russia_ads_clean.rds") %>% 
  select(Clicks, Impressions, Spend_RUB, Spend_USD, Create_Date, End_Date)
df2 <- df %>% 
  filter(Impressions!=0)

# total spending
sum(df$Spend_RUB, na.rm=T)
sum(df$Spend_USD, na.rm=T)
sum(df$Impressions, na.rm=T)
sum(df$Clicks, na.rm=T)

# summary statistics
lapply(df,summary)

# remove the ads that were never seen
sum(df$Impressions==0,na.rm=T)/length(df$Impressions)
lapply(df2 %>% filter(Impressions != 0),summary)

# how many ads made up half of the total impressions
kable(df %>% 
  arrange(-Impressions) %>% 
  mutate(tot_imp = sum(Impressions, na.rm=T),
         cum_imp = cumsum(Impressions), 
         pct_cum_imp = cum_imp/tot_imp,
         n = row_number(),
         g50 = pct_cum_imp >= .5) %>% 
  filter((pct_cum_imp >= .10 & lag(pct_cum_imp) < 0.10)|
         (pct_cum_imp >= .25 & lag(pct_cum_imp) < 0.25)|
         (pct_cum_imp >= .50 & lag(pct_cum_imp) < 0.50)|
         (pct_cum_imp >= .75 & lag(pct_cum_imp) < 0.75)|
         (pct_cum_imp >= .90 & lag(pct_cum_imp) < 0.90)) %>% 
  select(n,pct_cum_imp))

pct_cum <- function(data,var) {
  data %>% 
    select_(var) %>% 
    arrange_(paste0("desc(",var,")")) %>% 
    mutate_(var = var) %>% 
    mutate(tot = sum(var, na.rm=T),
           cum = cumsum(var), 
           pct_cum = cum/tot,
           n = row_number()) %>% 
    filter((pct_cum >= .10 & lag(pct_cum) < 0.10)|
             (pct_cum >= .25 & lag(pct_cum) < 0.25)|
             (pct_cum >= .50 & lag(pct_cum) < 0.50)|
             (pct_cum >= .75 & lag(pct_cum) < 0.75)|
             (pct_cum >= .90 & lag(pct_cum) < 0.90)) %>% 
    select(n,pct_cum)
}
kable(pct_cum(df,"Impressions"))
kable(pct_cum(df,"Clicks"))
kable(pct_cum(df,"Spend_RUB"))

#
plot(pct_cum(df,"Impressions"))
lines(pct_cum(df,"Impressions"))

ggplot(df, aes(df$Impressions)) + stat_ecdf()
mydata <- df2$Spend_USD
qplot(unique(mydata), ecdf(mydata)(unique(mydata))*length(mydata), geom='step')



# Distribution of Impressions/Clicks
hist(df2$Impressions)