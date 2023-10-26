# sst annual increases

library(tidyverse)
# overall means

sst %>% 
  group_by(year(time)) %>% 
  summarise(mean_sst = mean(sst_C))

#summer time increases
# group by summer and calculate means

sst %>% filter(month %in% summer_months) %>% 
  group_by(year(time)) %>% 
  summarise(mean_sst = mean(sst_C))

