## SST
library("tidyverse")
library(ggforce)
library(data.table)
library("ggdark")
library(lubridate)

#sst <- readr::read_delim("./data/SST/Ostia_SST_2006_2022.txt")
sst <- read_delim("data/Ostia_SST_2006_2023.txt",
                  col_types = cols(`time` = col_datetime("%Y-%m-%d %H:%M:%S"))
)

sst <- sst %>% 
  rename(sst_degrees = ostia.analysed_sst.mean) %>% 
  mutate(sst_C = sst_degrees-273.15)

# add summer season
summer_months = c(6,7,8,9)
# add year
sst$year <- year(as.POSIXlt(sst$time))
# add month
sst$month <- month(as.POSIXlt(sst$time))
# add day
sst$day <- day(as.POSIXlt(sst$time))

