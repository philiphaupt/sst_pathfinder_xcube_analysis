# SST anomaly

sst_anom <- readr::read_delim("./data/SST/Ostia_SST_anomaly_2021_2022.txt")

sst_anom <- sst_anom %>% 
  rename(sst_anom_degrees = ostia_anom.sst_anomaly.mean) %>% 
  mutate(date = as.Date(time))
# make weekly average
# Extract day of the week (Saturday = 6)
sst_anom$week_day <- as.numeric(format(sst_anom$date, format='%w'))

# Adjust end-of-week date (first Saturday from the original Date)
sst_anom$End_of_Week <- sst_anom$date + (6 - sst_anom$week_day)

# Aggregate over week
sst_weekly_agg <- 
  aggregate(sst_anom_degrees~End_of_Week, FUN=mean, data=sst_anom, na.rm=TRUE)


# PLOT DATA
ggplot()+
  stat_smooth(data = sst_anom, aes(x = as.POSIXct(time), y = sst_anom_degrees), colour = "grey15",span = 0.4)+
  geom_point(data = sst_anom, aes(x = time, y = sst_anom_degrees, colour = sst_anom_degrees >0), alpha = 0.5, size = 0.5)+
  scale_colour_manual(name = 'Anomaly deg C > 0', values = setNames(c('red','blue'),c(T, F)))+
  xlab("Date")+
  ylab("SST anomaly Degrees C")+
  theme_minimal()

# PLOT DATA
ggplot()+
  stat_smooth(data = sst_anom, aes(x = as.POSIXct(time), y = sst_anom_degrees), colour = "grey15",span = 0.4)+
  geom_point(data = sst_anom, aes(x = time, y = sst_anom_degrees, colour = sst_anom_degrees >0), alpha = 0.5, size = 0.5)+
  scale_colour_manual(name = 'Anomaly deg C > 0', values = setNames(c('red','blue'),c(T, F)))+
  xlab("Date")+
  ylab("SST anomaly Degrees C")+
  theme_minimal()

# # PLOT DATA
# ggplot()+
#   geom_smooth(data = sst_weekly_agg, aes(x = as.POSIXct(End_of_Week), y = sst_anom_degrees), colour = "grey15",)+
#   geom_point(data = sst_anom, aes(x = time, y = sst_anom_degrees, colour = sst_anom_degrees >0), alpha = 0.5, size = 0.5)+
#   scale_colour_manual(name = 'Anomaly deg C > 0', values = setNames(c('red','blue'),c(T, F)))+
#   xlab("Date")+
#   ylab("SST anomaly Degrees C")+
#   theme_minimal()
# 
# # PLOT DATA
# ggplot()+
#   geom_smooth(data = sst_anom, aes(x = as.POSIXct(time), y = sst_anom_degrees), colour = "grey15",)+
#   geom_point(data = sst_weekly_agg, aes(x = as.POSIXct(End_of_Week), y = sst_anom_degrees, colour = sst_anom_degrees >0), alpha = 0.5, size = 0.5)+
#   scale_colour_manual(name = 'Anomaly deg C > 0', values = setNames(c('red','blue'),c(T, F)))+
#   xlab("Date")+
#   ylab("SST anomaly Degrees C")+
#   theme_minimal()