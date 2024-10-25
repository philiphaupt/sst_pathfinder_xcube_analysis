# plot time series

# PLOT DATA - fancy white back
ggplot(data = sst, 
       aes(x = time, y = sst_C),
       colour = sst_C
       )+
   scale_colour_gradient2(name = expression("Sea Surface \nTemperature \nscale "(degree~C)) ,low = "blue", mid = "chartreuse4", high = "red3",
                          midpoint=12.5)+ ##span = 0.4)+
  #scale_fill_brewer(palette = "RdYlBu",direction = -1, labels = function(x) gsub("\\(|\\[|\\]", "", x) %>% gsub(",", " - ", .))+
  scale_x_datetime(name = "Date", date_breaks = "1 year", date_labels = c("%Y"))+
  stat_smooth(aes(
    colour = ..y.. 
  ),
  method = "loess")+
  geom_point(data = sst,
             aes(x = time,
                 y = sst_C,
                 colour = sst_C
             ),
             alpha = 0.75,
             size = 0.8)+
  
  labs(y = expression("Sea Surface Temperature " (degree*C)))+
  ggplot2::ggtitle("Sea Surface Temperature in near Whitstable in Thames\nSource data : https://eutro-cube.cefas.co.uk/")+
  theme_classic()+
  theme(text = element_text(size = 16))




# PLOT DATA - dark
ggplot(data = sst, 
       aes(x = as.POSIXct(time), y = sst_C), 
       colour = sst_C)+
  scale_colour_gradient2(name = 'SST degrees',low = "blue", mid = "white" , high = "red",
                         midpoint=12.5)+ ##span = 0.4)+
  
  stat_smooth(aes(
    colour = ..y.. 
  ),
  method = "loess")+
  geom_point(data = sst,
             aes(x = time,
                 y = sst_C,
                 colour = sst_C
             ),
             alpha = 0.75,
             size = 0.8)+
  scale_x_datetime(name = "Date", date_breaks = "1 year", date_labels = c("%Y"))+
  xlab("Date")+
  ylab("SST Degrees C")+
  # geom_tile(aes(x = time,
  # y = sst_C,
  # colour = sst_C)) +
  # scale_fill_viridis_c(option = "magma")+
  ggdark::dark_theme_minimal() +
  theme(text = element_text(size = 16))

#-----
# PLOT Red and blue - two lines (average and summer average, plots boxes)
# Create rectangles
rects <- data.frame(xstart = c("2007-06-21 00:00:00 UTC", "2008-06-21 00:00:00 UTC", "2009-06-21 00:00:00 UTC", "2010-06-21 00:00:00 UTC","2011-06-21 00:00:00 UTC","2012-06-21 00:00:00 UTC","2013-06-21 00:00:00 UTC", "2014-06-21 00:00:00 UTC", "2015-06-21 00:00:00 UTC", "2016-06-21 00:00:00 UTC", "2017-06-21 00:00:00 UTC","2018-06-21 00:00:00 UTC", "2019-06-21 00:00:00 UTC", "2020-06-21 00:00:00 UTC", "2021-06-21 00:00:00 UTC", "2022-06-21 00:00:00 UTC", "2023-06-21 00:00:00 UTC"), 
                    xend = c("2007-09-21 00:00:00 UTC", "2008-09-21 00:00:00 UTC", "2009-09-21 00:00:00 UTC", "2010-09-21 00:00:00 UTC","2011-09-21 00:00:00 UTC","2012-09-21 00:00:00 UTC", "2013-09-21 00:00:00 UTC", "2014-09-21 00:00:00 UTC", "2015-09-21 00:00:00 UTC", "2016-09-21 00:00:00 UTC", "2017-09-21 00:00:00 UTC","2018-09-21 00:00:00 UTC", "2019-09-21 00:00:00 UTC", "2020-09-21 00:00:00 UTC", "2021-09-21 00:00:00 UTC", "2022-09-21 00:00:00 UTC", "2023-09-21 00:00:00 UTC"))#, col = letters[1:5])
# time series plot
ggplot()+
  stat_smooth(data = sst %>% filter(time > "2006-01-01 00:00:00"), aes(x = as.POSIXct(time), y = sst_C), colour = "grey30",method = "loess")+##span = 0.4)+
  geom_point(data = sst %>% filter(time > "2006-01-01 00:00:00"), aes(x = time, y = sst_C, colour = sst_C >18), alpha = 0.5, size = 0.8)+
  scale_colour_manual(name = 'SST deg C > 18', values = setNames(c('red','blue'),c(T, F)))+
  xlab("Date")+
  ylab("SST Degrees C")+
  theme_minimal()+
  geom_rect(data = rects, aes(xmin = as.POSIXct(xstart), xmax = as.POSIXct(xend), 
                              ymin = 14, ymax = 22), col = "black",fill = "white",lty = "dashed" , alpha = 0.25)+
  xlim(c(as.POSIXct("2006-01-01 00:00:00 UTC"),as.POSIXct("2024-01-04 00:00:00 UTC")))+
  stat_smooth(data = sst %>% filter(month %in% summer_months), aes(x = as.POSIXct(time), y = sst_C), method = "loess")##span = 0.4)

