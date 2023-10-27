library(tidyverse)

# ANOMALY BASED ON: 2007 - anomaly_max_year
# cut off year for anamoly mean
anomaly_max_year = 2020

# Filter the data for the summer period (June 21 to September 21) for each year and calculate the annual summer means up to 2017.
summer_data <- sst %>%
  filter(month >= 6 & month <= 9) %>%
  group_by(year) %>%
  summarize(mean_temperature = mean(sst_C),
            sd_year_temp = sd(sst_C))

# Filter for years up to 2017
summers_before_anomaly_max_year <- summer_data %>%
  filter(year <= anomaly_max_year)

# Calculate the 10-year mean of the summer temperatures.
ten_year_mean_before_anomaly_max_year <- summers_before_anomaly_max_year %>%
  summarize(ten_year_mean_sst = mean(mean_temperature),
            ten_year_sd_sst = sd(mean_temperature))


# Calculate anomalies for the years 2018 to 2022.
anomalies <- summer_data %>%
  #filter(year >= 2018) %>%
  mutate(anomaly = mean_temperature - ten_year_mean_before_anomaly_max_year$ten_year_mean_sst,
         #sd_temp_anomaly = mean_temperature  ten_year_mean_before_2017$
         )

# Create a bar or line plot of the anomalies using ggplot
library(ggplot2)

ggplot(anomalies, aes(x = year, y = round(anomaly,2), fill = cut(round(anomaly, 1), 6))) +
  geom_bar(stat = "identity") +  # Bar plot
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = paste0("Sea Surface Temperature Summer months (Jun to Sept) Anomalies \nbased on mean STT: 2007 - ",anomaly_max_year),
       x = "Year",
       y = expression("Temperature anomaly " (degree*C)),
       fill = expression("Temperature \nanomaly " (degree*C))) +
  theme_bw()+
  scale_fill_brewer(palette = "RdYlBu",direction = -1, labels = function(x) gsub("\\(|\\[|\\]", "", x)%>% gsub(",", " to ", .))+
  scale_y_continuous(breaks = seq(-1, 2, by = 0.5), limits = c(-1, 2)) +  # Customize the y-axis
  scale_x_continuous(breaks = seq(2007, 2023, by = 1))
