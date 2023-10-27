#main_script

# sst 
source("./scripts/read_sst.R", echo = TRUE)

# sst time series plot
file.edit("./scripts/plot_temperature_time_series.R", echo = TRUE)

# sst means
source("./scripts/sst_means.R", echo = TRUE)

# sst number of days over thresholds
source("./scripts/sst_num_days_over_thresholds_18_and_22.R", echo = TRUE)

# my sst anomaly
# I want to calculate water temperature anomaly of one year compared to a long term data set 
# using R statistical software. My data set starts in 2007 and contains a single reading for each day. 
# I would like to isolate the summers period (between 21 June to 21 September) for each year, and then 
# calculate the means up to 2017. From that I want to construct a 10 year mean, that I want to calculate 
# the anomalies for each year following that (i.e. 2018, 2019, 2020, 2021, and 2022). Once that is done, 
# I want to use ggplot, to create a graph to show the anomalies as bars or lines that go above or below 
# the 10 year mean. Can you generate some code that would help me to do this?
file.edit("./scripts/sst_2007-2017_v_recent_years_anomly.R")


# read sst anomaly
# source("./scripts/read_sst_anomaly.R", echo = TRUE) - cefas pre calculated anomaly


