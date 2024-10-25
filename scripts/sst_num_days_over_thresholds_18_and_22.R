library(tidyverse)
library(data.table)
library(RColorBrewer)

sst %>% 
ggplot()+
  geom_histogram(aes(y = sst_C))+
  facet_wrap(~year)
 
# NOT IN FUNCTION 
`%!in%` = function(x, y) !(x%in%y)

# temperature colour n days at temp
sst %>%
  filter(year %!in% c(2006, 2025)) %>% 
  mutate(sst_rnd = round(sst_C, 0)) %>% 
  ggplot(aes(x = sst_rnd, fill = cut(sst_rnd, 9))) +
  geom_histogram(binwidth = 1) +
  labs(x = expression("Sea Surface Temperature " (degree*C)), y = "Number of days (in a year at temperature)", fill = expression("Sea Surface \nTemperature " (degree*C))) +
  facet_wrap(~ year) +
  theme_bw() +
  scale_fill_brewer(palette = "RdYlBu",direction = -1, labels = function(x) gsub("\\(|\\[|\\]", "", x) %>% gsub(",", " - ", .))+
  geom_vline(xintercept =20, colour = "black", linetype ="dashed")+
  theme(text = element_text(size = 16))
#  coord_flip()

# Grey and red - as used in Fishing news
# Grey and red n days
sst %>%
  filter(year %!in% c(2006, 2025)) %>% 
  mutate(sst_rnd = round(sst_C, 0)) %>%
  ggplot(aes(x = sst_rnd, fill = sst_rnd >= 20)) +
  geom_histogram(binwidth = 1) +
  labs(x = expression("Sea Surface Temperature " (degree*C)), y = "Number of days (in a year at temperature)") +
  facet_wrap(~ year) +
  theme_bw() +
  geom_vline(xintercept =20, colour = "black", linetype ="dashed")+
  scale_fill_manual(values = c("grey", "red"),
                    labels = c("Below 20°C", "Over 20°C"),
                    guide = guide_legend(title = "Temperature Range"))+
  theme(text = element_text(size = 16))

# ICES poster blue and yellow - 
ices_plot <- sst %>%
  filter(year %!in% c(2006, 2025)) %>% 
  mutate(sst_rnd = round(sst_C, 0)) %>%
  ggplot(aes(x = sst_rnd, fill = sst_rnd >= 20)) +
  geom_histogram(binwidth = 1) +
  labs(x = expression("Sea Surface Temperature " (degree*C)), 
       y = "Number of days (in a year at temperature)") +
  facet_wrap(~ year) +
  theme_bw() +
  geom_vline(xintercept = 20, colour = "#237194", linetype = "dashed") +
  scale_fill_manual(values = c("grey", "#faa32b"),#"#00bbd6", #f1f1f2"
                    labels = c("Below 20°C", "Over 20°C"),
                    guide = guide_legend(title = "Temperature Range")) +
  theme(
    text = element_text(size = 16),
    strip.background = element_rect(fill = "#f1f1f2", colour = "#237194", size = 1),
    strip.text = element_text(colour = "#237194"),
    legend.position = c(0.95, 0.03),       # Position the legend inside the plot area
    legend.justification = c("right", "bottom"), # Align the legend to the top-right corner
    legend.background = element_rect(fill = alpha('white', 0.5)), # Optional: semi-transparent background
    panel.border = element_rect(colour = "#237194", fill = NA, size = 0.95)
    )

# Print the plot
print(ices_plot)

# Define the path to the "outputs" directory
output_dir <- file.path(getwd(), "outputs")

# Create the "outputs" directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Define the file path
file_path <- file.path(output_dir, "n_days_at_temp_ices_plot_v2.png")

# Save the plot with high resolution (300 DPI) and A5 size
ggsave(
  filename = file_path,
  plot = ices_plot,
  width = 21,  # A5 width in cm
  height = 14.8,   # A5 height in cm
  units = "cm",
  dpi = 300      # High resolution for printing
)



#table
over20Cdays <- sst %>%
  filter(year %!in% c(2006, 2025)) %>% 
  filter(month >= 6, month <= 10 ) %>% 
  mutate(over20C = if_else(sst_C >=20, "over20", "under"),
         before2018 = if_else(year < 2018, "before2018", "after2018")) %>% 
  group_by(before2018, year, over20C) %>% 
  summarise(days_per_year = n()) %>% 
  ungroup() %>% 
  group_by(before2018, over20C) %>% 
  summarise(mean(days_per_year, na.rm = TRUE))  
  
# Grey and red n days
sst %>%
  filter(year %!in% c(2006, 2025)) %>% 
  mutate(temp_range = ifelse(sst_C < 20, "Below 20", "Above 20"),
                             sst_trunc = trunc(sst_C)
                             ) %>%
  ggplot(aes(x = sst_trunc, fill = temp_range)) +
  geom_histogram(binwidth = 1) +
  labs(x = expression("Sea Surface Temperature " (degree*C)), y = "Number of days in a year at temperature") +
  facet_wrap(~ year) +
  theme_bw() +
  scale_fill_manual(values = c("Below 20" = "grey", "Above 20" = "red"),
                    guide = guide_legend(title = "Temperature Range"))+
  geom_vline(xintercept =20, colour = "black", linetype ="dashed")+
  coord_flip()


# # NOT RUN {earleir versions}
# #18 deg
# days_above_18 <- sst %>% 
#   mutate(t_18 = if_else(sst_C >= 18, "above", "under")) %>% 
#   group_by(year(time)) %>% 
#   count(t_18 == "above") %>% 
#   rename(year = `year(time)`, 
#          t_18 = `t_18 == "above"`)
# 
# ggplot(days_above_18 %>% filter(t_18 == TRUE))+
#   geom_point(aes(x = `year`, 
#                  y = n),
#              size = 5,
#              color = "black"
#              )+
#   theme_minimal()
# 
# #22 deg
# days_above_21 <- sst %>% 
#   mutate(t_21 = if_else(sst_C >= 21, "above", "under")) %>% 
#   group_by(year(time)) %>% 
#   count(t_21 == "above") %>% 
#   rename(year = `year(time)`, 
#          t_21 = `t_21 == "above"`)
# 
# ggplot(days_above_21 %>% filter(t_21 == TRUE))+
#   geom_point(aes(x = `year`, 
#                  y = n),
#              size = 5,
#              color = "red"
#   )+
#   theme_minimal()
# 
# #-----------------------
# # N days exceeding temp
# gg_b <- ggplot_build(sst %>% filter(year >2006, year < 2023) %>% 
#                        ggplot()+
#                        geom_histogram(aes(x = sst_C), binwidth=1, #fill = rainbow(378)
#                        )+
#                        facet_wrap(~year))
# gg_b
# # Extract the data
# n_days_at_temp <- gg_b$data[[1]] %>% 
#   select(temperature = x, n_days = count, PANEL) %>% 
#   mutate(year = 2006 + as.numeric(PANEL)) %>% 
#   select(-PANEL) %>% 
#   pivot_wider(names_from = temperature, values_from = n_days) 
# 
# 
# 
# nu_bins <- dim(gg_b$data[[1]])[1]/16
# 
# 
# ggplot(sst) + geom_histogram(aes(x = sst %>% filter(year >2006, year < 2024)), binwidth=1, fill = rainbow(nu_bins))#nu_bins
# #--
# sst %>% filter(year >2006, year < 2024) %>% 
#   ggplot()+
#   geom_histogram(aes(x = sst_C, 
#                      #fill = sst_C
#   ) , binwidth=1)+
#   # scale_fill_gradient2(name = 'SST degrees',low = "blue", mid = "white" , high = "red",
#   #                         midpoint=12)+
#   # 
#   geom_vline(aes(xintercept = 20),col = "black", lty = 2)+
#   theme_minimal()+
#   xlab("Water temperature degrees Centrigrade")+
#   ylab("Number of days")+
#   facet_wrap(~year,scales = "fixed")
# 
# 
# # gg_b <- ggplot_build(
# #   ggplot() + geom_histogram(aes(x = myData), binwidth=.1)
# # )
# # 
# # nu_bins <- dim(gg_b$data[[1]])[1]
# # 
# # ggplot() + geom_histogram(aes(x = myData), binwidth=.1, fill = rainbow(nu_bins))
# 
# 

