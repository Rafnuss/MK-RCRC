library(tidyverse)
library(lubridate)


d<-read_csv("data/era5_daily.csv") %>%
  mutate(temperature_2m_mean=temperature_2m_mean-273.15)

d %>%
  pivot_longer(c(temperature_2m_mean,total_precipitation_sum,surface_pressure_mean,u_component_of_wind_10m_mean,v_component_of_wind_10m_mean)) %>%
  mutate(doy = yday(time)) %>%
  ggplot(aes(x=doy, y=value)) +
    geom_point()+
  geom_smooth() +
    facet_wrap(~ name, ncol = 1, , scales = "free_y")


d %>%
  pivot_longer(c(temperature_2m_mean,total_precipitation_sum,surface_pressure_mean,u_component_of_wind_10m_mean,v_component_of_wind_10m_mean)) %>%
  mutate(month = month(time)) %>%
  ggplot(aes(group=month, y=value)) +
  geom_boxplot()+
  facet_wrap(~ name, ncol = 1, , scales = "free_y")

data = d %>% mutate(
  spd = sqrt(u_component_of_wind_10m_mean^2 + v_component_of_wind_10m_mean^2),
  dir = (-atan2(u_component_of_wind_10m_mean/spd, v_component_of_wind_10m_mean/spd)*180/pi+90) %% 360
) %>% plot.windrose(spdmax = 10)
