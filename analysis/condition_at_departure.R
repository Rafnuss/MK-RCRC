library(tidyverse)
library(ggplot2)
library(plotly)
library(GeoPressureR)
library(GeoLocatoR)

pkg <- read_gldp("data/datapackage/datapackage.json")


plot(kg)
plot_path(p |> filter(tag_id == "30II"))

thr_long <- 30

# Read data
p <- paths(pkg) %>%
  filter(type == "most_likely") %>%
  left_join(
    staps(pkg),
    by = join_by(tag_id, stap_id)
  ) %>%
  select(-c(j, interp, known_lat, known_lon, include)) %>%
  mutate(
    duration = stap2duration(.),
    stage = case_when(
      known ~ "wintering",
      !known & duration > thr_long ~ "breeding",
      #!known & duration < thr_long & duration > 5 & month(start)>7 ~ "post stopover",
      #!known & duration < thr_long & duration > 5 & month(start)<=7 ~ "pre stopover (5-30d.)",
      TRUE ~ "stopover"
    )
  )

ggplot(p) +
  geom_histogram(aes(x = duration, color = stage)) +
  facet_wrap(~tag_id) +
  geom_vline(xintercept = thr_long, colour = "red")


p %>%
  filter(stage == "wintering") %>%
  mutate(
    doy_end = yday(end),
    year_end = year(end)
  ) %>%
  ggplot(aes(x = doy_end, y = year_end)) +
  geom_point()


daily_mwamba <- read_csv(
  "data/ERA5/ERA5_daily_Mwamba.csv",
  show_col_types = FALSE
) %>%
  transmute(
    date,
    temperature = temperature_2m - 273.15,
    precipitation = total_precipitation_sum,
    wind_speed = sqrt(u_component_of_wind_10m^2 + v_component_of_wind_10m^2),
    wind_direction = speed2bearing(
      u_component_of_wind_10m + 1i * v_component_of_wind_10m
    )
  )

hourly_mwamba <- read_csv(
  "data/ERA5/ERA5_hourly_Mwamba.csv",
  show_col_types = FALSE
) %>%
  transmute(
    date,
    temperature = temperature_2m - 273.15,
    precipitation = total_precipitation,
    wind_speed = sqrt(u_component_of_wind_10m^2 + v_component_of_wind_10m^2),
    wind_direction = speed2bearing(
      u_component_of_wind_10m + 1i * v_component_of_wind_10m
    )
  )

daily_mtwari <- read_csv(
  "data/ERA5/ERA5_daily_Mtwari.csv",
  show_col_types = FALSE
) %>%
  transmute(
    date,
    temperature = temperature_2m - 273.15,
    precipitation = total_precipitation_sum,
    ws = u_component_of_wind_10m + 1i * v_component_of_wind_10m,
    wind_speed = abs(ws),
    wind_direction = speed2bearing(ws),
    wind_support = windsupport(ws, complex(0, -1))
  )

hourly_mtwari <- read_csv(
  "data/ERA5/ERA5_hourly_Mtwari.csv",
  show_col_types = FALSE
) %>%
  transmute(
    date,
    temperature = temperature_2m - 273.15,
    precipitation = total_precipitation,
    wind_speed = sqrt(u_component_of_wind_10m^2 + v_component_of_wind_10m^2),
    wind_direction = speed2bearing(
      u_component_of_wind_10m + 1i * v_component_of_wind_10m
    )
  )


## Annual and monthly average
daily_mtwari %>%
  pivot_longer(c(temperature, precipitation, wind_speed, wind_direction)) %>%
  mutate(doy = yday(date)) %>%
  ggplot(aes(x = doy, y = value)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~name, ncol = 1, , scales = "free_y")


daily_mtwari %>%
  pivot_longer(c(temperature, precipitation, wind_speed, wind_direction)) %>%
  mutate(month = month(date)) %>%
  ggplot(aes(group = month, y = value)) +
  geom_boxplot() +
  facet_wrap(~name, ncol = 1, , scales = "free_y")


#####

ggplotly(
  ggplot() +
    geom_line(
      data = hourly_mwamba %>% filter(year(date) < 2023),
      aes(x = date, y = precipitation)
    )
)


ggplotly(
  ggplot() +
    geom_step(
      data = daily_mwamba %>% filter(year(date) < 2023 & year(date) > 2019),
      aes(x = yday(date), y = wind_speed, colour = as.factor(year(date)))
    ) +
    geom_vline(
      data = pp %>% filter(stap_id == 1),
      aes(xintercept = as.numeric(yday(end)), colour = as.factor(year(end)))
    ) +
    geom_line(
      data = daily_mwamba %>%
        mutate(date = yday(date)) %>%
        group_by(date) %>%
        summarise(across(
          c(temperature, precipitation, wind_speed, wind_direction),
          median
        )),
      aes(x = date, y = wind_speed)
    ) +
    xlim(270, 360)
)


ggplotly(
  ggplot() +
    geom_step(
      data = daily_mtwari %>% filter(year(date) < 2024 & year(date) > 2020),
      aes(x = yday(date), y = wind_speed, colour = as.factor(year(date)))
    ) +
    geom_vline(
      data = pp %>% filter(status == "wintering"),
      aes(xintercept = as.numeric(yday(end)), colour = as.factor(year(end)))
    ) +
    geom_line(
      data = daily_mtwari %>%
        mutate(date = yday(date)) %>%
        group_by(date) %>%
        summarise(across(
          c(temperature, precipitation, wind_speed, wind_direction),
          median
        )),
      aes(x = date, y = wind_speed)
    )
)
