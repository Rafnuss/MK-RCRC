library(tidyverse)
library(ggplot2)
library(plotly)
library(GeoPressureR)
library(GeoLocatoR)
library(lubridate)

pkg <- read_gldp("data/datapackage/datapackage.json")

# Set where the circular x-axis starts (day-of-year that maps to x = 0)
x0 <- 200
# Length of the cycle; change to 360 if you explicitly want a 360-day wrap
cycle_length <- 365


plt_lat_doy <- paths(pkg) %>%
  filter(type == "most_likely") %>%
  # filter(tag_id=="28CG") %>%
  left_join(
    staps(pkg),
    by = join_by(tag_id, stap_id)
  ) %>%
  mutate(
    doy_start = yday(start),
    doy_end = yday(end),
    x_start = (doy_start - x0 + cycle_length) %% cycle_length,
    x_end = (doy_end - x0 + cycle_length) %% cycle_length,
    wraps = x_end < x_start
  ) %>%
  group_by(tag_id) %>%
  arrange(start, .by_group = TRUE) %>%
  mutate(stap_order = row_number()) %>%
  rowwise() %>%
  mutate(
    # Insert NA when a stap wraps to break the path instead of drawing a full-length line
    path_x = list(
      if (!wraps) c(x_start, x_end) else c(x_start, cycle_length, NA, 0, x_end)
    ),
    point_id = list(seq_along(path_x))
  ) %>%
  unnest(c(path_x, point_id)) %>%
  ungroup() %>%
  arrange(tag_id, stap_order, point_id) %>%
  ggplot() +
  geom_path(
    aes(x = path_x, y = lat, colour = tag_id, group = tag_id),
    linewidth = 1
  ) +
  scale_x_continuous(
    name = sprintf("Month/Day (x = 0 at doy %s)", x0),
    limits = c(0, cycle_length),
    breaks = scales::pretty_breaks(n = 12)(c(0, cycle_length)),
    labels = function(x) {
      (as.Date("2001-01-01") +
        ((x + x0 - 1) %% cycle_length)) |>
        format("%b %d")
    }
  ) +
  labs(y = "Latitude", colour = "Tag") +
  theme_minimal()

# Display as static ggplot; wrap with ggplotly() for interactivity
plt_lat_doy
# ggplotly(plt_lat_doy)
