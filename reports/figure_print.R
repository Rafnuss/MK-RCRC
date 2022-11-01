library(GeoPressureR)
library(raster)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(maps)
library(ggmap)
library(ggnewscale)
library(readxl)
require(gridExtra)
library(cowplot)
library(moveVis)
library(move)
library(lubridate)
library(colorspace)
library(ggsn)
library(ncdf4)
library(khroma)

gdl_list <- read_excel("data/gpr_settings.xlsx") %>%
  filter(keep) %>%
  .$gdl_id


# Figure non-breeding site ----

p0 <- get_googlemap(center = c(lon = 40, lat = -11), zoom = 8, maptype = "satellite", size = c(1280, 1280), scale = 4) %>%
  ggmap() + theme_map()

# p0 + coord_fixed(
#   xlim=c(20, 37),
#   ylim=c(4, 12),
#   ratio=1/cos(pi*41.39/180),
#   expand = F
# )+ scalebar(x.min= 20, x.max = 37, y.min=4, y.max=12, transform=T,
#            location = "topright", dist = 100, height = 0.1, model = "WGS84", dist_unit = "km")

p <- p0
for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]

  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))

  id_winter <- static_prob_marginal %>%
    lapply(function(x) {
      difftime(metadata(x)$temporal_extent[2], metadata(x)$temporal_extent[1], units = "days") %>%
        as.numeric() > 50
    }) %>%
    unlist() %>%
    which() %>%
    nth(2)

  df <- static_prob_marginal[[id_winter]] %>%
    disaggregate(2, method = "bilinear") %>%
    as.data.frame(xy = TRUE) %>%
    mutate(layer = layer) %>%
    mutate(layer = ifelse(is.na(layer), 0, layer)) %>%
    arrange(desc(layer)) %>%
    mutate(layerP = 1 - cumsum(layer) / sum(layer))

  p <- p + new_scale_colour() +
    geom_contour(data = df, aes(x = x, y = y, z = layerP, color = ..level..), size = .9, breaks = c(.01)) +
    scale_colour_gradient(
      high = gpr$Color,
      low = "white",
      limits = c(0, .01),
      guide = "none"
    )
}
print(p)

plot_inset <- ggplot() +
  borders("world", colour = "gray90", fill = "gray50", size = 0.1) +
  coord_quickmap(
    # xlim=c(gpr$extent_W, gpr$extent_E),
    # ylim=c(gpr$extent_S, gpr$extent_N),
    xlim = c(-18, 51),
    ylim = c(-35, 37),
    expand = F
  ) +
  geom_rect(
    aes(
      xmin = layer_scales(p0)$x$range$range[1],
      xmax = layer_scales(p0)$x$range$range[2],
      ymin = layer_scales(p0)$y$range$range[1],
      ymax = layer_scales(p0)$y$range$range[2]
    ),
    color = "red", alpha = 0.1, size = 1
  ) +
  theme_map() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.background = element_rect(fill = "#FFFFFF")
  )

pf <- ggdraw() +
  draw_plot(p) +
  draw_plot(plot_inset,
    x = .72,
    y = .72,
    width = 0.25, height = .25
  )
pf

ggsave(plot = pf, "reports/figure_print/wintering_location.png")
ggsave(plot = pf, "reports/figure_print/wintering_location.eps", device = "eps")









# Cumulative flight duration ----

d <- list()
for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]
  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  load(paste0("data/3_static/", gdl, "_static_prob.Rdata"))

  tmp <- lapply(static_prob, function(x) {
    mt <- metadata(x)
    if (is.null(mt$flight)) {
      f <- 0
    } else {
      f <- as.numeric(sum(difftime(mt$flight$end, mt$flight$start, units = "hours")))
    }
    df <- data.frame(
      start = mt$temporal_extent[1],
      end = mt$temporal_extent[2],
      flight = f
    )
  })

  tmp2 <- do.call("rbind", tmp)

  id <- month(tmp2$end) < 8 & month(tmp2$end) > 1
  tmp2$flight[id] <- -tmp2$flight[id]

  d[[i]] <- data.frame(
    x = c(tmp2$start, tail(tmp2$end, 1)),
    y = c(0, cumsum(tmp2$flight)),
    gdl = gdl,
    color = darken(gpr$Color, 0.2)
  )
}
d2 <- do.call("rbind", d)

l <- d2 %>%
  summarise(color, gdl) %>%
  unique()

# Set on cst time
d2$x[d2$gdl %in% c("28AD","26YD","28BP")] <- d2$x[d2$gdl %in% c("28AD","26YD","28BP")]-years(1)

d2 %>%
  ggplot(aes(x = x, y = y, color = color, group = gdl)) +
  geom_step(size = 1) +
  scale_color_identity(
    name = "Track",
    labels = l$gdl,
    breaks = l$color,
    guide = "legend"
  ) +
  scale_x_datetime(date_breaks = "1 month", minor_breaks = NULL, date_labels = "%b") +
  scale_y_continuous(breaks = seq(0, 210, by = 20)) +
  coord_cartesian(
    xlim = as.POSIXct(c("2020-10-01 UTC", "2021-7-1 UTC"))
  ) +
  ylab("Cumulative Hours of flight") +
  xlab("Date") +
  theme_bw() +
  theme(
    legend.position = c(.5, .1),
    legend.box.background = element_rect(colour = "black"),
    legend.direction = "horizontal"
  )

ggsave("reports/figure_print/cumulative_flight.png", width = 8, height = 4)
ggsave("reports/figure_print/cumulative_flight.eps", device = "eps", width = 8, height = 4)











# Map of trajectory ----
e <- extent(static_prob_marginal[[1]])

p0 <- map_data("world") %>%
  filter(region %in% c("Kenya", "Tanzania", "Mozambique", "Malawi", "Uganda", "Somalia")) %>%
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "black", colour = "grey50") +
  theme_void() +
  coord_cartesian(xlim = c(e[1]+3, e[2]), ylim = c(e[3]+4, e[4]-2)) +
  theme(panel.background = element_rect(fill = "grey10"))

for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]
  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))

  p <- p0
  for (i_s in seq_len(length(static_prob_marginal))) {
    d <- as.data.frame(static_prob_marginal[[i_s]], xy = T) %>%
      mutate(layer = ifelse(is.na(layer), 0, layer))


    p <- p +
      geom_tile(data = d, aes(x, y, alpha = layer^3), fill = colour("batlow")(length(static_prob_marginal))[i_s]) + # brewer.pal(8,"Dark2")[i_s %% 7])  +
      scale_alpha(range = c(0, 1)) + new_scale("alpha")
  }

  sp <- shortest_path %>%
    as.data.frame() %>%
    left_join(pam$sta, by = "sta_id") %>%
    mutate(
      col = colour("batlow")(length(static_prob_marginal))[sta_id],
      duration = as.numeric(difftime(end, start, units = "days"))
    )

  pf <- p + theme(legend.position = "none") +
    geom_path(data = sp, aes(lon, lat), colour = "white") +
    geom_point(data = sp, aes(lon, lat, fill = sta_id, size = duration^(0.3) * 10), pch = 21, colour = "white") +
    scale_fill_batlow()
    # coord_map(xlim=e[c(1,2)], ylim = e[c(3,4)])


  ggsave(plot = pf, paste0("reports/figure_print/trajectory_", gdl, ".png"), width = 4, height = 8)
  # ggsave(plot = pf, paste0("reports/figure_print/trajectory_", gdl, "eps"), device = "eps", width = 8, height = 4)

}

























# Animated map of uncertainty ----

p0 <- get_googlemap(
  center = c(
    lon = mean(c(gpr$extent_W, gpr$extent_E)),
    lat = mean(c(gpr$extent_N, gpr$extent_S))
  ),
  zoom = 4, maptype = "satellite"
) %>% ggmap() +
  borders("world", colour = "gray90", size = 0.1) +
  coord_quickmap(
    xlim = c(gpr$extent_W, gpr$extent_E),
    ylim = c(gpr$extent_S, gpr$extent_N),
    expand = F
  ) +
  theme_map() +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]

  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))


  dfr <- do.call(rbind, lapply(static_prob_marginal, function(x) {
    mt <- metadata(x)
    as.data.frame(x, xy = TRUE) %>%
      filter(!is.na(layer) & layer > 0) %>%
      mutate(
        layer = layer / max(layer),
        sta_id = mt$sta_id,
        start = mt$temporal_extent[1],
        end = mt$temporal_extent[2],
        dur = as.numeric(difftime(mt$temporal_extent[2], mt$temporal_extent[1], units = "days"))
      )
  })) %>% mutate(
    sta_id <- as.factor(sta_id),
    rect_norm_start = as.numeric(difftime(start, min(start), units = "days")) / as.numeric(difftime(max(end), min(start), units = "days")),
    rect_norm_start = as.numeric(difftime(end, min(start), units = "days")) / as.numeric(difftime(max(end), min(start), units = "days"))
  )


  p <- p0 + geom_tile(data = dfr, aes(x = x, y = y, fill = (layer), group = seq_along(sta_id))) +
    scale_fill_gradient(
      high = gpr$Color,
      low = paste0(gpr$Color, "00"),
      limits = c(0, 1),
      guide = "none"
    ) +
    # + geom_rect(data=dfr, aes(xmin = gpr$extent_W, xmax = mean(gpr$extent_W+rect_norm_start), ymin = gpr$extent_S, ymax = gpr$extent_S+.1))
    # geom_text(data = dfr, aes(gpr$extent_W, gpr$extent_N, label=start),nudge_y=-1, nudge_x=.1, size = 8, hjust = 0, color = gpr$Color) +
    transition_states(sta_id) +
    enter_fade() +
    exit_shrink() +
    ggtitle("Stationary period {closest_state}")

  animate(p, height = 800, width = 600)
  anim_save(paste0("reports/figure_print/marginal_animation_", gdl, ".gif"))
}












# moveVis ----
d <- list()
for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]

  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))

  d[[i]] <- path2df(pam, shortest_path) %>%
    mutate(color = gpr$Color)
}

d <- do.call("rbind", d)
year(d$time[month(d$time) < 8]) <- 2000
year(d$time[month(d$time) >= 8]) <- 1999

d2 <- df2move(d, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
  x = "lon", y = "lat", time = "time", track_id = "track_id"
) %>%
  align_move(res = 24, unit = "hours", digit = 12) %>% # Use midday position rather than midnight (while the bird could be flying)
  subset_move(from = "1999-10-1", to = "2000-7-1") # remove the long equipement and retrival period duration

frames <- d2 %>%
  frames_spatial(
    equidistant = T,
    path_colours = unique(d$color), trace_colour = unique(d$color),
    map_service = "mapbox", map_type = "satellite", map_token = "pk.eyJ1IjoicmFmbnVzcyIsImEiOiIzMVE1dnc0In0.3FNMKIlQ_afYktqki-6m0g",
    ext = extent(static_prob[[1]])
  ) %>% #
  add_labels(x = NULL, y = NULL) %>% # add some customizations, such as axis labels
  add_timestamps(type = "label", size = 6) %>%
  add_progress()

animate_frames(frames, out_file = paste0("reports/figure_print/movevis3.gif"), height = 1000, width = 1000, overwrite = T)





# Simulation ----
d <- list()
for (i in seq(1, length(gdl_list))) {
  gdl <- gdl_list[i]

  load(paste0("data/1_pressure/", gdl, "_pressure_prob.Rdata"))
  load(paste0("data/5_wind_graph/", gdl, "_wind_graph.Rdata"))

  path_sim2 <- path_sim
  path_sim2$lat <- path_sim$lat[c(1, 2), ]
  path_sim2$lon <- path_sim$lon[c(1, 2), ]

  d <- path2df(pam, path_sim) %>%
    mutate(color = gpr$Color)

  d2 <- df2move(d, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
    x = "lon", y = "lat", time = "time", track_id = "track_id"
  ) %>%
    align_move(res = 24, unit = "hours", digit = 12) %>% # Use midday position rather than midnight (while the bird could be flying)
    subset_move(from = pam$sta$end[1] - 3 * 60 * 60 * 24, to = tail(pam$sta$start, 1) + 0 * 60 * 60 * 24) # remove the long equipement and retrival period duration

  frames <- d2 %>%
    frames_spatial(
      path_colours = rep(gpr$Color, nrow(path_sim$lat)), path_alpha = 0.4,
      tail_size = .5, path_size = 1, tail_colour = rep(gpr$Color, nrow(path_sim$lat)), path_legend = FALSE,
      ext = extent(static_prob[[1]])
    ) %>% #
    add_labels(x = NULL, y = NULL) %>% # add some customizations, such as axis labels
    add_timestamps(type = "label", size = 4) %>%
    add_progress()

  animate_frames(frames, out_file = paste0("reports/figure_print/movevis4.gif"), height = 1000, width = 1000, overwrite = T)
}
