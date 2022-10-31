library(tidyverse)
library(readxl)


## yearly average of wind

## equipement site timeseire of rain/temperature

gdl_list <- read_excel("data/gpr_settings.xlsx") %>%
  filter(keep) %>%
  .$gdl_id

i <- 1

gdl <- gdl_list[i]

gpr <- read_excel("data/gpr_settings.xlsx") %>%
  filter(gdl_id == gdl)


request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("100m_u_component_of_wind", "100m_v_component_of_wind", "2m_temperature", "mean_total_precipitation_rate", "total_precipitation"),
  year = c("2021","2022"),
  month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c( gpr$calib_lat,  gpr$calib_lon, gpr$calib_lat,  gpr$calib_lon),
  dataset_short_name = "reanalysis-era5-single-levels",
  target = "download.nc"
)
ecmwfr::wf_set_key(user = Sys.getenv("cds_user"), key = Sys.getenv("cds_key"), service = "cds")
req <- wf_request(
  request,
  transfer = FALSE,
  path = ".",
  verbose = TRUE
)



request <- list(
  format = "netcdf",
  product_type = "monthly_averaged_reanalysis",
  variable = c("10m_u_component_of_wind", "10m_v_component_of_wind", "2m_temperature", "total_precipitation"),
  year = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"),
  month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  time = "00:00",
  area = c( gpr$calib_lat,  gpr$calib_lon, gpr$calib_lat,  gpr$calib_lon),
  dataset_short_name = "reanalysis-era5-single-levels-monthly-means",
  target = "monthly.nc"
)

re2q <- wf_request(
  request,
  transfer = FALSE,
  path = ".",
  verbose = TRUE
)





nc <- ncdf4::nc_open("data/condition_at_departure.nc")
cond <- data.frame(
  u100 = ncdf4::ncvar_get(nc, "u100"),
  v100 = ncdf4::ncvar_get(nc, "v100"),
  t2m = ncdf4::ncvar_get(nc, "t2m"),
  mtpr = ncdf4::ncvar_get(nc, "mtpr"),
  tp = ncdf4::ncvar_get(nc, "tp"),
  time = as.POSIXct(ncdf4::ncvar_get(nc, "time") * 60 * 60,
                    origin = "1900-01-01", tz = "UTC"
  )
)

ggplot(cond) +
  geom_line(aes(x=time,y=t2m))
