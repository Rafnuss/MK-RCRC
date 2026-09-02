### NEW PROJECT YEAR

library(tidyverse)
library(GeoLocatoR)
library(readxl)

# Read the Access database
# gdl0 <- read_soi_gld(access_file = "../../40 Database/GDL_Data.accdb", filter_col = FALSE)
gdl0 <- read_soi_gld(
  access_file = "C:/Users/jwo/Documents/DP tables/GDL_Data.accdb",
  filter_col = FALSE
)


gdl <- gdl0 %>% filter(stringr::str_detect(OrderName, "GeoGut"))

pkg <- create_gldp()
