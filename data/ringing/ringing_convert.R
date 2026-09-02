library(tidyverse)
library(readxl)
library(GeoLocatoR)
library(stringr)

# Get SOI data for tag
# soi_folder <- '/Volumes/DFS/Daten/DOM_Forschung/UNIT_Vogelzug/40 Data/20 Geolocator' # macOS
soi_folder <- "/Users/rafnuss/Library/CloudStorage/OneDrive-Vogelwarte/2-geolocator_data/UNIT_Vogelzug/"

access_file <- file.path(soi_folder, "40 Database/GDL_Data.accdb")
directory_data <- file.path(soi_folder, "10 Raw data")

gdl0 <- read_soi_gld(
  access_file = access_file,
  filter_col = FALSE
)

gdl <- gdl0 |>
  filter(str_detect(OrderName, "CosNatKE") | str_detect(OrderName, "HalSenKE"))

pkg_soi <- read_soi(gdl, ".")

tags_soi <- tags(pkg_soi) |>
  select(-c(ring_number, scientific_name, tag_comments, readout_method))

# Get currated list of tags
t <- read_xlsx(
  "data/ringing/deployment_details_AllYrs.xlsx",
  sheet = "tags"
) %>%
  filter(deployement_year < 2026) |>
  select(
    tag_id,
    ring_number,
    scientific_name,
    tag_comments
  )

# check fro missing tag_id
tags_soi |> filter(!(tag_id %in% t$tag_id))

# Joint table
t <- left_join(t, tags_soi)

# filter tags that where not equiped:
t <- t |> filter(!is.na(ring_number) | ring_number == "" | ring_number == " ")


o <- read_xlsx(
  "data/ringing/deployment_details_AllYrs.xlsx",
  sheet = "observations"
) %>%
  filter(!is.na(ring_number)) |>
  transmute(
    ring_number,
    tag_id = `GDL Number`,
    observation_type,
    datetime = force_tz(
      Date +
        as.difftime(
          ifelse(is.na(Time) | Time > 1, 0, Time) * 86400,
          units = "secs"
        ),
      tzone = "Africa/Nairobi"
    ),
    latitude_tmp = longitude, ### NEED TO FIX THIS!
    longitude_tmp = latitude,
    location_name,
    device_status,
    observer = Initial,
    catching_method = "M",
    age_class = ifelse(is.na(`Age/Sex`), 0, `Age/Sex`),
    sex = "U",
    condition = ifelse(is.na(condition), "unknown", condition),
    mass = Mass,
    wing_length = Wing,
    additional_metric = glue::glue(
      "{{head:{Head}, tail:{Tail}, tarsus:{Tarsus}, primary_molt: {`Primary Moult`}, secondary_molt: {`Secondary Moult`}}"
    ),
    observation_comments = glue::glue(
      "{ifelse(Retrap, 'Retrap | ', '')}{Notes} | nets: {Net}"
    )
  ) %>%
  rename(
    latitude = latitude_tmp,
    longitude = longitude_tmp
  )

write.csv(o, "data/observations.csv", row.names = FALSE)
write.csv(t, "data/tags.csv", row.names = FALSE)
