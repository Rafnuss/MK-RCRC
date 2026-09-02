library(tidyverse)
library(glue)
library(purrr)
library(readr)
library(lubridate)

access_file <- "data/ringing/00_ARK_Ringing_DB_2025-06-05_LempiLesi.accdb"

system(glue::glue("mdb-tables '{access_file}'"))

tables <- c("Captures", "Sessions", "Species2", "Localities", "Ringer list")

tbl_list <- map(set_names(tables), function(ta) {
  file_name <- tempfile(gsub("[^A-Za-z0-9]", "_", ta), fileext = ".csv")
  system(glue("mdb-export '{access_file}' '{ta}' > {file_name}"))
  read_csv(file_name, show_col_types = FALSE)
})

d <- tbl_list$Captures %>%
  left_join(tbl_list$Sessions, by = "SessionID") %>%
  left_join(tbl_list$Species2, by = c("SpeciesID" = "Species#")) %>%
  left_join(tbl_list$Localities, by = "LocalityID") %>%
  left_join(tbl_list$`Ringer list`, by = c("Initials" = "RingerInitials")) %>%
  select(
    Date,
    Location,
    Latitude,
    Longitude,
    Habitat,
    Altitude,
    Country,
    NettingSite,
    RingNo,
    Age,
    Sex,
    Wing,
    Head,
    Tarsus,
    Weight,
    Time,
    Initials,
    RingerName,
    NetNo,
    Notes,
    ColourRing,
    Mark,
    Condition,
    CommonName,
    ScientificName
  ) %>%
  mutate(Date = as.Date(mdy_hms(Date)), Time = format(mdy_hms(Time), "%H:%M"))


## Filter/Search

d %>%
  # filter(RingNo=="bb6492")
  filter(grepl("^AA44800.*$", RingNo))
# filter(Date=="07/21/21 08.07.2021")

d %>%
  filter(if_any(
    everything(),
    ~ str_detect(., regex("AA44800", ignore_case = TRUE))
  ))
