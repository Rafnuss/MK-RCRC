### NEW PROJECT YEAR

library(tidyverse)
library(GeoLocatoR)
library(readxl)

# Read the Access database
# gdl0 <- read_soi_gld(access_file = "../../40 Database/GDL_Data.accdb", filter_col = FALSE)
gdl0 <- read_soi_gld(access_file = "C:/Users/jwo/Documents/DP tables/GDL_Data.accdb", filter_col = FALSE)


gdl <- gdl0 %>% filter(stringr::str_detect(OrderName, "GeoGut"))

pkg <- create_gldp()

pkg <- read_soi(
  gdl = gdl,
  directory_data = "C:/Users/jwo/OneDrive - Vogelwarte/VOWA_AG_Bird_Migration - 0_projects/GeoGut/data/raw-tag/",
  generate_observations = TRUE
)


# If you have additional information on the actual data
pkg <- create_gldp(
  title = "Short Distance Migration Of Spotted Ground Thrush (Geokichla Guttata Guttata) In Coastal Kwazulu-
Natal, South Africa",
  contributors = list(
    list(
      title = "Jonah Gula",
      path= "https://orcid.org/0000-0003-4922-6337",
      email = "jonah.gula@yahoo.com",
      roles = c("ContactPerson", "ProjectLeader", "DataCollector"),
      organization = "University of KwaZulu-Natal"
    ),
    list(
      title = "Nasiphi Bitani",
      path= "https://orcid.org/0000-0003-4729-5996",
      email = "bitanin@ukzn.ac.za",
      roles = c("DataCollector", "Researcher"),
      organization = "University of KwaZulu-Natal"
    ),
    list(
      title = "Colleen Downs",
      path= "https://orcid.org/0000-0001-8334-1510",
      email = "downs@ukzn.ac.za",
      roles = c("Supervisor"),
      organization = "University of KwaZulu-Natal"
    ),
    list(
      title = "Raphaël Nussbaumer",
      path= "https://orcid.org/0000-0002-8185-1020",
      email = "raphael.nussbaumer@vogelwarte.ch",
      roles = c("DataCurator"),
      organization = "Swiss Ornithological Institute"
    ),
    list(
      title = "Yann Rime",
      path= "https://orcid.org/0000-0003-2745-0557",
      email = "yann.rime@vogelwarte.ch",
      roles = c("Researcher"),
      organization = "Swiss Ornithological Institute"
    )
  ),
  embargo = "2030-01-01",
  licenses = list(list(name = "CC-BY-4.0", title = "Creative Commons Attribution 4.0",
                       path = "https://creativecommons.org/licenses/by/4.0/")),
  description = "This project aims to understand the migration of Spotted Ground Thrush wintering in Durban, South Africa. In June-July 2025, 10-15 birds will be fitted with geolocators at several forest patches in Durban. We aim to understand whether Durban's wintering thrush population migrates to breeding areas in southern KwaZulu-Natal/Eastern Cape, northern KwaZulu-Natal, or both."
  ) %>%
  read_soi(
    gdl = gdl,
    directory_data = ".",
    generate_observations = TRUE
  )

pkg$version = "0.1.0"

# Export all resources as csv
frictionless::write_package(pkg, pkg$version)

# Or excel
writexl::write_xlsx(tags(pkg), "tags.xlsx")
writexl::write_xlsx(observations(pkg), "observations.xlsx")



####### EXISTING/FOLLOWING PROJECT YEARS
library(GeoLocatoR)
library(dplyr)
library(readxl)
library(writexl)

old_version <- "0.4.0"
new_version <- "0.5.0"

out_dir <- file.path("data", "meta", new_version)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# Read previous years tags/obs xlsx
old_tags <- read_excel(glue::glue("data/meta/{old_version}/tags.xlsx"))
old_obs  <- read_excel(glue::glue("data/meta/{old_version}/observations.xlsx"))

# csv version
old_tags <- read.csv(
  glue::glue("data/meta/{old_version}/tags.csv"),
  stringsAsFactors = FALSE
)

old_obs <- read.csv(
  glue::glue("data/meta/{old_version}/observations.csv"),
  stringsAsFactors = FALSE
)

# Build package first from current raw tags/GDL database
gdl0 <- read_soi_gld(access_file = "C:/Users/jwo/Documents/DP tables/GDL_Data.accdb", filter_col = FALSE)

gdl <- gdl0 %>% filter(stringr::str_detect(OrderName, "CosNatKE"))

pkg <- create_gldp()

pkg <- read_soi(
  gdl = gdl,
  directory_data = "data/raw-tag/",
  generate_observations = TRUE
)

# Extract automatically generated tables for all current IDs
new_tags_generated <- tags(pkg)
new_obs_generated  <- observations(pkg)

# Keep old edited rows & append only IDs not already present
new_tags_only <- new_tags_generated %>%
  filter(!tag_id %in% old_tags$tag_id)

tags_merged <- bind_rows(old_tags, new_tags_only)

# Match by tag_id + observation_type is enough to make obs table
obs_key <- c("tag_id", "observation_type")

new_obs_only <- new_obs_generated %>%
  anti_join(old_obs, by = obs_key)

## only for the current csvs format
old_obs <- old_obs %>%
  mutate(
    datetime = suppressWarnings(lubridate::ymd_hms(datetime, tz = "UTC")),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    mass = as.numeric(mass),
    age_class = as.character(age_class),
    wing_length = as.numeric(wing_length)
  )
observations_merged <- bind_rows(old_obs, new_obs_only)


# Put merged tables back into pkg
tags(pkg) <- tags_merged
observations(pkg) <- observations_merged

pkg$version <- paste0(new_version)


# Write datapackage and new excel tables to be sent out
frictionless::write_package(pkg, directory = out_dir)


write_xlsx(tags(pkg), glue::glue("data/meta/{new_version}/tags.xlsx"))
write_xlsx(observations(pkg), glue::glue("data/meta/{new_version}/observations.xlsx"))

