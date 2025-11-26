# Extract summury result of the birds trajectory
# - pml: path most likely: information at the stationary period information
# - eml: edge most likely: imformation at the level of each flight
# - journey: information at the level of pre-breeding, post-breeding, non-breeding, breeding level.


library(tidyverse)
library(GeoPressureR)

# Define tag id that you want to compute
list_id <- tail(names(yaml::yaml.load_file("config.yml", eval.expr = FALSE)), -1)
list_id <- c("24UL", "28AD", "26YD", "28BP", "24TA", "30HQ", "30HZ", "30IJ", "30JS")

EMl <- list()
PMl <- list()
JOURNEy <- list()

for (i in seq(1, length(list_id))) {

  # load data
  cli::cli_h2("Process tag {list_id[i]}")
  load(paste0("data/interim/", list_id[i], ".Rdata"))

  # Take the most likely path information and add more info
  pml <- path_most_likely
  pml$duration <- stap2duration(pml, return_numeric = F)

  istap_winter <- which(pml$duration > 18)[2]
  pml$status <- ""
  pml$status[seq(2, istap_winter - 1)] <- "post-breeding"
  pml$status[seq(istap_winter, nrow(pml))] <- "pre-breeding"
  pml$status[istap_winter] <- "non-breeding"
  pml$status[1] <- "breeding"
  pml$status[nrow(pml)] <- "breeding"

  # Keep id on the dataset
  pml$id <- list_id[i]
  pml$species <- config::get("scientific_name", list_id[i])

  # flight data from edges
  eml <- edge_most_likely
  eml$id <- list_id[i]
  eml$species <- config::get("scientific_name", list_id[i])

  # Add status + congo info
  eml$status <- ""
  eml$status[eml$stap_s < istap_winter] <- "post-breeding"
  eml$status[eml$stap_t > istap_winter] <- "pre-breeding"

  # Compute wind support
  eml$ws_support <- windsupport(eml$ws, eml$gs)
  eml$aws <- abs(eml$ws)
  eml$ags <- abs(eml$gs)
  eml$aas <- abs(eml$gs - eml$ws)

  journey <- pml %>%
    group_by(status) %>%
    summarise(
      start = first(start),
      end = last(end),
      duration = last(end) - first(start),
      stap_s = min(stap_id) - 1,
      stap_t = max(stap_id) + 1,
      n = n()
    ) %>%
    merge(
      eml %>%
        group_by(status) %>%
        summarise(
          flight_distance = sum(distance),
          flight_duration = sum(duration),
          flight_number = sum(n)
        ),
      by = "status"
    )

  # Add elevation to pressurepath
  elevation <- path2elevation(pml, scale = tag$param$scale)
  pressurepath$elevation <- approx(elevation$stap_id, elevation$X50, pressurepath$stap_id)$y

  # Altitude
  eml <- eml %>%
    merge(
      pressurepath %>%
        filter(stap_id != round(stap_id)) %>%
        mutate(stap_s = floor(stap_id)) %>%
        group_by(stap_s) %>%
        summarise(
          mean_alt_sl = mean(altitude),
          min_alt_sl = min(altitude),
          max_alt_sl = max(altitude),
          std_alt_sl = sd(altitude),
          mean_alt_gl = mean(altitude - elevation),
          min_alt_gl = min(altitude - elevation),
          max_alt_gl = max(altitude - elevation),
          std_alt_gl = sd(altitude - elevation),
        )
    )



  eml$tag_id <- tag$param$id
  pml$tag_id <- tag$param$id
  journey$tag_id <- tag$param$id

  EMl[[i]] <- eml
  PMl[[i]] <- pml
  JOURNEy[[i]] <- journey
}


EML <- do.call(rbind, EMl)
PML <- do.call(rbind, PMl)
JOURNEY <- do.call(rbind, JOURNEy)

write.csv(EML, file = "output/summary_flight.csv")
write.csv(PML, file = "output/summary_stap.csv")
write.csv(JOURNEY, file = "output/summary_journey.csv")
