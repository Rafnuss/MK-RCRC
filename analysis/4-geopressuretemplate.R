# See https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-workflow.html

library(GeoPressureR)
library(GeoLocatoR)
library(tidyverse)


## Run workflow for all tags
list_id <- config2tibble() |>
  filter(
    use &
      geopressuretemplate.likelihood != "map_light"
  ) |>
  pull(id)

# Manual checking of coherence
id = "16LF"
geopressureviz(id)

# Add wind
for (id in list_id) {
  cli::cli_h1("Run tag_download_wind for {id}")
  load(glue::glue("./data/interim/{id}.RData"))
  job::job({
    tag_download_wind(tag)
  })
}

# Run graph
for (id in list_id) {
  cli::cli_h1("Run graph for {id}")
  geopressuretemplate_graph(id)
}

# Run pressurepath
for (id in list_id) {
  cli::cli_h1("Run pressurepath for {id}")
  geopressuretemplate_pressurepath(id)
}
