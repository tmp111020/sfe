library(tigris)
library(dplyr)
library(here)

states <- states(cb = TRUE, class = "sf") %>%
  arrange(STATEFP) %>%
  slice(1:(n()-5)) %>%
  filter(STATEFP != '02' & STATEFP != '15') %>%
  st_transform(4326)

st_write(states, here("data-raw/states.shp"), delete_layer = TRUE)
usethis::use_data(states, overwrite = TRUE)
