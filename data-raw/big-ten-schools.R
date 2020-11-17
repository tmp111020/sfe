library(sf)
library(dplyr)
library(here)
library(USAboundaries)

big.ten.cities <- us_cities() %>%
  filter((city == "Madison" & state_name == "Wisconsin") |
         (city == "Bloomington" & state_name == "Indiana") |
         (city == "Ann Arbor" & state_name == "Michigan") |
         (city == "East Lansing" & state_name == "Michigan") |
         (city == "Columbus" & state_name == "Ohio") |
         (city == "College Park" & state_abbr == "MD") |
         (city == "State College" & state_name == "Pennsylvania") |
         (city == "New Brunswick" & state_abbr == "NJ") |
         (city == "Champaign" & state_abbr == "IL") |
         (city == "Iowa City" & state_abbr == "IA") |
         (city == "Minneapolis" & state_abbr == "MN") |
         (city == "Lincoln" & state_abbr == "NE") |
         (city == "Evanston" & state_abbr == "IL") |
         (city == "West Lafayette" & state_abbr == "IN"))

st_write(big.ten.cities, here("data-raw/big-ten-cities.shp"), delete_layer = TRUE)
usethis::use_data(big.ten.cities, overwrite = TRUE)

## TODO make function for old big ten schools?
