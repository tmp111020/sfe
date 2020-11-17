library(sf)
library(dplyr)
library(sfe)

polygons <- load_polygons("/home/matt/git-repos/mapsurvey/example-data/prm-test/")

## TODO document data
usethis::use_data(polygons, overwrite = TRUE)
