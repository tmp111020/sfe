#' Count number of points comprising polygon
#'
#' @usage num_points(x)
#' @param x sf object
#' @return int
#'
num_points <- function(x) {
  num_points_fn <- function(x) {
    x %>%
      st_coordinates() %>%
      data.frame() %>%
      nrow()
  }

  pts <- sapply(x$geometry, num_points_fn)
  return(pts)
}

read_and_clean <- function(file.name) {
  x <- st_read(file.name, type = 6, quiet = TRUE) %>%
    ## filter out instances where there are fewer than three points; will cause error
    filter(sfe:::num_points(.) > 3) %>%
    ## fix malformed (overlapping?) geomtries
    st_make_valid() %>%
    ## dissolve features
    st_union() %>%
    ## get collections to more primitive geometry types
    st_collection_extract() %>%
    ## dissolve again for those collections which have multiple features
    st_union() %>%
    ## from geometry set back to sf/dataframe (dplyr::distinct needs a dataframe, can't use sf)
    st_sf()

  return(x)
}

#' Load polygon data as single sf dataset
#'
#' @param dsn data source directory
#' @export
#'
load_polygons <- function(dsn) {
  file.list <- list.files(pattern = "*.geojson",
                          path = dsn,
                          full.names = TRUE)

  ## loop through file list, getting geometry and file name as variable
  for (i in 1:length(file.list)) {
    if (i == 1) {
      poly <- read_and_clean(file.list[i])
      poly$file <- basename(file.list[i])
    } else {
      tmp.poly <- read_and_clean(file.list[i])
      ## avoid empty geometries
      if (nrow(tmp.poly) != 0) {
        tmp.poly$file <- basename(file.list[i])
        poly <- rbind(poly, tmp.poly)
      }
    }
  }

  ## remove duplicates here, convert from collections
  poly.unique <- poly %>%
    ## this will convert polygon to multipolygon
    st_collection_extract() %>%
    ## remove file name to get rid of duplicates
    dplyr::distinct(., geometry, .keep_all = TRUE)

  return(poly.unique)
}

#' Compute centroid in one dimension (X or Y)
#'
#' @param x sf object
#' @param dim dimension, 'X' or 'Y'
#' @param crs coordinate reference system (CRS) to use for geometric operations.
#'   Necessary when using a geographic CRS; can be an epsg code
#' @return numeric of centroid in one dimension
#'
cent <- function(x, dim, crs = NA) {
  if (dim != "X" & dim != "Y") stop("'dim' must be 'X' or 'Y'")
  ## if no crs, use cartesian coordinates and avoid warnings
  if (!is.na(crs)) {
    orig.crs <- sf::st_crs(x)
    x.trans <- sf::st_transform(x, crs)
    cent_dim <- sf::st_centroid(x.trans) %>%
      sf::st_transform(., orig.crs) %>%
      sf::st_coordinates() %>%
      data.frame() %>%
      pull(dim)
  } else {
    x <- st_set_crs(x, NA)
    cent_dim <- sf::st_centroid(x) %>%
      sf::st_coordinates() %>%
      data.frame() %>%
      pull(dim)
  }
  return(cent_dim)
}

#' Used to get the min/max of x/y dimension of polygon
#' 
#' @param x sf object
#' @param dim dimension, 'X' or 'Y'
#' @param type type of computation, 'min' or 'max'
#'
bound <- function(x, dim, type) {
  bound_fn <- function(x, dim, type) {
    x %>%
      st_coordinates() %>%
      data.frame() %>%
      pull(dim) %>%
      purrr::exec(type, .)
  }

  bound.var <- sapply(x$geometry, bound_fn, type = type, dim = dim)
  return(bound.var)
}

diff_geo_x <- function(x, crs = 4326) {
  x$min_x <- bound(x, "X", "min")
  x$max_x <- bound(x, "X", "max")

  p1 <- data.frame(x = x$min_x, y = 0) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)

  p2 <- data.frame(x = x$max_x, y = 0) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)

  return(st_distance(p1, p2, by_element = TRUE))
}

diff_geo_y <- function(x, crs = 4326) {
  x$min_y <- bound(x, "Y", "min")
  x$max_y <- bound(x, "Y", "max")

  p1 <- data.frame(x = 0, y = x$min_y) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)

  p2 <- data.frame(x = 0, y = x$max_y) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)

  return(st_distance(p1, p2, by_element = TRUE))
}


#' Get polygon area
#'
#' @param x sf object
#' @param crs crs code (numeric) to use for area calculation
#'
area <- function(x, crs = NA) {
  area <- sf::st_area(x)
  return(area)
}

## TODO remove warnings about st_centroid and CRS
## TODO figure out if crs matters (based on warning) -> idea: remove crs to
## prevent warning. it actually makes sense to use the default CRS here instead
## of transforming, otherwise intersections that don't exist on a survey will
## potentially be there in the calculation? one caveat is if someone asks users
## to complete a survey in something other than epsg 4326 (not likely for web
## mapping, but still)
state_int_per <- function(x, crs = NA) {
  if (!is.na(crs)) {
    x <- x %>% st_transform(crs)
    states <- states %>% st_transform(crs)
  } else {
    x <- st_set_crs(x, NA)
    states <- st_set_crs(states, NA)
  }

  ## set column value to zero
  states$area_int <- 0

  ## find states that actually intersect
  int_cond <- st_intersects(states, x, sparse = FALSE)

  ## create empty matrix
  mat <- matrix(0, nrow = nrow(states), ncol = nrow(x))

  ## only update rows where the state intersection is there
  for (i in 1:ncol(mat)) {
    mat[,i][which(int_cond[,i])] <- st_intersection(x[i,], states) %>% st_area %>% as.numeric
  }

  # create matrix of state areas
  state_area_mat <- matrix(rep(st_area(states), nrow(x)), nrow = nrow(states), ncol = nrow(x))

  ## get area of each state, transpose matrix
  df <- data.frame(t(mat / state_area_mat))

  ## get the percentage intersection
  #states$int_per <- states$area_int / states$area

  #df <- data.frame(matrix(data = states$int_per, ncol = 49, byrow = TRUE))
  colnames(df) <- paste0(sfe::states$STUSPS, "_int_per")

  return(df)
}

state_cent_int <- function(x, crs = NA) {
  if (!is.na(crs)) {
    x <- x %>% st_transform(crs)
    states <- states %>% st_transform(crs)
  } else {
    x <- st_set_crs(x, NA)
    states <- st_set_crs(states, NA)
  }

  ## get centroids
  cents <- st_centroid(states, crs)

  int <- st_intersects(cents, x, sparse = FALSE) %>%
    as.numeric()

  df <- data.frame(matrix(data = int, ncol = 49, byrow = TRUE))
  colnames(df) <- paste0(sfe::states$STUSPS, "_cent")

  return(df)
}

historic_def_int_per <- function(x, historic_definition) {

  vals <- c()

  historic_definition <- st_transform(historic_definition, st_crs(x))
  x <- st_set_crs(x, NA)
  historic_definition <- st_set_crs(historic_definition, NA)

  ## this ought to be vectorized but doing so with spatial operations in this
  ## way is challenging
  for (i in 1:nrow(x)) {

    ## get the intersection between the user response and the historic definition
    x_int_historic <- st_intersection(x[i,], historic_definition)

    if (length(st_area(x_int_historic)) == 0) {

      percent_overlap <- 0

    } else {

      historic_area <- st_area(historic_definition)
      x_int_historic_area <- st_area(x_int_historic)
      percent_overlap <- x_int_historic_area / historic_area
    }

    vals <- c(vals, percent_overlap)

  }

  return(vals)
}

great_lakes_sts_int <- function(x, crs = NA) {

  ## get just great lakes states
  great.lakes.states <- c("Minnesota", "Wisconsin", "Michigan",
                          "Illinois", "Indiana", "Ohio", "Pennsylvania",
                          "New York")
  states.sub <- states %>%
    filter(NAME %in% great.lakes.states)

  if (!is.na(crs)) {
    x <- x %>% st_transform(crs)
    states.sub <- states.sub %>% st_transform(crs)
  } else {
    x <- st_set_crs(x, NA)
    states.sub <- st_set_crs(states.sub, NA)
  }

  int <- st_intersects(states.sub, x, sparse = FALSE) %>%
    colSums()

  return(int)
}

big_ten_cities_int <- function(x, crs = NA) {
  if (!is.na(crs)) {
    x <- x %>% st_transform(crs)
    big.ten.cities <- big.ten.cities %>% st_transform(crs)
  } else {
    x <- st_set_crs(x, NA)
    big.ten.cities <- st_set_crs(big.ten.cities, NA)
  }

  int <- st_intersects(big.ten.cities, x, sparse = FALSE) %>%
    colSums()
}

nw_angle_int <- function(x, crs = NA) {
  ## define nw angle as sf object
  nw.angle <- data.frame(lng = -94.957,
                         lat = 49.242) %>%
    mutate(x = lng,
           y = lat) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)

  if (!is.na(crs)) {
    x <- x %>% st_transform(crs)
    nw.angle <- nw.angle %>% st_transform(crs)
  } else {
    x <- st_set_crs(x, NA)
    nw.angle <- st_set_crs(nw.angle, NA)
  }

  int <- st_intersects(nw.angle, x, sparse = FALSE) %>%
    as.numeric()
  return(int)
}


#' Feature engineering for polygons
#'
#' @usage poly_features(x, crs)
#' @param x sf object
#' @param crs crs code (numeric) to use for geometric operations
#' @return sf object with original columns and newly created columns
#' @export
#'
poly_features <- function(x, crs = NA, state_intersect = TRUE, state_cent_intersect = TRUE, historic_def_int_per = TRUE, great_lakes_intersect = TRUE, big_ten_cities = TRUE) {
  x$cent_x <- cent(x, "X", crs)
  x$cent_y <- cent(x, "Y", crs)
  x$area <- area(x, crs)
  x$min_x <- bound(x, "X", "min")
  x$max_x <- bound(x, "X", "max")
  x$min_y <- bound(x, "Y", "min")
  x$max_y <- bound(x, "Y", "max")
  x$diff_planar_x <- x$max_x - x$min_x
  x$diff_planar_y <- x$max_y - x$min_y
  x$diff_geo_x <- diff_geo_x(x)
  x$diff_geo_y <- diff_geo_y(x)
  x$xy_ratio_planar <- x$diff_planar_x / x$diff_planar_y
  x$xy_ratio_geo <- x$diff_geo_x / x$diff_geo_y
  x$num_points <- num_points(x)
  x$points_area_ratio <- x$num_points / x$area

  ## US state intersections
  if (state_intersect) {
    state_int_per <- state_int_per(x)
    x <- cbind(x, state_int_per)
  }

  ## US state centroid intersections
  if (state_cent_intersect) {
    state_cent_int <- state_cent_int(x)
    x <- cbind(x, state_cent_int)
  }

  ## historic definition intersections
  if (historic_def_int_per) {
    x$shortrige_int_per <- historic_def_int_per(x, shortridge[4,])
    x$census_def_int_per <- historic_def_int_per(x, census_def)
  }

  ## number of great lakes states intersected
  if (great_lakes_intersect) {
    x$great_lakes_states <- great_lakes_sts_int(x)
  }

  ## number of big ten cities intersected
  if (big_ten_cities) {
    x$big_ten_cities <- big_ten_cities_int(x)
  }

  return(x)
}
