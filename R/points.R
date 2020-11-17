#' Create sf object of true locations based on labels, x, and y
#'
#' @param labels character vector of short labels to use for locations; needed
#'   for generating succinct column names for features
#' @param x longitude coordinates of true locations (numeric vector)
#' @param y latitude coordinates of true locations (numeric vector)
#' @param crs coordinate reference system to use for distance calculations
#' @export
#'
true_locations <- function(labels, lng, lat, crs = 4326) {
  true.loc <- data.frame(city = labels,
                         lng = lng,
                         lat = lat) %>%
    mutate(x = lng,
           y = lat) %>%
    st_as_sf(coords = c("x", "y"), crs = crs)
}

#' function for testing and getting colnames needed for labels
#'
#' @param file path to file as character string
#' @export
#'
load_one_response <- function(file, labels, crs = 4326) {
  ## check to be sure values are within appropriate range
  readr::read_csv(file) %>% mutate(lng = ifelse(abs(lng) < 180 & abs(lat) < 90, yes = lng, no = NA),
                                   lat = ifelse(abs(lng) < 180 & abs(lat) < 90, yes = lat, no = NA)) %>%
    mutate(lab = labels,
           x = lng,
           y = lat) %>%
    st_as_sf(coords = c("x", "y"), na.fail = FALSE) %>%
    st_set_crs(crs)
}

#' function for getting planar distance
#'
get_dist_planar <- function(p1, p2) {
  st_crs(p1) <- NA
  st_crs(p2) <- NA

  dist_planar <- st_distance(p1, p2, by_element = TRUE)
  return(dist_planar)
}

#' function for getting geodesic distance
#'
#' @param p1 set of points of class sf
#' @param p2 set of points of class sf
#'
get_dist_geo <- function(p1, p2) {
  dist_geo <- st_distance(p1, p2, by_element = TRUE)
  return(dist_geo)
}

#' Get bearing (angle) between -180 and 180 from one set of points (i.e., responses) to another (i.e. actual locations)
#'
#' @param set1 sf object of locations; must have 'lng' and 'lat' columns
#' @param set2 sf object of locations; must have 'lng' and 'lat' columns
#'
get_bearing <- function(set1, set2) {
  ## set geometry to NULL so that these are just dataframes; apply doesn't like sf much
  st_geometry(set1) <- NULL
  st_geometry(set2) <- NULL

  bearings <- apply(cbind(set1[c('lng', 'lat')], set2[c('lng', 'lat')]), 1, function(y) geosphere::bearing(c(y[1],y[2]), c(y[3],y[4])))
  return(bearings)
}


#' function for making data wide, getting x/y as columns specific to each location
#'
to_wide <- function(df, labels) {
  long.labs <- c(paste0(labels, "_x"),
                 paste0(labels, "_y"),
                 paste0(labels, "_dist_geo"),
                 paste0(labels, "_dist_planar"),
                 paste0(labels, "_bearing"))

  row <- matrix(c(df$lng, df$lat, df$dist_geo, df$dist_planar, df$bearing), nrow = 1) %>%
    data.frame() %>%
    magrittr::set_colnames(long.labs) %>%
    select(sort(names(.)))

  return(row)
}

#' Create features from one user response
#'
#' @param file path to file as character string
#' @param true.loc sf object of true locations
#' @param labels character vector of short labels to use for locations; needed
#'   for generating succinct column names for features
#' @param crs coordinate reference system to use for distance calculations
#'
gen_features <- function(file, true.loc, labels, crs = 4326) {
  df <- load_one_response(file, labels)
  ## if all are empty return nothing, otherwise get a bunch of variables and rbind
  if (sum(st_is_empty(df)) == nrow(true.loc)) {
    result <- NULL
  } else {
    df$dist_geo <- get_dist_geo(df, true.loc)
    df$dist_planar <- get_dist_planar(df, true.loc)
    df$bearing <- get_bearing(df, true.loc)

    ## make data wide here
    df.row <- to_wide(df, labels)

    ## summaries of distance results
    df.row$mean_dist_planar <- mean(df$dist_planar, na.rm = TRUE)
    df.row$med_dist_planar <- median(df$dist_planar, na.rm = TRUE)
    df.row$min_dist_planar <- min(df$dist_planar, na.rm = TRUE)
    df.row$max_dist_planar <- max(df$dist_planar, na.rm = TRUE)
    df.row$sd_dist_planar <- sd_pop(df$dist_planar)

    df.row$mean_dist_geo <- mean(df$dist_geo, na.rm = TRUE)
    df.row$med_dist_geo <- median(df$dist_geo, na.rm = TRUE)
    df.row$min_dist_geo <- min(df$dist_geo, na.rm = TRUE)
    df.row$max_dist_geo <- max(df$dist_geo, na.rm = TRUE)
    df.row$sd_dist_geo <- sd_pop(df$dist_geo)

    ## get text of min/max
    df.row$min_dist_planar_city <- df$lab[match(min(df$dist_planar, na.rm = TRUE), df$dist_planar)]
    df.row$max_dist_planar_city <- df$lab[match(min(df$dist_planar, na.rm = TRUE), df$dist_planar)]
    df.row$min_dist_geo_city <- df$lab[match(min(df$dist_geo, na.rm = TRUE), df$dist_geo)]
    df.row$max_dist_geo_city <- df$lab[match(min(df$dist_geo, na.rm = TRUE), df$dist_geo)]

    result <- df.row
  }
  return(result)
}

#' Create features from a directory of points
#'
#' @param dsn character string of data directory from where .csv's are stored
#' @param true.loc sf object preferably generated from the function
#'   true_locations, used for calculating distance from user response to the
#'   true locations
#' @param labels character vector of short labels to use for locations; needed
#'   for generating succinct column names for features
#' @param crs coordinate reference system to use for distance calculations
#' @export
#'
point_features <- function(dsn, true.loc, labels, crs = 4326) {
  files <- list.files(dsn, pattern = "*.csv", full.names = TRUE)
  for (i in 1:length(files)) {
    if (i == 1) {
      df <- gen_features(files[i], true.loc, labels, crs = 4326)
    } else {
      row <- gen_features(files[i], true.loc, labels, crs = 4326)
      df <- rbind(df, row)
    }
  }
  return(df)
}
