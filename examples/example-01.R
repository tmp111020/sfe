library(sfe)
data(polygons)

## create features
poly_data <- poly_features(polygons)

plot(states$geometry)
plot(poly_data$geometry, add = TRUE)

## view features
poly_data
