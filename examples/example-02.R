library(sfe)

labels <- c("tokyo", "mx_city", "lagos", "london", "sao_paulo", "seoul", "edmonton")
lng <- c(139.6922, -99.1333, 3.3841, -0.1252, -46.6333, 126.9667, -113.4903)
lat <- c(35.6897, 19.4333, 6.4550, 51.5089, -23.5500, 37.5667, 53.5344)

true.loc <- true_locations(labels, lng, lat, crs = 4326)

file <- "/home/matt/git-repos/mapsurvey/example-data/wc-test/158102034627206-fop.csv"
file <- "/home/matt/git-repos/mapsurvey/example-data/wc-test/158099927987820-rqw.csv"
dsn <- "/home/matt/git-repos/mapsurvey/example-data/wc-test"

df.tmp <- load_one_response(file, labels)
df <- point_features(dsn, true.loc, labels)
