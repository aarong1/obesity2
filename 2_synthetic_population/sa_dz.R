# sa_dz.R
library(sf)
cent_sa <- read_csv("data/Small Area population weighted centroids.csv")
cent_dz <- read_csv("data/geography-census-2021-population-weighted-centroids-csv/census-2021-population-weighted-centroids-data-zone.csv")

sa_points <- st_as_sf(cent_sa, coords = c("X Coordinate", "Y Coordinate"), crs = 29903)
sa_points_wgs84 <- st_transform(sa_points, 4326)

dz_points <- st_as_sf(cent_dz, coords = c("X", "Y"), crs = 29903)
dz_points_wgs84 <- st_transform(dz_points, 4326)


dist_sa_dz <- st_distance(dz_points_wgs84,sa_points_wgs84)

dimnames(dist_sa_dz) <- list(dz_points_wgs84$DZ2021_code, sa_points_wgs84$SA2011)

closest_sa <- apply(dist_sa_dz, 1, function(x) which.min(x))
dist_closest_sa <- apply(dist_sa_dz, 1, function(x) min(x))

sa_dz_lookup <- data.frame(
    dz = dz_points_wgs84$DZ2021_code,
    sa = sa_points_wgs84$SA2011[closest_sa],
    dist = round(dist_closest_sa)
)

row.names(sa_dz_lookup) <- NULL

sa_dz_lookup <- as_tibble(sa_dz_lookup)

write_fst(sa_dz_lookup, './synthetic_population/lookup_sa_dz.fst') 

 # dz_shape <- st_read("./data/geography/geography-dz2021-esri-shapefile")
 # dz_shape$area <- st_area(dz_shape)

dz_shape_wgs84 <- st_transform(dz_shape, 4326)

leaflet(dz_shape_wgs84) |> 
  # addProviderTiles("CartoDB.Voyager") |>   # clean white/light base
  addProviderTiles("CartoDB.Positron") |> 
  addPolygons(  color = 'mediumseagreen', 
                opacity = 1,
                stroke = T,
                fillOpacity = 0
                   ) |> 
  addCircles( data = sa_points_wgs84,
              color = 'rgb(255,201,41)',
              fillOpacity = 1,
              stroke = F,
              radius = 30
             #fillOpacity = 1
             )
