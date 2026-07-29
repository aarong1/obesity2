library(sf)
library(tidyverse)
library(leaflet)

library(readr)

library(fst)

#---------------------------
# 0) Read + prepare
#---------------------------
# Replace with your file paths



cent_sdz <- read_csv("data/geography-census-2021-population-weighted-centroids-csv/census-2021-population-weighted-centroids-super-data-zone.csv")

# sdz_points <- st_as_sf(cent_sdz, coords = c("X", "Y"), crs = 29903)
# sdz_points_wgs84 <- st_transform(sf_points, 4326)

sdz <- st_read("./data/geography/geography-sdz2021-esri-shapefile")
sdz$area <- st_area(sdz)
soa <- st_read("./data/soa2011_shapefiles", quiet = TRUE)

# Use a projected CRS in metres (BNG works fine for NI)
# target_crs <- 27700
target_crs <- 4326  # Irish Transverse Mercator
soa <- soa %>% st_make_valid() %>% st_transform(target_crs)
sdz <- sdz %>% st_make_valid() %>% st_transform(target_crs)

# Make sure IDs exist
soa <- soa %>% rename(soa_id = any_of(c("SOA_CODE", "soa2011", "soa_id"))[1])
sdz <- sdz %>% rename(sdz_id = any_of(c("SDZ2021_cd", "DATAZONE21", "sdz_id"))[1])

# Optional: keep only geometry + id
# soa <- soa %>% select(soa_id, geometry)
# sdz <- sdz %>% select(sdz_id, geometry)


#---------------------------
# 0-1) Is in point geometry
#---------------------------
# sdz_is_in_soz <- st_within(  sdz_points_wgs84,
#             soa
# )
# 
# st_drop_geometry(sdz_points_wgs84) |> 
#   mutate(soa_id = soa[unlist(sdz_is_in_soz),]$soa_id )
#   
#   
# 
# 
# 
# leaflet() |>
#   addTiles() |>
#   addPolygons(data = soa,
#               color = "red",
#               weight = 1,
#               fillOpacity = 0.1,
#               label = ~soa_id) |>
#   addCircleMarkers(data = sdz_points_wgs84#,
#     
#     # color = "blue",
#     # label = ~id#,
#     # popup = ~paste("ID:", id,
#     #                "<br>Lon:", round(st_coordinates(geometry)[,1], 5),
#     #                "<br>Lat:", round(st_coordinates(geometry)[,2], 5))
#   )
#---------------------------
# 1) Intersection (filtered)
#---------------------------
# Pre-filter candidate pairs to speed up st_intersection
cand <- st_intersects(soa, sdz)          # list: for each soa, which sdzs touch
pairs <- map2_dfr(seq_len(nrow(soa)), cand, ~{
  if (length(.y) == 0) return(tibble(soa_row = integer(), sdz_row = integer()))
  tibble(soa_row = .x, sdz_row = .y)
})

# Build the small candidate sf for intersecting
soa_cand <- soa[pairs$soa_row, ]
sdz_cand <- sdz[pairs$sdz_row, ]

ints <- sf::st_sf(soa_id = character(), sdz_id = character(), geometry = st_sfc(), crs = target_crs)
# Real intersections
for( i in 1:500){
  print(paste((i-1)*10 +1 ,'-', i*10))
ints1 <- st_intersection(
  soa_cand %>% 
    mutate(.key = row_number()) |> 
    filter(between(row_number(), (i-1)*10 +1 , i*10)),
  sdz_cand %>% 
    mutate(.key2 = row_number()) |> 
    filter(between(row_number(), (i-1)*10 +1 , i*10))
)
  ints <- rbind(ints,ints1)
}

ints <- st_make_valid( ints)

ints$area <- st_area(ints) 


pal_pastel_28 <- function(n = 28, c = 35, l = 85) {
  h <- seq(0, 360 - 360/n, length.out = n)
  grDevices::hcl(h = h, c = c, l = l)
}
pastel28 <- pal_pastel_28()

pal_vibrant_28 <- function(n = 28, c = 80, l = 60) {
  h <- seq(0, 360 - 360/n, length.out = n)
  grDevices::hcl(h = h, c = c, l = l)
}
vibrant28 <- pal_vibrant_28()

pal_contrast_28 <- function(n = 28, c = 70, l = 55) {
  h <- seq(0, 360 - 360/n, length.out = n)
  grDevices::hcl(h = h, c = c, l = l)
}
contrast28 <- pal_contrast_28()

leaflet() |> 
  addTiles() |> 
  addPolygons(data = ints[ints$soa_id == '95MM15S2',],
              color = vibrant28,
              stroke = F,
              label =  ~paste(.key, sdz_id, area),
              layerId = ~paste(.key,sdz_id ),
              group='intersection'
              ) |>
  
  addPolygons(data = sdz_cand[sdz_cand$sdz_id %in% ints[ints$soa_id == '95MM15S2',]$sdz_id,] ,
               color='green', 
              stroke = T,
              fillOpacity = 0.1,
              label = ~sdz_id,
              layerId = ~sdz_id,
              group = 'SDZ') |> 
  addPolygons(data = soa_cand[soa_cand$soa_id=='95MM15S2',] ,
              color='blue', 
              stroke = T,
              fillOpacity = 0,
              label = ~soa_id, 
              group = 'SOA') |> 
  addLayersControl(baseGroups = c("OSM"),
                   overlayGroups = c('intersection','SOA','SDZ'),
                   options = layersControlOptions(collapsed = FALSE)
                   )

# ints now has both attributes (soa_id, sdz_id) if columns didn’t collide.
# If you lost IDs, recover from row indices:

# if (!("soa_id" %in% names(ints)) || !("sdz_id" %in% names(ints))) {
#   ints <- ints %>%
#     mutate(soa_id = soa_cand$soa_id[.key],
#            sdz_id = sdz_cand$sdz_id[.key2]) %>%
#     select(soa_id, sdz_id, geometry)
# }

#---------------------------
# 2) Areas + proportions
#---------------------------
soa_area <- soa %>% st_area() %>% as.numeric()
sdz_area <- sdz %>% st_area() %>% as.numeric()
soa_area_tbl <- tibble(soa_id = soa$soa_id, soa_area = soa_area)
sdz_area_tbl <- tibble(sdz_id = sdz$sdz_id, sdz_area = sdz_area)

overlap_tbl <- ints %>%
  mutate(overlap_area = as.numeric(st_area(geometry))) %>%
  st_set_geometry(NULL) %>%            # drop geometry for tabular ops
  left_join(soa_area_tbl, by = "soa_id") %>%
  left_join(sdz_area_tbl, by = "sdz_id") %>%
  mutate(
    prop_soa = overlap_area / soa_area,  # share of soa covered by this sdz
    prop_sdz = overlap_area / sdz_area,  # share of sdz covered by this soa
    iou     = overlap_area / (soa_area + sdz_area - overlap_area) # Jaccard
  )


lookup_sdz_soa <- overlap_tbl |> 
  group_by(sdz_id) |> 
  arrange(desc(iou)) |> 
  slice_head(n=1)
  
lookup_sdz_soa <- lookup_sdz_soa |> 
  select(soa_id, sdz_id, soa_sdz_conf = iou)


write_fst(lookup_sdz_soa, './synthetic_population/lookup_sdz_soa.fst') 

# Jaccard Intersection Overunion
# Simlarity metric between sets


# #---------------------------
# # 3) Weight matrices (long → wide)
# #---------------------------
# # soa -> sdz weights (rows sum ~1)
# W_soa2sdz <- overlap_tbl %>%
#   select(soa_id, sdz_id, prop_soa) %>%
#   tidyr::pivot_wider(names_from = sdz_id, values_from = prop_soa, values_fill = 0)
# 
# # sdz -> soa weights (rows sum ~1)
# W_sdz2soa <- overlap_tbl %>%
#   select(sdz_id, soa_id, prop_sdz) %>%
#   tidyr::pivot_wider(names_from = soa_id, values_from = prop_sdz, values_fill = 0)
# 
# #---------------------------
# # 4) Best matches & summaries
# #---------------------------
# soa_best <- overlap_tbl %>%
#   group_by(soa_id) %>%
#   slice_max(prop_soa, n = 1, with_ties = FALSE) %>%
#   ungroup() %>%
#   select(soa_id, sdz_id_best = sdz_id, soa_covered_by_best = prop_soa, iou)
# 
# sdz_best <- overlap_tbl %>%
#   group_by(sdz_id) %>%
#   slice_max(prop_sdz, n = 1, with_ties = FALSE) %>%
#   ungroup() %>%
#   select(sdz_id, soa_id_best = soa_id, sdz_covered_by_best = prop_sdz, iou)
# 
# metrics <- list(
#   soa_mean_max_coverage = mean(soa_best$soa_covered_by_best),
#   soa_median_max_coverage = median(soa_best$soa_covered_by_best),
#   sdz_mean_max_coverage = mean(sdz_best$sdz_covered_by_best),
#   sdz_median_max_coverage = median(sdz_best$sdz_covered_by_best),
#   mean_iou_all_pairs = mean(overlap_tbl$iou),
#   median_iou_all_pairs = median(overlap_tbl$iou)
# )
# 
# #---------------------------
# # 5) (Optional) global alignment score
# #    “One-to-one” mapping via maximum bipartite matching (Hungarian)
# #    to see how well soas can be paired to sdzs without reuse.
# #---------------------------
# # Install if needed: install.packages("clue")
# library(clue)
# # Build a cost matrix using 1 - IoU for candidate pairs; large cost for non-candidates
# soa_ids <- soa$soa_id; sdz_ids <- sdz$sdz_id
# idx_soa <- match(overlap_tbl$soa_id, soa_ids)
# idx_sdz <- match(overlap_tbl$sdz_id, sdz_ids)
# cost <- matrix(1, nrow = length(soa_ids), ncol = length(sdz_ids))
# cost[cbind(idx_soa, idx_sdz)] <- 1 - overlap_tbl$iou
# 
# # Solve assignment (works best if counts similar; otherwise you can pad)
# assign <- solve_LsoaP(cost)  # returns sdz column chosen for each soa row
# matched_iou <- 1 - cost[cbind(seq_along(assign), assign)]
# global_alignment_iou_mean <- mean(matched_iou, na.rm = TRUE)