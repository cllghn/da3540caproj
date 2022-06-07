library(data.table)
library(ggplot2)
library(sf)
library(raster)

grid <- readRDS("data/ca_grid20x20.RDS")
ca_bbox <- grid |> st_as_sf() |> st_bbox()
ca_bbox_4326 <- grid |> st_as_sf() |> st_transform(4326) |> st_bbox()
world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]

# Option 1: Clip by extent
# pop_density_1x <- raster::raster("data/gpw_v4_population_density_rev11_2020_30_sec.tif")
# e <- as(extent(-92.246257, -77.141337, 5.515082, 18.491156), # it is xmin, xmax, ymin, ymax
#         'SpatialPolygons')
# crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
# pop_density_1x_crop <- crop(pop_density_1x, e)
# raster::writeRaster(pop_density_1x_crop, filename = "data/pop_density_1x_crop.tif",
#                     overwrite = TRUE)

# Option 2: Aggregate
# pop_density_2x <- raster::raster("data/gpw_v4_population_density_rev11_2020_30_sec.tif") |>
#   raster::aggregate(fact = 2, fun = "max")
# raster::writeRaster(pop_density_2x, filename = "data/pop_density_2x.tif")


pop_density <- stars::read_stars("data/pop_density_1x_crop.tif") |>
  sf::st_as_sf() |>
  sf::`st_crs<-`(4326) |>
  setDT()
 
pop_density <- pop_density[, c("x", "y") := .(mean(unlist(geometry)[[1]],
                                                   unlist(geometry)[[2]]),
                                              mean(unlist(geometry)[[7]],
                                                   unlist(geometry)[[8]])),
                           by = 1:NROW(pop_density)
                           ]

# Store it as the matching process takes a bit of time...
saveRDS(pop_density, "data/pop_density_raw.RDS")

pop_density <- readRDS("data/pop_density_raw.RDS")

ca_pop_density <- pop_density[x >= ca_bbox_4326[["xmin"]] &
                                x <= ca_bbox_4326[["xmax"]] &
                                y >= ca_bbox_4326[["ymin"]] &
                                y <= ca_bbox_4326[["ymax"]]] |>
  st_as_sf() |>
  st_transform(3857) |>
  # st_intersection(ca$geometry) |>
  setDT()

ca_pop_density_grid <- ca_pop_density[, temp_id := 1:NROW(ca_pop_density)
                                      ][, c("x", "y") := 
                                          .(mean(unlist(geometry)[[1]], unlist(geometry)[[2]]),
                                            mean(unlist(geometry)[[7]], unlist(geometry)[[8]])),
                                      by = temp_id
                                      ][, c("xmin", "xmax", "ymin", "ymax") :=
                                          .(max(grid[grid[["xmin"]] <= x, xmin]),
                                            min(grid[grid[["xmax"]] >= x, xmax]),
                                            max(grid[grid[["ymin"]] <= y, ymin]),
                                            min(grid[grid[["ymax"]] >= y, ymax])),
                                        by = temp_id][, by = .(xmin, xmax, ymin, ymax),
                                          .(density_sum = sum(pop_density_1x_crop.tif),
                                            density_avg = mean(pop_density_1x_crop.tif),
                                            id = paste0(xmin, ",", ymin, ",", xmax, ",", ymax),
                                            geometry = st_polygon(x = list(
                                              cbind(c(xmin, xmax, xmax, xmin, xmin),
                                                    c(ymin, ymin, ymax, ymax, ymin)))) |>
                                              sf::st_sfc()
                                          )]
saveRDS(ca_pop_density_grid, "data/ca_pop_density_grid_complete.RDS")

world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]
ca_pop_density_grid <- ca_pop_density_grid |>
  st_as_sf(crs = 3857) |>
  st_intersection(ca$geometry) |>
  setDT()

saveRDS(ca_pop_density_grid, "data/ca_pop_density_grid.RDS")


# Test areas
test <- merge(x = grid, y = unique(ca_pop_density_grid[, .(id, density_avg)]),
              by = "id", all.x = TRUE)
test[, geometry] |> st_area() |> hist()

# Take a look
world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]
ggplot() +
  geom_sf(data = ca_pop_density_grid, aes(geometry = geometry, fill = density_avg), 
          color = NA) +
  geom_sf(data = ca, color = "white", fill = NA)

