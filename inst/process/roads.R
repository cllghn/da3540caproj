library(ggplot2)
library(sf)
library(data.table)

grid <- readRDS("data/ca_grid20x20.RDS")
ca_bbox <- grid |> st_as_sf() |> st_bbox()
ca_bbox_4326 <- grid |> st_as_sf() |> st_transform(4326) |> st_bbox()

# Change format and cast data
# res <- sf::read_sf(dsn = "data/gROADS_v1.gdb") |>
#   st_cast("POINT") |>
#   setDT()
# 
# saveRDS(res, "data/road_points.RDS")

# res <- readRDS("data/road_points.RDS")
# 
# ca_road_point <- res[, temp_id := 1:NROW(res)
#           ][, c("x", "y") := .(unlist(Shape)[[1]],
#                                unlist(Shape)[[2]]),
#             by = temp_id
#             ][, !c("Shape")][x >= ca_bbox_4326[["xmin"]] &
#               x <= ca_bbox_4326[["xmax"]] &
#               y >= ca_bbox_4326[["ymin"]] &
#               y <= ca_bbox_4326[["ymax"]]] |>
#   st_as_sf(coords = c("x", "y"), crs = 4326) |>
#   st_transform(3857) |>
#   st_intersection(grid$geometry)
# saveRDS(ca_road_point, "data/ca_road_point.RDS")
# ca_road_point <- readRDS("data/ca_road_point.RDS")
# 
# ca_road_point <- setDT(ca_road_point)[, temp_id := 1:NROW(ca_road_point)
#                                       ][, c("x", "y") := .(unlist(geometry)[[1]],
#                                                            unlist(geometry)[[2]]),
#                                         by = temp_id
#                                       ][, c("xmin", "xmax", "ymin", "ymax") :=
#                                           .(max(grid[grid[["xmin"]] <= x, xmin]),
#                                             min(grid[grid[["xmax"]] >= x, xmax]),
#                                             max(grid[grid[["ymin"]] <= y, ymin]),
#                                             min(grid[grid[["ymax"]] >= y, ymax])),
#                                         by = .(temp_id)
#                                         ]
#saveRDS(ca_road_point, "data/ca_road_point.RDS")
ca_road_point <- readRDS("data/ca_road_point.RDS")

ca_road_grid <- unique(ca_road_point, by = c("xmin", "xmax", "ymin", "ymax", "ROADID"))[, by = .(xmin, xmax, ymin, ymax),
                              .(roads = .N,
                                id = paste0(xmin, ",", ymin, ",", xmax, ",", ymax),
                                geometry = st_polygon(x = list(
                                  cbind(c(xmin, xmax, xmax, xmin, xmin),
                                        c(ymin, ymin, ymax, ymax, ymin)))) |>
                                  sf::st_sfc())
                              ]
saveRDS(ca_road_grid, "data/ca_road_grid.RDS")

# Take a look
world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]
ggplot() +
  geom_sf(data = ca_road_grid, aes(geometry = geometry, fill = roads), 
          color = NA) +
  geom_sf(data = ca, color = "white", fill = NA)
