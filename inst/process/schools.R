# https://download.osmdata.xyz/
library(sf)
library(ggplot2)
library(data.table)

grid <- readRDS("data/ca_grid20x20.RDS")
ca_bbox <- grid |> st_as_sf() |> st_bbox()

# path <- "data/amenity_EPSG4326.gpkg"
# school <- lapply(st_layers(path)[["name"]],
#                 function(x) st_read(path,
#                                     query = paste0("SELECT * FROM \"",
#                                                    x,
#                                                    "\" WHERE amenity LIKE '%school%'")
#                                     )
#                 )
# saveRDS(school, file = "data/school.RDS")
school <- readRDS("data/school.RDS")[[1]] |>
  st_transform(3857) |>
  st_intersection(grid$geometry)

ca_school <- setDT(school)[, c("x", "y") := 
                           .(round(unlist(`_ogr_geometry_`)[[1]], 2),
                             round(unlist(`_ogr_geometry_`)[[2]], 2)),
                         by = .(osm_id)][, by = .(x, y), .("rounded_count" = .N,
                                                           "id" = paste0(x, ",", y))
                         ][, c("xmin", "xmax", "ymin", "ymax") :=
                             .(max(grid[grid[["xmin"]] <= x, xmin]),
                               min(grid[grid[["xmax"]] >= x, xmax]),
                               max(grid[grid[["ymin"]] <= y, ymin]),
                               min(grid[grid[["ymax"]] >= y, ymax])),
                           by = id]

ca_school_grid <- ca_school[, by = .(xmin, xmax, ymin, ymax),
                          .(school = sum(rounded_count),
                            id = paste0(xmin, ",", ymin, ",", xmax, ",", ymax),
                            geometry = st_polygon(x = list(
                              cbind(c(xmin, xmax, xmax, xmin, xmin),
                                    c(ymin, ymin, ymax, ymax, ymin)))) |>
                              sf::st_sfc()
                          )]

saveRDS(ca_school_grid, "data/ca_school_grid.RDS")

# Test areas
grid[, geometry] |> st_area() |> hist()
ca_parks_grid[, geometry] |> st_area() 
# Take a look
world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]
ggplot() +
  geom_sf(data = ca_school_grid, aes(geometry = geometry, fill = school), 
          color = NA) +
  geom_sf(data = ca, color = "white", fill = NA)
