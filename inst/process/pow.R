# Places of Worship
# https://download.osmdata.xyz/
library(sf)
library(ggplot2)
library(data.table)

grid <- readRDS("data/ca_grid20x20.RDS")
ca_bbox <- grid |> st_as_sf() |> st_bbox()

# path <- "data/amenity_EPSG4326.gpkg"
# pow <- lapply(st_layers(path)[["name"]],
#                 function(x) st_read(path,
#                                     query = paste0("SELECT * FROM \"",
#                                                    x,
#                                                    "\" WHERE amenity LIKE '%place_of_worship%'")
#                                     )
#                 )
# saveRDS(pow, file = "data/pow.RDS")
pow <- readRDS(file = "data/pow.RDS")[[1]] |>
  st_transform(3857) |>
  st_intersection(grid$geometry)

ca_pow <- setDT(pow)[, c("x", "y") := 
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

ca_pow_grid <- ca_pow[, by = .(xmin, xmax, ymin, ymax),
                          .(pow = sum(rounded_count),
                            id = paste0(xmin, ",", ymin, ",", xmax, ",", ymax),
                            geometry = st_polygon(x = list(
                              cbind(c(xmin, xmax, xmax, xmin, xmin),
                                    c(ymin, ymin, ymax, ymax, ymin)))) |>
                              sf::st_sfc()
                          )]

saveRDS(ca_pow_grid, "data/ca_pow_grid.RDS")

# Test areas
grid[, geometry] |> st_area() |> hist()
ca_pow_grid[, geometry] |> st_area() 
# Take a look
world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]
ggplot() +
  geom_sf(data = ca_pow_grid, aes(geometry = geometry, fill = pow), 
          color = NA) +
  geom_sf(data = ca, color = "white", fill = NA)
