library(data.table)
library(ggplot2)
library(sf)

grid <- readRDS("data/ca_grid20x20.RDS")
ca_bbox <- grid |> st_as_sf() |> st_bbox()

towers <- data.table::fread(input = "data/cell_towers.csv.gz"
                            )[, c("x", "y") :=
                     .(round(lon, digits = 2), round(lat, digits = 2))
                   ][, .N, by = .(x, y)
                     ] |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  st_transform(3857) 

# Down size
ca_towers <- setDT(towers)[, temp_id := 1:NROW(towers)
                 ][, c("x", "y") := .(unlist(geometry)[[1]],
                                      unlist(geometry)[[2]]),
                   by = temp_id
                   ][x >= ca_bbox[["xmin"]] &
                       y >= ca_bbox[["ymin"]] &
                       x <= ca_bbox[["xmax"]] &
                       y <= ca_bbox[["ymax"]]
                     ][, c("xmin", "xmax", "ymin", "ymax") :=
                         .(max(grid[grid[["xmin"]] <= x, xmin]),
                           min(grid[grid[["xmax"]] >= x, xmax]),
                           max(grid[grid[["ymin"]] <= y, ymin]),
                           min(grid[grid[["ymax"]] >= y, ymax])),
                       by = temp_id]

ca_towers_grid <- ca_towers[, by = .(xmin, xmax, ymin, ymax),
                        .(towers = sum(N),
                          id = paste0(xmin, ",", ymin, ",", xmax, ",", ymax),
                          geometry = st_polygon(x = list(
                            cbind(c(xmin, xmax, xmax, xmin, xmin),
                                  c(ymin, ymin, ymax, ymax, ymin)))) |>
                            sf::st_sfc()
                        )]

# Bound polygons only to study area...
world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]
ca_towers_grid <- ca_towers_grid |>
  st_as_sf(crs = 3857) |>
  st_intersection(ca$geometry) |>
  setDT()
saveRDS(ca_towers_grid, "data/ca_towers_grid.RDS")

# Test areas
grid[, geometry] |> st_area() |> hist()
ca_towers_grid[, geometry] |> st_area() 
# Take a look
ggplot() +
  geom_sf(data = ca_towers_grid , aes(geometry = geometry, fill = towers), 
          color = NA) +
  geom_sf(data = ca, color = "white", fill = NA, lwd = 1.5)

