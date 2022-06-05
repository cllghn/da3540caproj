# Power plants
library(data.table)
library(sf)
# https://datasets.wri.org/dataset/globalpowerplantdatabase
# https://sedac.ciesin.columbia.edu/data/set/groads-global-roads-open-access-v1


grid <- readRDS("data/ca_grid20x20.RDS")
ca_bbox <- grid |> st_as_sf() |> st_bbox()

file <- "data/global_power_plant_database.csv"
res <- fread(file) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(3857) |>
  st_intersection(grid$geometry)
  
ca_power <- setDT(res)[, temp_id := 1:NROW(res)
                       ][, c("x", "y") := .(unlist(geometry)[[1]],
                                            unlist(geometry)[[2]]),
                         by = temp_id
                         ][, c("xmin", "xmax", "ymin", "ymax") :=
                             .(max(grid[grid[["xmin"]] <= x, xmin]),
                               min(grid[grid[["xmax"]] >= x, xmax]),
                               max(grid[grid[["ymin"]] <= y, ymin]),
                               min(grid[grid[["ymax"]] >= y, ymax])),
                           by = .(temp_id)]

ca_power_grid <- ca_power[, by = .(xmin, xmax, ymin, ymax),
                        .(power = .N,
                          id = paste0(xmin, ",", ymin, ",", xmax, ",", ymax),
                          geometry = st_polygon(x = list(
                            cbind(c(xmin, xmax, xmax, xmin, xmin),
                                  c(ymin, ymin, ymax, ymax, ymin)))) |>
                            sf::st_sfc()
                        )]
saveRDS(ca_power_grid, "data/ca_power_grid.RDS")

# Test areas
grid[, geometry] |> st_area() |> hist()
ca_power_grid[, geometry] |> st_area() 
# Take a look
world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]
ggplot() +
  geom_sf(data = ca_power_grid, aes(geometry = geometry, fill = power), 
          color = NA) +
  geom_sf(data = ca, color = "white", fill = NA)
