# Creating the conflict variable -----------------------------------------------
# 
# The goal is creating a dependent variable on collective violence from the UCDP
# data made publically available through the following URL:
# https://ucdp.uu.se/downloads/index.html#ged_global

library(data.table)
library(ggplot2)
library(sf)

grid <- readRDS("data/ca_grid20x20.RDS")
ca_bbox <- grid |> st_as_sf() |> st_bbox()

file <- "data/ged211.csv"
ucdp <- fread(file)[, c("x", "y") := .(round(longitude, 2), 
                                       round(latitude, 2)),
                    by = .(id)
                    ][, .N, by = .(x, y)] |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  st_transform(3857) |>
  st_intersection(grid$geometry)

ca_ucdp <- setDT(ucdp)[, temp_id := 1:NROW(ucdp)
                 ][, c("x", "y") := .(unlist(geometry)[[1]],
                                      unlist(geometry)[[2]]),
                   by = temp_id
                   ][, c("xmin", "xmax", "ymin", "ymax") :=
                       .(max(grid[grid[["xmin"]] <= x, xmin]),
                         min(grid[grid[["xmax"]] >= x, xmax]),
                         max(grid[grid[["ymin"]] <= y, ymin]),
                         min(grid[grid[["ymax"]] >= y, ymax])),
                     by = .(temp_id)]
ca_ucdp_grid <- ca_ucdp[, by = .(xmin, xmax, ymin, ymax),
                 .(conflict = sum(N),
                   id = paste0(xmin, ",", ymin, ",", xmax, ",", ymax),
                   geometry = st_polygon(x = list(
                     cbind(c(xmin, xmax, xmax, xmin, xmin),
                           c(ymin, ymin, ymax, ymax, ymin)))) |>
                     sf::st_sfc()
                 )]
saveRDS(ca_ucdp_grid, "data/ca_ucdp_grid.RDS")

# Test areas
grid[, geometry] |> st_area() |> hist()
ca_ucdp_grid[, geometry] |> st_area() 
# Take a look
world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]
ggplot() +
  geom_sf(data = ca_ucdp_grid, aes(geometry = geometry, fill = conflict), 
          color = NA) +
  geom_sf(data = ca, color = "white", fill = NA)
