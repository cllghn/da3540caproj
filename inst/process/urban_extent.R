library(data.table)
library(ggplot2)
library(sf)

grid <- readRDS("data/ca_grid20x20.RDS")
ca_bbox <- grid |> st_as_sf() |> st_bbox()

# https://sedac.ciesin.columbia.edu/data/set/grump-v1-urban-extents/data-download

# Load data, transform from raster to data.frame, set CRS, coherce into data.table
# ext <- raster::raster("data/glurextents.bil") |>
#   raster::aggregate(fact = 4, fun = "max")
# 
# raster::writeRaster(ext, "data/glurextents_4x.tiff", overwrite = TRUE)

# ext_4x <- stars::read_stars("data/glurextents_4x.tiff") |>  
#   sf::st_as_sf() |>
#   sf::`st_crs<-`(4326) |> 
#   st_transform(3857) |>
#   setDT()

# Keep only urban centers
# ext_4x <- ext_4x[glurextents_4x.tiff == 2
#                  ][, id := 1:NROW(.SD)
#                    ][, c("x", "y") := 
#     .(mean(unlist(geometry)[[1]], unlist(geometry)[[2]]),
#       mean(unlist(geometry)[[6]], unlist(geometry)[[8]])),
#   by = .(id)]
# saveRDS(ext_4x, "data/ext_4x_only_urban.RDS")

ext_4x <- readRDS("data/ext_4x_only_urban.RDS")

ca_ext_4x_grid <- ext_4x[x >= ca_bbox[["xmin"]] &
                      x <= ca_bbox[["xmax"]] &
                      y >= ca_bbox[["ymin"]] &
                      y <= ca_bbox[["ymax"]]
                    ][, c("xmin", "xmax", "ymin", "ymax") :=
                        .(max(grid[grid[["xmin"]] <= x, xmin]),
                          min(grid[grid[["xmax"]] >= x, xmax]),
                          max(grid[grid[["ymin"]] <= y, ymin]),
                          min(grid[grid[["ymax"]] >= y, ymax])),
                      by = id
                      ][, by = .(xmin, xmax, ymin, ymax),
                        .(urban_sum = sum(glurextents_4x.tiff),
                          id = paste0(xmin, ",", ymin, ",", xmax, ",", ymax),
                          geometry = st_polygon(x = list(
                            cbind(c(xmin, xmax, xmax, xmin, xmin),
                                  c(ymin, ymin, ymax, ymax, ymin)))) |>
                            sf::st_sfc()
                        )][, urban_bin := ifelse(urban_sum > 0, TRUE, FALSE)]


world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]
ca_ext_4x_grid <- ca_ext_4x_grid |>
  st_as_sf(crs = 3857) |>
  st_intersection(ca$geometry) |>
  setDT()

saveRDS(ca_ext_4x_grid, "data/ca_ext_4x_grid.RDS")


# Take a look
ggplot() +
  geom_sf(data = ca_ext_4x_grid, aes(geometry = geometry, fill = urban_bin), 
          color = NA) +
  geom_sf(data = ca, color = "white", fill = NA)
