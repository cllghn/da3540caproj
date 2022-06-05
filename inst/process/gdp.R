# Creating GDP Control Variable ------------------------------------------------

# The paper for this data set can be found here:
# https://www.nature.com/articles/sdata20184
# The data used here was accessed here:
# https://datadryad.org/stash/dataset/doi:10.5061/dryad.dk1j0
# 1. Open required libraries:
library(raster)
library(stars)
library(sf)
library(ggplot2)
library(data.table)

# 2. Read raster file using {stars}
# gdp_ppp_2015_30arcsec <- raster::raster("data/GDP_PPP_30arcsec_v3.nc", band = 3)

# NOTES: The GDP per capita (PPP) dataset represents average gross domestic 
# production per capita in a given administrative area unit. 
# GDP is given in 2011 international US dollars. 
# Gap-filled sub-national data were used, supplemented by national data where 
# necessary. Datagaps were filled by using national temporal pattern. 
# Dataset has global extent at 5 arc-min resolution for the 26-year period of 
# 1990-2015. Detail description is given in a linked article and metadata is 
# provided as an attribute in the NetCDF file itself.

# 2.1 Inspect object
# gdp_ppp_2015_30arcsec
# 2.2 Plot object 
# raster::plot(gdp_ppp_2015_30arcsec)
# 2.3 Write out aggregated raster as a tiff
# raster::writeRaster(gdp_ppp_2015_30arcsec, "data/gdp_ppp_2015_30arcsec.tiff",
#                     overwrite = TRUE)

# 3. Read in .tiff  and ransform it to an sf object 
# gdp_ppp_2015 <- stars::read_stars("data/gdp_ppp_2015_30arcsec.tiff") |>
#   sf::st_as_sf() |>
#   `colnames<-`(c("gdp_ppp_2015", "geometry")) |>
#   sf::`st_crs<-`(4326) |>
#   setDT()
# 3.1 Extract centroid from the polygons. We will match point data with the 
# grid at 20km^2 resolution level
# gdp_ppp_2015 <- gdp_ppp_2015[, id := 1:NROW(.SD)][gdp_ppp_2015 != -9
#                              ][, c("x", "y") := .(mean(unlist(geometry)[[1]],
#                                                        unlist(geometry)[[2]]),
#                                   mean(unlist(geometry)[[6]],
#                                        unlist(geometry)[[8]])),
#                  by = id]
# 
# saveRDS(gdp_ppp_2015, file = "data/gdp_ppp_2015.RDS")
# gdp_ppp_2015 <- readRDS("data/gdp_ppp_2015.RDS")

# 3. Downsize...
grid <- readRDS("data/ca_grid20x20.RDS")
# ca_bbox_4326 <- grid |> st_as_sf() |> st_transform(4326) |> st_bbox()
# ca_gdp_ppp_2015 <- gdp_ppp_2015[x >= ca_bbox_4326[["xmin"]] &
#                                   y >= ca_bbox_4326[["ymin"]] &
#                                   x <= ca_bbox_4326[["xmax"]] &
#                                   y <= ca_bbox_4326[["ymax"]]
#                                 ][, id := paste0(x, ",", y)] |> 
#   st_as_sf(coords = c("x", "y"), crs = 4326) |>
#   st_transform(3857) |>
#   st_intersection(grid$geometry)
# saveRDS(ca_gdp_ppp_2015, "data/ca_gdp_ppp_2015.RDS")
ca_gdp_ppp_2015 <- readRDS("data/ca_gdp_ppp_2015.RDS")
ca_gdp_ppp_2015_grid <- setDT(ca_gdp_ppp_2015)[, c("x", "y") := 
                                            .(round(unlist(geometry)[[1]]),
                                              round(unlist(geometry)[[2]])),
                            by = id
                            ][, c("xmin", "xmax", "ymin", "ymax") :=
                                .(max(grid[grid[["xmin"]] <= x, xmin]),
                                  min(grid[grid[["xmax"]] >= x, xmax]),
                                  max(grid[grid[["ymin"]] <= y, ymin]),
                                  min(grid[grid[["ymax"]] >= y, ymax])),
                              by = id
                              ][, by = .(xmin, xmax, ymin, ymax),
                                .(avg_gdp = mean(gdp_ppp_2015),
                                  sum_gdp = sum(gdp_ppp_2015),
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
ca_gdp_ppp_2015_grid <- ca_gdp_ppp_2015_grid |>
  st_as_sf(crs = 3857) |>
  st_intersection(ca$geometry) |>
  setDT()

saveRDS(ca_gdp_ppp_2015_grid, "data/ca_gdp_ppp_2015_grid.RDS")

# Test areas
grid[, geometry] |> st_area() |> hist()
ca_gdp_ppp_2015_grid[, geometry] |> st_area() 
# Take a look
world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]
ggplot() +
  geom_sf(data = ca_gdp_ppp_2015_grid, aes(geometry = geometry, fill = avg_gdp), 
          color = NA) +
  geom_sf(data = ca, color = "white", fill = NA)

