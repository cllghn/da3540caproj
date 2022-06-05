library(sf)
library(ggplot2)
library(data.table)

# Build a grid
world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]
ca_grid20x20 <- ca |> st_transform(3857) |>
  st_make_grid(cellsize = c(4472, 4472))
ca_grid20x20 <- ca_grid20x20[ca]

ca_grid20x20 <- setDT(
  as.data.frame(ca_grid20x20)
  )[, c("xmin", "ymin", "xmax", "ymax") :=
      .(unlist(geometry)[[1]], unlist(geometry)[[7]],
        unlist(geometry)[[2]], unlist(geometry)[[8]]), 
    by = 1:NROW(ca_grid20x20)
    ][, id := paste0(xmin, ",", ymin, ",", xmax, ",", ymax)
      ][, .(xmin, ymin, xmax, ymax, id, geometry)]
saveRDS(ca_grid20x20, "data/ca_grid20x20.RDS")

# Test areas
ca_grid20x20[, geometry] |> st_area() |> hist()
# Take a look
ggplot() +
  geom_sf(data = ca_grid20x20, aes(geometry = geometry)) +
  geom_sf(data = ca, color = "white", fill = NA)

