library(dplyr)
library(ggmap)
library(ggspatial)
library(sf)

register_google(key  = readLines(".secret/googlekey"))
panama <- get_googlemap(
  center  = c(-79.550107, 8.949327),
  style    = "feature:poi|element:labels|visibility:off",
  maptype = "satellite",
  # maptype = "terrain",
  zoom    = 10
)

grid <- readRDS("data/grid_w_variables.RDS") |> st_as_sf() |> st_transform(4326)

m1 <- ggmap(panama) +
  geom_sf(data = grid, inherit.aes = FALSE, color = "lightgrey", fill = NA, alpha = 0.25) +
  geom_point(aes(x = -79.5373546, 8.9523203), color = "white", size = 2) +
  geom_point(aes(x = -79.5373546, 8.9523203), color = "red", size = 1.25) +
  labs(x = "", y = "") +
  theme(axis.ticks      = element_blank(),
        axis.text.x     = element_blank(),
        axis.text.y     = element_blank(),
        panel.grid      = element_blank(),
        panel.border    = element_blank(),
        plot.margin     = margin(0, 0, 0, 0, "pt")) +
  annotate(geom = "text", x = -79.4, y = 8.9523203,
           label = "Panama Metropolitan Cathedral", fontface = "italic", color = "white",
           size = 5)
ggsave(m1, filename = "inst/visuals/10x.png", width = 10, height = 10)

panama <- get_googlemap(
  center  = c(-79.550107, 8.949327),
  style    = "feature:poi|element:labels|visibility:off",
  maptype = "satellite",
  # maptype = "terrain",
  zoom    = 12
)
m0 <- ggmap(panama) +
  geom_sf(data = grid, inherit.aes = FALSE, color = "lightgrey", fill = NA, alpha = 0.25) +
  geom_point(aes(x = -79.5373546, 8.9523203), color = "white", size = 4) +
  geom_point(aes(x = -79.5373546, 8.9523203), color = "red", size = 3.25) +
  labs(x = "", y = "") +
  theme(axis.ticks      = element_blank(),
        axis.text.x     = element_blank(),
        axis.text.y     = element_blank(),
        panel.grid      = element_blank(),
        panel.border    = element_blank(),
        plot.margin     = margin(0, 0, 0, 0, "pt")) +
  annotate(geom = "text", x = -79.49, y = 8.9523203,
           label = "Panama Metropolitan Cathedral", fontface = "italic", color = "white",
           size = 5)
ggsave(m0, filename = "inst/visuals/12x.png", width = 10, height = 10)

panama <- get_googlemap(
  center  = c(-79.550107, 8.949327),
  style    = "feature:poi|element:labels|visibility:off",
  maptype = "satellite",
  # maptype = "terrain",
  zoom    = 7
)

world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
pa <- world[world$admin %in% c("Panama"), ]

m2 <- ggmap(panama) +
  geom_sf(data = grid, inherit.aes = FALSE, color = "lightgrey", fill = NA, alpha = 0.25) +
  geom_sf(data = pa, inherit.aes = FALSE, color = "red", fill = NA, lwd = 0.75) +
  geom_point(aes(x = -79.5373546, 8.9523203), color = "white", size = 4) +
  geom_point(aes(x = -79.5373546, 8.9523203), color = "red", size = 3) +
  labs(x = "", y = "") +
  theme(axis.ticks      = element_blank(),
        axis.text.x     = element_blank(),
        axis.text.y     = element_blank(),
        panel.grid      = element_blank(),
        panel.border    = element_blank(),
        plot.margin     = margin(0, 0, 0, 0, "pt")) 
ggsave(m2, filename = "inst/visuals/6x.png", width = 10, height = 10)
