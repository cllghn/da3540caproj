library(data.table)
library(ggmap)
library(ggplot2)
library(ggspatial)

grid <- readRDS("data/grid_w_variables.RDS")

# UCDP collective violence
world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]

g <- ggplot(data = grid) +
  geom_sf(data = ca, fill = "black", lwd = 0.10, color = "white") +
  geom_sf(aes(geometry = geometry, fill = log(conflict + 0.01)), color = NA) +
  scale_fill_gradient(low = "#fff7bc", high = "#662506",
                      name = "Log Count of \n Events", na.value = NA) +
  theme_minimal() +
  theme(
    text = element_text(color = "#22211d"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom", 
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x = NULL, 
       y = NULL, 
       title = "Collective Violence Events",        
       # subtitle = bquote('Unit of Analysis: Grid Cell 20'~Km^2), 
       caption = bquote('Data: UCDP, 2022 \nUnit of Analysis: Grid Cell 20'~Km^2))
ggsave(g, filename = "inst/visuals/conflict.png", width = 5, height = 7)

# Towers
g2 <- ggplot(data = grid) +
  geom_sf(data = ca, fill = "black", lwd = 0.10, color = "white") +
  geom_sf(aes(geometry = geometry, fill = log(towers + 1)), color = NA) +
  scale_fill_gradient(low = "#ffffcc", high = "#800026",
                      name = "Log Count of \nTowers in a Grid",
                      na.value = NA) +
  theme_minimal() +
  theme(
    text = element_text(color = "#22211d"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom", 
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x = NULL, 
       y = NULL, 
       title = "Cellphone Tower Density",        
       caption = bquote('Data: opencellid.org, 2022 \nUnit of Analysis: Grid Cell 20'~Km^2)
       )
ggsave(g2, filename = "inst/visuals/towers.png", width = 5, height = 7)

# Power generation
g3 <- ggplot(data = grid) +
  geom_sf(data = ca, fill = "black", lwd = 0.10, color = "white") +
  geom_sf(aes(geometry = geometry, fill = log(power + 1)), color = NA) +
  scale_fill_gradient(low = "#ffffcc", high = "#800026",
                      name = "Log Count of \nPower Plants in a Grid",
                      na.value = NA) +
  theme_minimal() +
  theme(
    text = element_text(color = "#22211d"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom", 
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x = NULL, 
       y = NULL, 
       title = "Power Plant Density",        
       caption = bquote('Data: WRI, 2021 \nUnit of Analysis: Grid Cell 20'~Km^2)
  )
ggsave(g3, filename = "inst/visuals/power.png", width = 5, height = 7)

# Roads
g4 <- ggplot(data = grid) +
  geom_sf(data = ca, fill = "black", lwd = 0.10, color = "white") +
  geom_sf(aes(geometry = geometry, fill = log(roads + 1)), color = NA) +
  scale_fill_gradient(low = "#ffffcc", high = "#800026",
                      name = "Log Count of \nRoads in a Grid",
                      na.value = NA) +
  theme_minimal() +
  theme(
    text = element_text(color = "#22211d"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom", 
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x = NULL, 
       y = NULL, 
       title = "Road Density",        
       caption = bquote('Data: CIESIN, 2013 \nUnit of Analysis: Grid Cell 20'~Km^2)
  )
ggsave(g4, filename = "inst/visuals/roads.png", width = 5, height = 7)

# OSM
g5 <- ggplot(data = grid) +
  geom_sf(data = ca, fill = "black", lwd = 0.10, color = "white") +
  geom_sf(aes(geometry = geometry, fill = log(parks + 1)), color = NA) +
  scale_fill_gradient(low = "#ffffcc", high = "#800026",
                      na.value = NA) +
  theme_minimal() +
  theme(
    text = element_text(color = "#22211d"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x = NULL, 
       y = NULL,
       title = "Park Density")

g6 <- ggplot(data = grid) +
  geom_sf(data = ca, fill = "black", lwd = 0.10, color = "white") +
  geom_sf(aes(geometry = geometry, fill = log(pow + 1)), color = NA) +
  scale_fill_gradient(low = "#ffffcc", high = "#800026",
                      na.value = NA) +
  theme_minimal() +
  theme(
    text = element_text(color = "#22211d"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x = NULL, 
       y = NULL,
       title = "Places of Worship Density")
g7 <- ggplot(data = grid) +
  geom_sf(data = ca, fill = "black", lwd = 0.10, color = "white") +
  geom_sf(aes(geometry = geometry, fill = log(school + 1)), color = NA) +
  scale_fill_gradient(low = "#ffffcc", high = "#800026",
                      na.value = NA) +
  theme_minimal() +
  theme(
    text = element_text(color = "#22211d"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x = NULL, 
       y = NULL,
       title = "School Density")
g8 <- ggplot(data = grid) +
  geom_sf(data = ca, fill = "black", lwd = 0.10, color = "white") +
  geom_sf(aes(geometry = geometry, fill = log(health + 1)), color = NA) +
  scale_fill_gradient(low = "#ffffcc", high = "#800026",
                      na.value = NA) +
  theme_minimal() +
  theme(
    text = element_text(color = "#22211d"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "none", 
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x = NULL, 
       y = NULL,
       title = "Healthcare Density")

require(gridExtra)
grid.arrange(ncol = 2, nrow = 2, g5, g6, g7, g8)


ggsave(arrangeGrob(g5, g6, g7, g8),
       filename = "inst/visuals/osm.png", width = 5, height = 7)



