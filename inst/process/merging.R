library(sf)
library(ggplot2)
library(data.table)

grid <- readRDS("data/ca_grid20x20.RDS")

grid <- merge(x = grid, y = unique(readRDS("data/ca_towers_grid.RDS")[, .(id, towers)]),
              by = "id", all.x  = TRUE)
grid <- merge(x = grid, y = unique(readRDS("data/ca_ucdp_grid.RDS")[, .(id, conflict)]),
              by = "id", all.x  = TRUE)
grid <- merge(x = grid, y = unique(readRDS("data/ca_ext_4x_grid.RDS")[, .(id, urban_bin)]),
              by = "id", all.x  = TRUE)
grid <- merge(x = grid, y = unique(readRDS("data/ca_parks_grid.RDS")[, .(id, parks)]),
              by = "id", all.x  = TRUE)
grid <- merge(x = grid, y = unique(readRDS("data/ca_pow_grid.RDS")[, .(id, pow)]),
              by = "id", all.x  = TRUE)
grid <- merge(x = grid, y = unique(readRDS("data/ca_power_grid.RDS")[, .(id, power)]),
              by = "id", all.x  = TRUE)
grid <- merge(x = grid, y = unique(readRDS("data/ca_school_grid.RDS")[, .(id, school)]),
              by = "id", all.x  = TRUE)
grid <- merge(x = grid, y = unique(readRDS("data/ca_health_grid.RDS")[, .(id, health)]),
              by = "id", all.x  = TRUE)
grid <- merge(x = grid, y = unique(readRDS("data/ca_gdp_ppp_2015_grid.RDS")[, .(id, avg_gdp, sum_gdp)]),
              by = "id", all.x  = TRUE)
grid <- merge(x = grid, y = unique(readRDS("data/ca_pop_density_grid.RDS")[, .(id, density_avg, density_sum)]),
              by = "id", all.x  = TRUE)
grid <- merge(x = grid, y = unique(readRDS("data/ca_road_grid.RDS")[, .(id, roads)]),
              by = "id", all.x  = TRUE)

saveRDS(grid, "data/grid_w_variables.RDS")

# Take a look
world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "large") |>
  st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]

ggplot() +
  geom_sf(data = grid , aes(geometry = geometry, fill = conflict), 
          color = NA) +
  geom_sf(data = ca, color = "white", fill = NA, lwd = 1.5)
