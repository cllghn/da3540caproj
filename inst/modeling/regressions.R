library(data.table)
library(ggplot2)
library(MixMerge)
library(stargazer)
library(sf)
grid <- readRDS("data/grid_w_variables.RDS")

# 1. Recode to include no missing values and filter to only land.
grid_land <- grid[, c("towers", "conflict", "point_park", "point_pow", "gdp_sum",
      "gdp_avg", "point_school", "power", #"clean_power",
      "roads",
      "density_sum", "density_avg", "healthcare",
      "urban_bin") := 
    .(ifelse(is.na(towers), 0, towers),
      ifelse(is.na(conflict), 0, conflict),
      ifelse(is.na(parks), 0, parks),
      ifelse(is.na(pow), 0, pow),
      ifelse(is.na(sum_gdp), 0, sum_gdp),
      ifelse(is.na(avg_gdp), 0, avg_gdp),
      ifelse(is.na(school), 0, school),
      ifelse(is.na(power), 0, power),
      # ifelse(is.na(clean_power), 0, clean_power),
      ifelse(is.na(roads), 0, roads),
      ifelse(is.na(density_sum), 0, density_sum),
      ifelse(is.na(density_avg), 0, density_avg),
      ifelse(is.na(health), 0, health),
      ifelse(is.na(urban_bin), 0, urban_bin)
    )][, pg := sum(point_park, point_pow, point_school, healthcare, roads), by = id
       ][, txpg := towers * pg, by = id]

world <- rnaturalearth::ne_countries(returnclass = 'sf', scale = "small") |>
  sf::st_transform(3857)
ca <- world[world$admin %in% c("Guatemala", "Belize", "Honduras", "El Salvador",
                               "Nicaragua", "Costa Rica", "Panama"), ]

grid_land <- grid_land |> st_as_sf() |> st_intersection(ca)

# Build models -----------------------------------------------------------------
# 1. Check assumptions
# [X] y-values are counts
# [X] Counts must be positive integers: 0 or greater (0,1,2,3…k)
range(grid_land$conflict)
# [X] Counts must follow a Poisson distribution
ggplot(grid_land) +
  # geom_density(color = "red", aes(x = conflict))
  geom_density(color = "red", aes(x = log(conflict + 1))) 
# [X] Explanatory variables must be continuous, dichotomous or ordinal
str(grid_land[, .(conflict, towers, point_park, point_pow, gdp_sum, gdp_avg,
                  point_school, power, #clean_power, 
                  roads, density_sum,
                  density_avg, healthcare, urban_bin)
])
# [X] Observations must be independent

# 2. Build models
# Notes: 
#   - All explanatory variables follow a poisson distribution and should be 
#     logged. This is in part because of the nature of the distribution of 
#     "things". For example, conflict and public goods are bi-products of humans
#     and are expected to exist where people live. 

# Model 0: Baseline of control variables
m0 <- glm(conflict ~ log(gdp_avg + 1) + log(density_avg + 1) + urban_bin + admin,  
          data = grid_land, family = 'poisson')

# Model 1: Conflict as a function of communication
m1 <- glm(conflict ~ log(towers + 1) +
            # Control variables
            log(gdp_avg + 1) + log(density_avg + 1) + urban_bin + admin,  
          data = grid_land, family = 'poisson')
# Model 2: Conflict as a function of public goods that connect people, those
# may fall into the infrastructure (roads, power, clean_power), environment 
# (point_parks), education and science (point_pow, point_school)
# Notes: 
#   - Missing as they don't connect people in the real world: Security 
#     (military, police) and Public Health (hospital, life)
m2 <- glm(conflict ~ log(roads + 1) +
            log(power + 1) + #log(clean_power + 1) +
            log(point_park + 1) + log(point_pow + 1) + log(point_school + 1) +
            log(healthcare + 1) + 
            # Control variables
            log(gdp_avg + 1) + log(density_avg + 1) + urban_bin + admin,  
          data = grid_land, family = 'poisson')

# Model 3: Mixed model communication infrastructure and public goods that 
# connect people
# m3 <- glm(conflict ~ log(towers + 1) + log(roads + 1) + 
#             log(power + 1) +
#             #log(clean_power + 1) + 
#             log(point_park + 1) + log(point_pow + 1) + 
#             log(point_school + 1) + log(healthcare + 1) +
#             # Control variables
#             log(gdp_avg + 1) + log(density_avg + 1) + urban_bin,  
#           data = grid_land, family = 'poisson')

# m4 <- glm(conflict ~ log(towers + 1) + log(roads + 1) +
#             + log(point_pow + 1) +
#             log(point_school + 1) + log(healthcare + 1) +
#             # Control variables
#             log(gdp_avg + 1) + log(density_avg + 1) + urban_bin,
#           data = grid_land, family = 'poisson')

m6 <- glm(conflict ~ log(towers + 1) + log(roads + 1) + log(power + 1) + 
            log(point_park + 1) + log(point_pow + 1) + log(point_school + 1) +
            log(healthcare + 1) + 
            # Interactions
            log(towers + 1) * log(roads + 1) + log(towers + 1) * log(power + 1) + 
            log(towers + 1) * log(point_park + 1) + log(towers + 1) * log(point_pow + 1) + 
            log(towers + 1) * log(point_school + 1) + log(towers + 1) * log(healthcare + 1) +
            # Control variables
            log(gdp_avg + 1) + log(density_avg + 1) + urban_bin + admin,
          data = grid_land, family = 'poisson')

stargazer::stargazer(m0, m1, m2, m6,
                     type = "html", out = "inst/modeling/output.html",
                     dep.var.labels = "Conflict",
                     single.row = TRUE,
                     omit = c("adminEl Salvador", "adminCosta Rica", "adminGuatemala",
                              "adminHonduras", "adminNicaragua", "adminPanama"),
          # covariate.labels = c("Cellphone Towers",
          #                      "Roads",
          #                      "Power Generation",
          #                      "Parks",
          #                      "Places of Worship",
          #                      "Schools",
          #                      "Healthcare",
          #                      "GDP PPP",
          #                      "Population Density",
          #                      "Urbanized",
          #                      "Cellphone Towers x Roads",
          #                      "Cellphone Towers x Power Generation",
          #                      "Cellphone Towers x Parks",
          #                      "Cellphone Towers x Places of Worship",
          #                      "Cellphone Towers x Schools",
          #                      "Cellphone Towers x Healthcare",
          #                      "Constant"
          # ),
          omit.stat = c("LL"), 
          add.lines = list(c("Fixed effects", "Yes", "Yes", "Yes", "Yes")),
          digits = 2, 
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          model.numbers = FALSE
)

ggplot(data = grid_land, aes(x = log(towers +1 ), y = log(roads + 1))) +
  stat_density_2d(aes(fill=..level..), geom = "raster", countour = F)

# Hypothesis:
# Areas with a higher investment in public goods (e.g., parks, telecommunication
# infrastructure, roads) and higher financial capital will experience a lower 
# probability of conflict. 

# Table interpretation:
#   - Model interpretation: 
#     + Model 1 is a baseline which only includes control variables
#     + Model 2 adds cellphone towers to the baseline model
#     + Model 3 removes cellphone towers but adds public goods to the baseline
#     + Model 4 includes all variables to account for both communication and  
#       the effect of public goods


# Substantive Effects ----------------------------------------------------------
vars <- c("towers",
          "point_park",
          "point_pow", 
          "point_school")

pd <- predict_diff(m6, xvar = vars, xquantile = c(0.05, 0.95)) |> setDT()

pd[, labels := c("Cellphone Towers",
                 "Parks", 
                 "Places of Worship", 
                 "Schools")
][, my_fill := ifelse(pred_diff < 0, "blue", "red")]

pred_plot <- ggplot(data = pd, aes(x = reorder(labels, -pred_diff), y = pred_diff,
                      color = my_fill, label = round(pred_diff, 3))) +
  geom_point(stat = "identity", size = 12) +
  geom_segment(aes(y = 0, x = labels, yend = pred_diff, xend = labels),
               lwd = 4) +
  geom_text(color = "white", size = 3.5) +
  coord_flip() + 
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = NULL, 
       y = "Difference in Collective Violence",
       title = "Model Variables Substantive Effect on Collective Violence",
       # subtitle = "Unit of Analysis: 20 Km{2} Grid Cell \n",
       subtitle = bquote('Unit of Analysis: Grid Cell 20'~Km^2), 
       caption = "\n Replication data and code: https://github.com/cjcallag/da3450proj") +
  theme(axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 15),
        legend.text = element_text(size = 13))

ggsave(pred_plot, filename = "inst/modeling/substantive.png", width = 10,
       height = 6)


# https://nicholasrjenkins.science/post/data_viz_r/data_visualization_r/
library(broom)
library(tidyverse)

model_results <- bind_rows(tidy(m1) %>% mutate(model = "Model 2"),
                           tidy(m2) %>% mutate(model = "Model 3"),
                           tidy(m6) %>% mutate(model = "Model 4"),
                           )

fit <- model_results %>%
  filter(!term %in% c("(Intercept)", "adminCosta Rica", "adminEl Salvador",
                      "adminGuatemala", "adminHonduras", "adminNicaragua",
                      "adminPanama")) %>%
  mutate(term = fct_relevel(term, "log(towers + 1)", "log(roads + 1)",
                            "log(power + 1)", "log(point_park + 1)",
                            "log(point_pow + 1)", "log(point_school + 1)",
                            "log(healthcare + 1)", "log(gdp_avg + 1)",
                            "log(density_avg + 1)", "urban_bin", 
                            "log(towers + 1):log(roads + 1)",
                            "log(towers + 1):log(power + 1)",
                            "log(towers + 1):log(point_park + 1)",
                            "log(towers + 1):log(point_pow + 1)",
                            "log(towers + 1):log(point_school + 1)",
                            "log(towers + 1):log(healthcare + 1)"),
         color = ifelse(estimate < 0, "blue", "red")) %>%
  ggplot(mapping = aes(x = estimate, y = term, # xmin = conf.low, xmax = conf.high,
                       color = color, 
                       shape = model)) +
  geom_vline(xintercept = 0, color = "slategrey", linetype = "dashed") +
  # geom_pointrange(size = 1, alpha = 0.75, position = position_dodge(width = 0.5)) +
  geom_point(size = 3, alpha = 0.5, position = position_dodge(width = 0.5)) +
  labs(title = "Model Estimates of Predictor Variables on\nCollective Violence",
       x = "Coefficient Estimate",
       y = "",
       caption = "Models fit with Poisson Regression.\n Intercept removed.",
       shape = "") +
  scale_y_discrete(limits = rev, labels = rev(c("Cellphone Towers",
                                                "Roads",
                                                "Power Generation",
                                                "Parks",
                                                "Places of Worship", "Schools",
                                                "Healthcare", "GDP PPP",
                                                "Population Density",
                                                "Urbanized",
                                                "Cellphone Towers x Roads",
                                                "Cellphone Towers x Power Generation",
                                                "Cellphone Towers x Parks",
                                                "Cellphone Towers x Places of Worship",
                                                "Cellphone Towers x Schools",
                                                "Cellphone Towers x Healthcare"
                                                )
                                              )
                   ) +
  guides(color = "none") +
  theme_minimal() +
  theme(legend.position = "right", axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 15),
        legend.text = element_text(size = 13))
fit
ggsave(fit, filename = "inst/modeling/fit.png", width = 8, height  = 8)


### Predictive Effects
predict_model(m6, xvar = "roads", zvar = "towers", conf =  0.95,
                         nsim = 1000,
                      xquantile = c(0.5, 0.95), zlog = FALSE, zquantile = c(0.5, 0.95)
              )[["pred"]]  |>
  ggplot(aes(x = roads, y = pred, ymin = lmin, ymax = umax,
             color = factor(round(towers, 0)))) +
  geom_ribbon(alpha = 0.25, aes(fill = factor(round(towers, 0)))) + 
  geom_line(lwd = 1) +
  theme_minimal() +
  labs(x = "Number of Roads", y = "Predicted Conflict", 
       color = "Towers") + 
  guides(fill = "none") +
  theme(legend.position = "bottom")
 
p_parks <- predict_model(m6, xvar = "point_park", zvar = "towers", conf =  0.95,
              nsim = 1000,
              xquantile = c(0.5, 0.95), zlog = FALSE, zquantile = c(0.5, 0.95))[["pred"]] |>
  ggplot(aes(x = point_park, y = pred, ymin = lmin, ymax = umax,
             color = factor(round(towers, 0)))) +
  geom_ribbon(alpha = 0.25, aes(fill = factor(round(towers, 0)))) + 
  geom_line(lwd = 1) +
  theme_minimal() +
  labs(x = "Number of Parks", y = "Predicted Conflict", 
       color = "Towers") + 
  # xlim(0, 2000) +
  theme(legend.position = "bottom") +
  guides(fill = "none")

p_pow <- predict_model(m6, xvar = "point_pow", zvar = "towers", conf =  0.95, 
                       nsim = 1000,
                       xquantile = c(0.5, 0.95), zlog = FALSE, zquantile = c(0.5, 0.95))[["pred"]] |>
  ggplot(aes(x = point_pow, y = pred, ymin = lmin, ymax = umax,
             color = factor(round(towers, 0)))) +
  geom_ribbon(alpha = 0.25, aes(fill = factor(round(towers, 0)))) + 
  geom_line(lwd = 1) +
  theme_minimal() +
  labs(x = "Number of Places of Worship", y = "Predicted Conflict", 
       color = "Log Towers") + 
  # xlim(0, 2000) +
  theme(legend.position = "bottom") +
  guides(fill = "none")

p_schools <- predict_model(m6, xvar = "point_school", zvar = "towers", conf =  0.95,
                           nsim = 1000,
                           xquantile = c(0.5, 0.95), zlog = FALSE, zquantile = c(0.5, 0.95))[["pred"]] |>
  ggplot(aes(x = point_school, y = pred, ymin = lmin, ymax = umax,
             color = factor(round(towers, 0)))) +
  geom_ribbon(alpha = 0.25, aes(fill = factor(round(towers, 0)))) + 
  geom_line(lwd = 1) +
  theme_minimal() +
  labs(x = "Number of Schools", y = "Predicted Conflict", 
       color = "Towers") + 
  # xlim(0, 2000) +
  theme(legend.position = "bottom") +
  guides(fill = "none")

p_health <- predict_model(m6, xvar = "healthcare", zvar = "towers", conf =  0.95, nsim = 1000,
                          xquantile = c(0.5, 0.95), zlog = FALSE, zquantile = c(0.5, 0.95))[["pred"]] |>
  ggplot(aes(x = healthcare, y = pred, ymin = lmin, ymax = umax,
             color = factor(round(towers, 0)))) +
  geom_ribbon(alpha = 0.25, aes(fill = factor(round(towers, 0)))) + 
  geom_line(lwd = 1) +
  theme_minimal() +
  labs(x = "Number of Healthcare Facilities", y = "Predicted Conflict", 
       color = "Towers") + 
  # xlim(0, 2000) +
  theme(legend.position = "bottom") +
  guides(fill = "none")

ggsave(p_parks, filename = "inst/modeling/p_parks.png", width = 5, height = 5)
ggsave(p_pow, filename = "inst/modeling/p_pow.png", width = 5, height = 5)
ggsave(p_schools, filename = "inst/modeling/p_school.png", width = 5, height = 5)
ggsave(p_health, filename = "inst/modeling/p_health.png", width = 5, height = 5)
ggsave(p_roads, filename = "inst/modeling/p_roads.png", width = 5, height = 5)


# grid$p_towers <- predict(m4,
#                     newdata = m4$data[, c("towers", "roads", "point_pow",
#                                           "point_school", "healthcare", "conflict") :=
#                                         .(towers, mean(roads), mean(point_pow),
#                                           mean(point_school), mean(healthcare), conflict)])
# ggplot(data = grid, aes(x = towers, y = p_towers)) +
#   geom_line() +
#   geom_smooth()

