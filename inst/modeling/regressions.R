library(data.table)
library(ggplot2)
library(MixMerge)
library(stargazer)

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
    )]

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
m0 <- glm(conflict ~ log(gdp_avg + 1) + log(density_avg + 1) + urban_bin,  
          data = grid_land, family = 'poisson')

# Model 1: Conflict as a function of communication
m1 <- glm(conflict ~ log(towers + 1) +
            # Control variables
            log(gdp_avg + 1) + log(density_avg + 1) + urban_bin,  
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
            log(gdp_avg + 1) + log(density_avg + 1) + urban_bin,  
          data = grid_land, family = 'poisson')

# Model 3: Mixed model communication infrastructure and public goods that 
# connect people
m3 <- glm(conflict ~ log(towers + 1) + log(roads + 1) + 
            log(power + 1) +
            #log(clean_power + 1) + 
            log(point_park + 1) + log(point_pow + 1) + 
            log(point_school + 1) + log(healthcare + 1) +
            # Control variables
            log(gdp_avg + 1) + log(density_avg + 1) + urban_bin,  
          data = grid_land, family = 'poisson')

m4 <- glm(conflict ~ log(towers + 1) + log(roads + 1) + 
            log(point_park + 1) + log(point_pow + 1) + 
            log(point_school + 1) + log(healthcare + 1) +
            # Control variables
            log(gdp_avg + 1) + log(density_avg + 1) + urban_bin,  
          data = grid_land, family = 'poisson')

stargazer::stargazer(m0, m1, m2, m3, m4,  type = "html", out = "inst/modeling/output.html",
          dep.var.labels = "Conflict",
          covariate.labels = c("Cellphone Towers",
                               "Roads",
                               "Power Generation",
                               # "Clean Power Generation",
                               "Parks",
                               "Places of Worship",
                               "Schools",
                               "Healthcare",
                               "GDP PPP",
                               "Population Density",
                               "Urbanized"
          )
)

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
vars <- c("towers", "roads", 
          "point_park", "point_pow", 
          "point_school", "healthcare")
pd <- predict_diff(m4, xvar = vars, xquantile = c(0.05, 0.95)) |> setDT()

pd[, labels := c("Cellphone Towers", "Roads", 
                 "Parks", "Places of Worship", 
                 "Schools", "Healthcare")
][, my_fill := ifelse(pred_diff < 0, "blue", "red")]

pred_plot <- ggplot(data = pd, aes(x = reorder(labels, -pred_diff), y = pred_diff,
                      color = my_fill, label = round(pred_diff, 2))) +
  geom_point(stat = "identity", size = 12) +
  geom_segment(aes(y = 0, x = labels, yend = pred_diff, xend = labels),
               lwd = 4) +
  geom_text(color = "white", size = 3.5) +
  coord_flip() + 
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = NULL, 
       y = "Difference in Collective Violence",
       title = "Model Variables Substantive Effect on Conflict",
       # subtitle = "Unit of Analysis: 20 Km{2} Grid Cell \n",
       subtitle = bquote('Unit of Analysis: Grid Cell 20'~Km^2), 
       caption = "\n Replication data and code: https://github.com/cjcallag/da3450proj")

ggsave(pred_plot, filename = "inst/modeling/substantive.png", width = 10,
       height = 6)


# https://nicholasrjenkins.science/post/data_viz_r/data_visualization_r/
library(broom)
library(tidyverse)

model_results <- bind_rows(# tidy(m1, conf.int = TRUE) %>% mutate(model = "Model 2"),
#                            tidy(m2, conf.int = TRUE) %>% mutate(model = "Model 3"),
                           tidy(m3, conf.int = TRUE) %>% mutate(model = "Model 4"),
                           tidy(m4, conf.int = TRUE) %>% mutate(model = "Model 5"))

fit <- model_results %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_relevel(term, "log(towers + 1)", "log(roads + 1)",
                            "log(power + 1)", "log(point_park + 1)",
                            "log(point_pow + 1)", "log(point_school + 1)",
                            "log(healthcare + 1)", "log(gdp_avg + 1)",
                            "log(density_avg + 1)", "urban_bin"),
         color = ifelse(estimate < 0, "blue", "red")) %>%
  ggplot(mapping = aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high,
                       color = color, shape = model)) +
  geom_vline(xintercept = 0, color = "slategrey", linetype = "dashed") +
  geom_pointrange(size = 1.5, alpha = 0.75, position = position_dodge(width = 0.5)) +
  labs(title = "Model Estimates of Predictor Variables on Collective Violence",
       x = "Coefficient Estimate",
       y = "",
       caption = "Models fit with Poisson Regression.\n Error bars show the 95% confidence interval.\n Intercept removed.",
       shape = "") +
  scale_y_discrete(limits = rev, labels = rev(c("Cellphone Towers", "Power Generation",
                                                "Roads", "Parks", 
                                                "Places of Worship", "Schools",
                                                "Healthcare", "GDP PPP",
                                                "Population Density",
                                                "Urbanized"))
                   ) +
  guides(color = "none") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 13),
        plot.title = element_text(size = 15))
ggsave(fit, filename = "inst/modeling/fit.png", width = 8, height  = 10)


