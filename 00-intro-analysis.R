source(here::here("Helper_Functions/data-readin.R"))
library(ggplot2)
library(CARBayes)
library(tidyverse)
library(viridis)
library(cowplot)

# Poverty Data (DP03_119PE)
# Vacant housing units (DP04_0003PE)
# Percent!!EMPLOYMENT STATUS!!Civilian labor force!!Unemployment Rate
ga_map_data <- data_readin(acs_vars = "DP03_0119PE,DP04_0003PE,DP03_0009PE")

## Bivariate-----------------------------------------------

Hmisc::rcorr(as.matrix(ga_map_data[,c("pct_poverty", "vacancy_rate", "unemployment_rate")]))

ggplot(data = ga_map_data, aes(x = vacancy_rate, y = unemployment_rate)) +
  geom_point()

# Bayes---------------------------------------------------

ga_map_nb <- spdep::poly2nb(ga_map_data, queen = F)

W_mt <- spdep::nb2mat(ga_map_nb, style = "B")

# Creates an empty numeric vector to hold the values of the smoothing
aggData <- data.frame(MortRateSmooth = numeric(length(ga_map_nb)))

# Add row names to the data frame for later merging
# row.names(aggData) <- row.names(gaCounty_n)

# Combine the aggregate data with the original population data
# gaCountyData <- cbind(popData,aggData)

# Add the data with that new vector to gaCounty
# arg 1 is the SpatialPolygons object and arg 2 is the data set
# gaCounty_df <- sp::SpatialPolygonsDataFrame(gaCounty_n, gaCountyData)

# Print the smoothed map
ps <- ggplot(ga_map_data) + 
  geom_sf(aes(fill = Incidence)) +
  labs(title = "Adjacency Smoothed\n Mortality Rates 19")

print(ps)

form <- "Incidence ~ pct_poverty + vacancy_rate + unemployment_rate"

model.spatial <- S.CARleroux(as.formula(form), family = "gaussian",
                             data = ga_map_data,
                             W = W_mt, burnin = 20000,
                             n.sample = 100000,
                             thin = 10)

print(summary(model.spatial))
model.spatial$summary.results

ga_map_data <- mutate(ga_map_data,
                      fitted_values = model.spatial$fitted.values)

ggplot(ga_map_data, aes(x = Incidence, y = fitted_values)) +
  geom_point() +
  theme_bw()

test_vals <- tibble(id = 1:length(model.spatial$fitted.values), 
                    fitted_inc = model.spatial$fitted.values,
                    true_inc = ga_map_data$Incidence) %>% 
  pivot_longer(c(fitted_inc, true_inc))

ggplot(test_vals, aes(x = id, y = value, group = name, colour = name)) +
  geom_point() +
  theme_bw()

p1 <- ggplot(ga_map_data) +
  geom_sf(aes(fill = Incidence)) +
  scale_fill_viridis() +
  labs("Unsmoothed Incidence")

p2 <- ggplot(ga_map_data) +
  geom_sf(aes(fill = fitted_values)) +
  scale_fill_viridis() +
  labs("Smoothed Incidence")

plot_grid(p1, p2, labels = c("Unsmoothed", "Smoothed"))
