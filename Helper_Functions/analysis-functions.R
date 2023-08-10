plot_maps <- function(map_data, model_object, response = Incidence, response_pop = Incidence_pop) {
  map_data <- mutate(map_data,
                        fitted_values = model_object$fitted.values)
  
  ggplot(map_data, aes(x = mortality, y = fitted_values)) +
    geom_point() +
    theme_bw()
  
  test_vals <- tibble(id = 1:length(model_object$fitted.values), 
                      fitted_inc = model_object$fitted.values,
                      true_inc = map_data %>% pull({{response}})) %>% 
    pivot_longer(c(fitted_inc, true_inc))
  
  ggplot(test_vals, aes(x = id, y = value, group = name, colour = name)) +
    geom_point() +
    theme_bw()

  # Okay the scale of the fitted values is way different from the mortality
  # This is either a model convergence issue or something else. Maybe
  map_data <- map_data %>% 
    mutate(across(c({{response}}, fitted_values), ~ .x / population * 100000, 
                  .names = "{.col}_pop"))
  
  p1 <- ggplot(map_data) +
    geom_sf(aes(fill = {{response_pop}})) +
    scale_fill_viridis() 
  
  p2 <- ggplot(map_data) +
    geom_sf(aes(fill = fitted_values_pop)) +
    scale_fill_viridis() 
  
  plot_grid(p1, p2, labels = c("Unsmoothed", "Smoothed"))
  
}
