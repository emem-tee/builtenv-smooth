road_metrics <- function(dtfm) {
  
  require(sf)
  require(tidyverse)
  require(units)
  
  # Read in the Georgia Road data
  ga_roads <- st_read(here::here("Data/Roads/tl_2019_13_prisecroads", "tl_2019_13_prisecroads.shp"))
  st_crs(ga_roads)
  
  # Read in the georgia cities (better than centroids because certain population center)
  ga_cities <- st_read(here::here("Data/Cities/", "Cities_Georgia.shp"))
  st_crs(ga_cities)
  
  # Read in the list of county seats to identify the cities
  county_seats <- read_csv(here::here("Data/CountySeats/county-seats.csv"))
  
  # Clean Up County Seats ---------------------------------------------------
  
  # Right side is the "true seat" and left side is the closest city
  # This happens because of citycounties, spacing issues, or unincorporated seats
  lookup <- c(
    `Georgetown-Quitman County` = "Georgetown",
    `Augusta-Richmond County (balance)` = "Augusta",
    `Cusseta-Chattahoochee County` = "Cusseta",
    `Athens-Clarke County (balance)` = "Athens",
    `La Grange` = "LaGrange",
    `Webster County` = "Preston",
    `Roberta` = "Knoxville")
  
  # Only keep the cities that are county seats
  county_seats_noManual <- ga_cities %>% 
    inner_join(county_seats, by = c("Name" = "SEAT"))
  
  # Join the county seats with the georgia cities
  county_seats_geo <- ga_cities %>% 
    mutate(Name_Seat = case_when(
      Name == "Georgetown-Quitman County" ~ "Georgetown",
      Name == "Augusta-Richmond County (balance)" ~ "Augusta",
      Name == "Cusseta-Chattahoochee County" ~ "Cusseta",
      Name == "Athens-Clarke County (balance)" ~ "Athens",
      Name == "Webster County" ~ "Preston",
      Name == "Roberta" ~ "Knoxville",
      .default = Name)
    ) %>% #filter(Name %in% names(lookup)) %>% 
    inner_join(county_seats, by = c("Name_Seat" = "SEAT"))
  
  # Makes the centroid computation possible
  county_seats_geo_v <- st_make_valid(county_seats_geo)
  
  # Name the centroids so they can be used for distance metric
  county_seats_centroids <- st_centroid(county_seats_geo_v)
  
  # Get the correct roads ---------------------------------------------------
  
  lookup <- c(
    `C` = 'County',
    `I` = 'Interstate',
    `M` = 'Common Name',
    `O` = 'Other',
    `S` = 'State recognized',
    `U` = 'U.S.')
  
  ga_roads_clean <- ga_roads %>% 
    mutate(RTTYP_f = factor(lookup[RTTYP], levels = c('County',
                                                      'Interstate',
                                                      'Common Name',
                                                      'Other',
                                                      'State recognized',
                                                      'U.S.')))
  
  # How far are the county centroids from the highway---------------------------

  # How far are either of these from the highway as a bird flies
  ga_roads_projected <- st_transform(ga_roads, st_crs(dtfm))

  county_seats_projected <- st_transform(county_seats_geo, st_crs(dtfm))
  
  # Get the centroid from the county seats  
  county_seats_projected <- st_centroid(st_make_valid(county_seats_projected))
  
  ga_roads_us <- ga_roads_projected %>%
    filter(RTTYP == "U")
  
  ga_roads_interst <- ga_roads_projected %>%
    filter(RTTYP == "I")
  
  # produces a list of indeces of the closest object
  feat <- st_nearest_feature(county_seats_projected, ga_roads_interst)
  
  # Computes the distance between the county seat and the closest interstate
  min_dist <- st_distance(county_seats_projected, 
                          ga_roads_interst[feat,],
                          by_element = TRUE)
 
  # Add the distance to the data frame
  dtfm_metrics <- county_seats_projected %>%
    mutate(dist_to_usroad = as.numeric(min_dist)) %>% 
    st_drop_geometry() %>% 
    dplyr::select(Name, Name_Seat, COUNTY, dist_to_usroad) 
  
  dtfm <- dtfm %>% 
    left_join(dtfm_metrics, by = c("NAME" = "COUNTY"))
  
  return(dtfm)
  
}