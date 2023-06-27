data_readin <- function(acs_vars = "DP03_0119PE,DP04_0003PE,DP03_0009PE"){
  # data readin
  require(readxl)
  require(dplyr)
  require(sf)
  source(here::here("Data/ACS/acs-api.R"))
  
  ##-Predictors------------------------------------------------------------------

  acs <- get_acs(2021, acs_vars = acs_vars, resolution = "county")
  
  # acs %>% 
  #   filter(state == "13") %>% count()
  #     n
  # 1 159
  
  acs_georgia <- acs %>% 
    filter(state == "13") %>% 
    mutate(NAME = trimws(gsub("\ County, Georgia"," ", NAME))) %>% 
    mutate(
      pct_poverty = as.numeric(DP03_0119PE),
      vacancy_rate = as.numeric(DP04_0003PE),
      unemployment_rate = as.numeric(DP03_0009PE)
    )
  
  ##-Opioid----------------------------------------------------------------------
  
  mort_data <- read_xlsx(here::here("Data/Opioid/PopData.xlsx"))
  
  full_data <- mort_data %>% 
    right_join(acs_georgia, by = c("County" = "NAME")) %>% 
    rename(NAME = County)
  
  ##-Map-------------------------------------------------------------------------
  
  ga_map <- st_read(here::here("Data/Map/County.shp"),
                    quiet = T)
  
  ga_map_data <- ga_map %>% 
    left_join(full_data) %>% 
    mutate(Incidence = Mortality / Population)
  
  return(ga_map_data)

}