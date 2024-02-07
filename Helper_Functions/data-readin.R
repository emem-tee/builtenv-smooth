data_readin <- function(acs_vars = "DP03_0119PE,DP04_0003PE,DP03_0009PE", acs_yr = 2021, mort_year = 2020){
  # data readin
  require(readxl)
  require(dplyr)
  require(sf)
  source(here::here("Data/ACS/acs-api.R"))
  source(here::here("Helper_Functions/road-metrics.R"))
  
  
  ##-Predictors------------------------------------------------------------------

  acs <- get_acs(acs_yr = acs_yr, acs_vars = acs_vars, resolution = "county")
  
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
  
  if(mort_year == 2017){
  mort_data <- read_xlsx(here::here("Data/Opioid/PopData.xlsx"))
  } else if(mort_year == 2020) {
    mort_data <- read_xlsx(here::here("Data/Opioid/PopData20.xlsx"))
  } 
  else stop(paste0(mort_year, " is an invalid year. Choose 2020 or 2017 as a numeric variable."))
  
  full_data <- mort_data %>% 
    right_join(acs_georgia, by = c("County" = "NAME")) %>% 
    rename(NAME = County)
  
  ##-Data Readin-----------------------------------------------------------------
  
  rucc <- read_xlsx(here::here("Data/NCHS/rucc_codes/NCHSURCodes2013.xlsx")) %>%
    filter(`State Abr.` == "GA") %>% 
    mutate(county = substr(as.character(`FIPS code`),3,5),
           rucc_code13 = factor(`2013 code`, labels = c("lcm","lfm","mm","sm","mi","non"))) %>% 
    dplyr::select(county, rucc_code13, `2013 code`) %>% 
    rename(rucc_code13_n = `2013 code`)
  
  # rucc %>% count(rucc_code13, `2013 code`)
    
  full_data_codes <- rucc %>% right_join(full_data)
    
  ##-Map-------------------------------------------------------------------------
  
  ga_map <- st_read(here::here("Data/Map/County.shp"),
                    quiet = T)
  
  ga_map_data <- ga_map %>% 
    left_join(full_data_codes) %>% 
    mutate(incidence = Mortality / Population) %>% 
    rename(mortality = Mortality,
           population = Population)
  
  ##-Add Road Metrics------------------------------------------------------------
  
  ga_map_data <- road_metrics(ga_map_data)
  
  ##-Standardize Variables------------------------------------------------------
  
  standardize <- function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
  
  ga_map_data <- ga_map_data %>% 
    mutate(across(c(pct_poverty, vacancy_rate, unemployment_rate, dist_to_usroad, dist_to_treatment), 
                  ~  standardize(.x),
                  .names = "{.col}_std"))
  
  return(ga_map_data)

}