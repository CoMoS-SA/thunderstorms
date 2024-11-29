library(readxl)
library(tidyverse)
library(sf)
library(magrittr)
library(tictoc)
library(tmaptools, warn.conflicts =FALSE)


rm(list = ls())

data_storm <- readRDS("data/clean_hail_step3.rds")
data_wages <- readRDS(file="data/wages_employment.rds")
data_wages_state <- readRDS(file="data/wages_employment_state.rds")
data_income <- readRDS(file="data/income.rds")
data_income_state <- readRDS(file="data/income_state.rds")
data_temp_prec <- readRDS(file="data/cleaned_temp_prec.rds")

data_wages_state %<>% 
  rename_with(~ paste0("ST_", .x, recycle0 = TRUE), AAE_TOT_TOT:GR_AAP_PRIV_UNCL)

data_income_state %<>% 
  rename_with(~ paste0("ST_", .x, recycle0 = TRUE), INCOME:GR_INCOME_PC)

data_temp_prec %<>% filter(as.numeric(YEAR)<=2019)
data_wages %<>% filter(as.numeric(YEAR)<=2019)
data_wages_state %<>% filter(as.numeric(YEAR)<=2019)
data_storm %<>% filter(YEAR<=2019) %>% mutate(YEAR=as.character(YEAR))
data_income %<>% filter(YEAR<=2019) %>% mutate(YEAR=as.character(YEAR))
data_income_state %<>% filter(YEAR<=2019) %>% mutate(YEAR=as.character(YEAR))

data_temp_prec %<>% filter(FIPS!="08014")
data_wages %<>% filter(FIPS!="08014")
data_income %<>% filter(FIPS!="08014")
data_storm %<>% filter(FIPS!="08014")

#### Aggregate Hail ####

data_storm %>% 
  group_by(FIPS_AG, YEAR) %>% 
  summarise(
    M1 = n(),
    M2 = max(MAGNITUDE),
    M3 = sum(MAGNITUDE^3),
    M4 = sum(MAGNITUDE^2),
    COUNTY_AREA  = COUNTY_AREA[1]
  ) %>% ungroup() -> storm_aggr

storm_aggr %>% 
  distinct(FIPS_AG, COUNTY_AREA) %>% 
  summarise(
    mean = mean(COUNTY_AREA)
  ) %>% pull(mean) -> mean_county_area

storm_aggr %<>% 
  mutate(
    S1 = ((M1/COUNTY_AREA)*mean_county_area),
    S2 = ((M2/COUNTY_AREA)*mean_county_area)*(2.54),
    S3 = ((M3/COUNTY_AREA)*mean_county_area)*(2.54^3),
    S4 = ((M4/COUNTY_AREA)*mean_county_area)*(2.54^2)
  )

setdiff(storm_aggr$FIPS_AG, data_temp_prec$FIPS)
setdiff(data_temp_prec$FIPS, storm_aggr$FIPS_AG)

data_temp_prec %>% 
  left_join(
    .,
    data_income %>% rename(AREA = GeoName) %>% select(-PCE_DEFLATOR),
    by=c("YEAR", "FIPS")
  ) %>%
  left_join(
    .,
    data_wages %>% select(FIPS, YEAR,
                          AAE_TOT_TOT, GR_AAE_TOT_TOT, 
                          ATW_TOT_TOT, GR_ATW_TOT_TOT, AAWW_TOT_TOT, GR_AAWW_TOT_TOT,
                          AAP_TOT_TOT, GR_AAP_TOT_TOT),
    by=c("YEAR", "FIPS")
  ) %>% 
  left_join(
    .,
    data_wages_state %>% select(STATE, YEAR,  
                                ST_AAE_TOT_TOT, ST_GR_AAE_TOT_TOT, 
                                ST_ATW_TOT_TOT, ST_GR_ATW_TOT_TOT, ST_AAWW_TOT_TOT, ST_GR_AAWW_TOT_TOT,
                                ST_AAP_TOT_TOT, ST_GR_AAP_TOT_TOT, ST_AAE_TOT_TOT),
    by=c("YEAR", "STATE")
  ) %>% 
  left_join(
    .,
    data_income_state %>% select(-FIPS),
    by=c("YEAR", "STATE")
  ) -> panel

panel %<>% 
  left_join(
    .,
    storm_aggr %>% select(-COUNTY_AREA),
    by = c("FIPS"="FIPS_AG", "YEAR"="YEAR")
  )

panel %<>% 
  mutate(
    across(
      colnames(storm_aggr)[!(colnames(storm_aggr)%in% c("FIPS_AG", "COUNTY_AREA", "YEAR"))],
      ~ ifelse(is.na(.x), 0, .x)
    )
  ) %>% relocate(FIPS, AREA, STATE, YEAR, M1:S4) 

#Both FIPS and AREA can be used as unique county identifiers. 

#### Add coordinates ####

county_shape <- st_read("utilities/c_10nv20/") 

panel %>% 
  filter(FIPS%in%setdiff(panel$FIPS, county_shape$FIPS)) %>% 
  select(FIPS, AREA) %>% 
  distinct() %>% print(n=25)

county_shape %>% 
  filter(STATE=="VA") %>% 
  filter(COUNTYNAME%in%c(
    "Albemarle", "City of Charlottesville",
    "Alleghany", "City of Covington",
    "Augusta", "City of Staunton", "City of Waynesboro",
    "Campbell", "City of Lynchburg",
    "Carroll", "City of Galax",
    "Dinwiddie", "City of Colonial Heights", "City of Petersburg",
    "Fairfax", "City of Fairfax", "City of Falls Church",
    "Frederick", "City of Winchester",
    "Greensville", "City of Emporia",
    "Henry", "City of Martinsville",
    "James City", "City of Williamsburg",
    "Montgomery", "City of Radford",
    "Pittsylvania", "City of Danville",
    "Prince George", "City of Hopewell",
    "Prince William", "City of Manassas", "City of Manassas Park",
    "Roanoke", "City of Salem",
    "Rockbridge", "City of Buena Vista", "City of Lexington",
    "Rockingham", "City of Harrisonburg",
    "Southampton", "City of Franklin",
    "Spotsylvania", "City of Fredericksburg",
    "Washington", "City of Bristol",
    "Wise", "City of Norton",
    "York", "City of Poquoson"
  )) %>% 
  mutate(
    group=case_when(
      COUNTYNAME %in% c("Albemarle", "City of Charlottesville") ~ 51901,
      COUNTYNAME %in% c("Alleghany", "City of Covington") ~ 51903,
      COUNTYNAME %in% c("Augusta", "City of Staunton", "City of Waynesboro") ~ 51907,
      COUNTYNAME %in% c("Campbell", "City of Lynchburg") ~ 51911,
      COUNTYNAME %in% c("Carroll", "City of Galax") ~ 51913,
      COUNTYNAME %in% c("Dinwiddie", "City of Colonial Heights", "City of Petersburg") ~ 51918,
      COUNTYNAME %in% c("Fairfax", "City of Fairfax", "City of Falls Church") ~ 51919,
      COUNTYNAME %in% c("Frederick", "City of Winchester") ~ 51921,
      COUNTYNAME %in% c("Greensville", "City of Emporia") ~ 51923,
      COUNTYNAME %in% c("Henry", "City of Martinsville") ~ 51929,
      COUNTYNAME %in% c("James City", "City of Williamsburg") ~ 51931,
      COUNTYNAME %in% c("Montgomery", "City of Radford") ~ 51933,
      COUNTYNAME %in% c("Pittsylvania", "City of Danville") ~ 51939,
      COUNTYNAME %in% c("Prince George", "City of Hopewell") ~ 51941,
      COUNTYNAME %in% c("Prince William", "City of Manassas", "City of Manassas Park") ~ 51942,
      COUNTYNAME %in% c("Roanoke", "City of Salem") ~ 51944,
      COUNTYNAME %in% c("Rockbridge", "City of Buena Vista", "City of Lexington") ~ 51945,
      COUNTYNAME %in% c("Rockingham", "City of Harrisonburg") ~ 51947,
      COUNTYNAME %in% c("Southampton", "City of Franklin") ~ 51949,
      COUNTYNAME %in% c("Spotsylvania", "City of Fredericksburg") ~ 51951,
      COUNTYNAME %in% c("Washington", "City of Bristol") ~ 51953,
      COUNTYNAME %in% c("Wise", "City of Norton") ~ 51955,
      COUNTYNAME %in% c("York", "City of Poquoson") ~ 51958
    )
  ) %>% 
  group_by(group) %>% 
  summarise(
    geometry = st_union(geometry)
  ) %>% 
  mutate(
    centroid = st_centroid(geometry)
  ) %>% 
  mutate(
    st_coordinates(centroid) %>% as_tibble()
  ) %>% 
  as_tibble() %>% 
  select(-centroid, -geometry) %>% 
  rename(longitude=X, latitude=Y, FIPS=group) -> merged_counties

county_shape %>% 
  filter(FIPS %in% panel$FIPS) %>% 
  group_by(FIPS) %>%
  summarise(
    geometry = st_union(geometry)
  ) %>% 
  mutate(
    centroid = st_centroid(geometry)
  ) %>% 
  mutate(
    st_coordinates(centroid) %>% as_tibble()
  ) %>% 
  as_tibble() %>% 
  select(-centroid, -geometry) %>% 
  rename(longitude=X, latitude=Y) -> regular_counties


lat_lon_counties <- rbind(regular_counties, merged_counties)

panel %<>% 
  left_join(lat_lon_counties, by="FIPS")

#### Cleaning ####

panel %>% 
  filter(
    GR_AAP_TOT_TOT > quantile(GR_AAP_TOT_TOT, 0.9975, na.rm = T) | GR_AAP_TOT_TOT < quantile(GR_AAP_TOT_TOT, 0.0025, na.rm = T)
  ) %>% 
  distinct(AREA) %>% pull() -> el_1

panel %>% 
  filter(
    GR_INCOME_PC > quantile(GR_INCOME_PC, 0.9975, na.rm = T) | GR_INCOME_PC < quantile(GR_INCOME_PC, 0.0025, na.rm = T)
  ) %>% 
  distinct(AREA) %>% pull() -> el_2

panel %>% 
  filter(
    S4 > quantile(S4, 0.99)
  ) %>% 
  distinct(AREA) %>% pull() -> el_3

cut <- as.numeric(quantile(data_storm$COUNTY_AREA, 0.05))

data_storm %>% 
  filter(COUNTY_AREA<cut) %>% 
  distinct(FIPS_AG) %>% pull() -> el_4

panel %>% 
  filter(!(AREA%in%el_1)) %>% 
  filter(!(AREA%in%el_3)) %>% 
  filter(YEAR>1990) -> panel1

panel %>% 
  filter(!(AREA%in%el_1)) %>% 
  filter(!(FIPS%in%el_4)) %>% 
  filter(YEAR>1990) -> panel2

panel %>% 
  filter(!(AREA%in%el_1)) %>% 
  filter(!(AREA%in%el_2)) %>% 
  filter(!(AREA%in%el_3)) %>% 
  filter(YEAR>1990) -> panel3

panel %>% 
  filter(!(AREA%in%el_1)) %>% 
  filter(!(AREA%in%el_2)) %>% 
  filter(!(FIPS%in%el_4)) %>% 
  filter(YEAR>1990) -> panel4

panel1 %<>% 
  rename(
    prec = TOT_PREC,
    tmp = AVG_TEMP, 
    Area=AREA,
    Year = YEAR
  )

panel2 %<>% 
  rename(
    prec = TOT_PREC,
    tmp = AVG_TEMP, 
    Area=AREA,
    Year = YEAR
  )

panel3 %<>% 
  rename(
    prec = TOT_PREC,
    tmp = AVG_TEMP, 
    Area=AREA,
    Year = YEAR
  )

panel4 %<>% 
  rename(
    prec = TOT_PREC,
    tmp = AVG_TEMP, 
    Area=AREA,
    Year = YEAR
  )


saveRDS(panel1, file="data/f_panel_hail_pa.rds")
saveRDS(panel2, file="data/f_panel_hail_pa_wo_small.rds")
saveRDS(panel3, file="data/f_panel_hail_tr.rds")
saveRDS(panel4, file="data/f_panel_hail_tr_wo_small.rds")


