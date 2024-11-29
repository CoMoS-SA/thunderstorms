library(readxl)
library(tidyverse)
library(sf)
library(magrittr)
library(tictoc)
library(tmaptools, warn.conflicts =FALSE)


rm(list = ls())

data_storm <- readRDS("data/clean_storm_step3.rds")
data_wages <- readRDS(file="data/wages_employment.rds")
data_wages_state <- readRDS(file="data/wages_employment_state.rds")
data_income <- readRDS(file="data/income.rds")
data_income_state <- readRDS(file="data/income_state.rds")
data_temp_prec <- readRDS(file="data/cleaned_temp_prec.rds")
dis_year <- readRDS(file="data/dis_year.rds") 

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

#### Aggregate Storms ####

data_storm %>% 
  group_by(FIPS_AG, YEAR) %>% 
  summarise(
    M1 = n(),
    M2 = max(MAGNITUDE),
    M3 = sum(MAGNITUDE^3),
    M4 = sum(MAGNITUDE^2),
    COUNTY_AREA  = COUNTY_AREA[1],
    #
    IS_D =    ifelse(any(IS_IN_DISASTER==T),1,0),
    IS_D2 =   ifelse(any(IS_IN_DISASTER2==T),1,0),
    IS_D3 =   ifelse(any(IS_IN_DISASTER3==T),1,0),
    IS_D_pa = ifelse(any(IS_IN_DISASTER==T&paProgramDeclared==T),1,0),
    IS_D_hm = ifelse(any(IS_IN_DISASTER==T&hmProgramDeclared==T),1,0),
    IS_D_ia = ifelse(any(IS_IN_DISASTER==T&iaProgramDeclared==T),1,0),
    IS_D_ih = ifelse(any(IS_IN_DISASTER==T&ihProgramDeclared==T),1,0),
    #
    M1_D = sum(IS_IN_DISASTER==T),
    M2_D = max(MAGNITUDE[IS_IN_DISASTER==T]),
    M3_D = sum(MAGNITUDE[IS_IN_DISASTER==T]^3),
    M4_D = sum(MAGNITUDE[IS_IN_DISASTER==T]^2),
    #
    M1_D_pa = sum(IS_IN_DISASTER==T&paProgramDeclared==T),
    M2_D_pa = max(MAGNITUDE[IS_IN_DISASTER==T&paProgramDeclared==T]),
    M3_D_pa = sum(MAGNITUDE[IS_IN_DISASTER==T&paProgramDeclared==T]^3),
    M4_D_pa = sum(MAGNITUDE[IS_IN_DISASTER==T&paProgramDeclared==T]^2),
    #
    M1_D_hm = sum(IS_IN_DISASTER==T&hmProgramDeclared==T),
    M2_D_hm = max(MAGNITUDE[IS_IN_DISASTER==T&hmProgramDeclared==T]),
    M3_D_hm = sum(MAGNITUDE[IS_IN_DISASTER==T&hmProgramDeclared==T]^3),
    M4_D_hm = sum(MAGNITUDE[IS_IN_DISASTER==T&hmProgramDeclared==T]^2),
    #
    M1_D_ia = sum(IS_IN_DISASTER==T&iaProgramDeclared==T),
    M2_D_ia = max(MAGNITUDE[IS_IN_DISASTER==T&iaProgramDeclared==T]),
    M3_D_ia = sum(MAGNITUDE[IS_IN_DISASTER==T&iaProgramDeclared==T]^3),
    M4_D_ia = sum(MAGNITUDE[IS_IN_DISASTER==T&iaProgramDeclared==T]^2),
    #
    M1_D_ih = sum(IS_IN_DISASTER==T&ihProgramDeclared==T),
    M2_D_ih = max(MAGNITUDE[IS_IN_DISASTER==T&ihProgramDeclared==T]),
    M3_D_ih = sum(MAGNITUDE[IS_IN_DISASTER==T&ihProgramDeclared==T]^3),
    M4_D_ih = sum(MAGNITUDE[IS_IN_DISASTER==T&ihProgramDeclared==T]^2),
    #
    M1_ND = sum(IS_IN_DISASTER==F),
    M2_ND = max(MAGNITUDE[IS_IN_DISASTER==F]),
    M3_ND = sum(MAGNITUDE[IS_IN_DISASTER==F]^3),
    M4_ND = sum(MAGNITUDE[IS_IN_DISASTER==F]^2),
  )  %>% 
  mutate(
    M2_D = ifelse(is.infinite(M2_D), 0, M2_D),
    M2_D_pa = ifelse(is.infinite(M2_D_pa), 0,M2_D_pa),
    M2_D_hm = ifelse(is.infinite(M2_D_hm), 0,M2_D_hm),
    M2_D_ia = ifelse(is.infinite(M2_D_ia), 0,M2_D_ia),
    M2_D_ih = ifelse(is.infinite(M2_D_ih), 0,M2_D_ih),
    M2_ND = ifelse(is.infinite(M2_ND), 0, M2_ND),
  ) %>% ungroup() -> storm_aggr

storm_aggr %>% 
  distinct(FIPS_AG, COUNTY_AREA) %>% 
  summarise(
    mean = mean(COUNTY_AREA)
  ) %>% pull(mean) -> mean_county_area

storm_aggr %<>% 
  mutate(
    S1 = ((M1/COUNTY_AREA)*mean_county_area),
    S2 = ((M2/COUNTY_AREA)*mean_county_area)*(0.44704),
    S3 = ((M3/COUNTY_AREA)*mean_county_area)*(0.44704^3),
    S4 = ((M4/COUNTY_AREA)*mean_county_area)*(0.44704^2),
    #
    S1_D = ((M1_D/COUNTY_AREA)*mean_county_area),
    S2_D = ((M2_D/COUNTY_AREA)*mean_county_area)*(0.44704),
    S3_D = ((M3_D/COUNTY_AREA)*mean_county_area)*(0.44704^3),
    S4_D = ((M4_D/COUNTY_AREA)*mean_county_area)*(0.44704^2),
    #
    S1_D_pa = ((M1_D_pa/COUNTY_AREA)*mean_county_area),
    S2_D_pa = ((M2_D_pa/COUNTY_AREA)*mean_county_area)*(0.44704),
    S3_D_pa = ((M3_D_pa/COUNTY_AREA)*mean_county_area)*(0.44704^3),
    S4_D_pa = ((M4_D_pa/COUNTY_AREA)*mean_county_area)*(0.44704^2),
    #
    S1_D_hm = ((M1_D_hm/COUNTY_AREA)*mean_county_area),
    S2_D_hm = ((M2_D_hm/COUNTY_AREA)*mean_county_area)*(0.44704),
    S3_D_hm = ((M3_D_hm/COUNTY_AREA)*mean_county_area)*(0.44704^3),
    S4_D_hm = ((M4_D_hm/COUNTY_AREA)*mean_county_area)*(0.44704^2),
    #
    S1_D_ia = ((M1_D_ia/COUNTY_AREA)*mean_county_area),
    S2_D_ia = ((M2_D_ia/COUNTY_AREA)*mean_county_area)*(0.44704),
    S3_D_ia = ((M3_D_ia/COUNTY_AREA)*mean_county_area)*(0.44704^3),
    S4_D_ia = ((M4_D_ia/COUNTY_AREA)*mean_county_area)*(0.44704^2),
    #
    S1_D_ih = ((M1_D_ih/COUNTY_AREA)*mean_county_area),
    S2_D_ih = ((M2_D_ih/COUNTY_AREA)*mean_county_area)*(0.44704),
    S3_D_ih = ((M3_D_ih/COUNTY_AREA)*mean_county_area)*(0.44704^3),
    S4_D_ih = ((M4_D_ih/COUNTY_AREA)*mean_county_area)*(0.44704^2),
    #
    S1_ND = ((M1_ND/COUNTY_AREA)*mean_county_area),
    S2_ND = ((M2_ND/COUNTY_AREA)*mean_county_area)*(0.44704),
    S3_ND = ((M3_ND/COUNTY_AREA)*mean_county_area)*(0.44704^3),
    S4_ND = ((M4_ND/COUNTY_AREA)*mean_county_area)*(0.44704^2)
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
                          AAE_PRIV_NATMIN, AAE_PRIV_CONSTR, AAE_PRIV_MANUF, AAE_PRIV_SERV,
                          GR_AAE_PRIV_NATMIN, GR_AAE_PRIV_CONSTR, GR_AAE_PRIV_MANUF, GR_AAE_PRIV_SERV,
                          AAP_TOT_TOT, GR_AAP_TOT_TOT,
                          AAP_PRIV_NATMIN, AAP_PRIV_CONSTR, AAP_PRIV_MANUF, AAP_PRIV_SERV,
                          GR_AAP_PRIV_NATMIN, GR_AAP_PRIV_CONSTR, GR_AAP_PRIV_MANUF, GR_AAP_PRIV_SERV,
    ),
    by=c("YEAR", "FIPS")
  ) %>% 
  left_join(
    .,
    data_wages_state %>% select(STATE, YEAR,  
                                ST_AAE_TOT_TOT, ST_GR_AAE_TOT_TOT, 
                                ST_AAE_PRIV_NATMIN, ST_AAE_PRIV_CONSTR, ST_AAE_PRIV_MANUF, ST_AAE_PRIV_SERV,
                                ST_GR_AAE_PRIV_NATMIN, ST_GR_AAE_PRIV_CONSTR, ST_GR_AAE_PRIV_MANUF, ST_GR_AAE_PRIV_SERV,
                                ST_AAP_TOT_TOT, ST_GR_AAP_TOT_TOT,
                                ST_AAP_PRIV_NATMIN, ST_AAP_PRIV_CONSTR, ST_AAP_PRIV_MANUF, ST_AAP_PRIV_SERV,
                                ST_GR_AAP_PRIV_NATMIN, ST_GR_AAP_PRIV_CONSTR, ST_GR_AAP_PRIV_MANUF, ST_GR_AAP_PRIV_SERV,
    ),
    by=c("YEAR", "STATE")
  ) %>% 
  left_join(
    .,
    data_income_state %>% select(-FIPS),
    by=c("YEAR", "STATE")
  ) %>% 
  left_join(
    .,
    dis_year %>% select(-COUNTY, -STATE_CODE), 
    by=c("YEAR", "FIPS")
  ) %>% 
  mutate(
    DISASTER_YEAR = ifelse(is.na(DISASTER_YEAR), F, DISASTER_YEAR),
    DISASTER_YEAR2 = ifelse(is.na(DISASTER_YEAR2), F, DISASTER_YEAR2),
    DISASTER_YEAR3 = ifelse(is.na(DISASTER_YEAR3), F, DISASTER_YEAR3),
    DISASTER_YEAR_EXT = ifelse(is.na(DISASTER_YEAR_EXT), F, DISASTER_YEAR_EXT),
    DISASTER_YEAR_ANY = ifelse(is.na(DISASTER_YEAR_ANY), F, DISASTER_YEAR_ANY),
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
  ) %>% relocate(FIPS, AREA, STATE, YEAR, M1:S4_ND) 

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
  group_by(FIPS) %>% #merge counties with multiple rows and geometries
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
  filter(
    GR_AAP_PRIV_NATMIN > quantile(GR_AAP_PRIV_NATMIN, 0.9975, na.rm = T) | GR_AAP_PRIV_NATMIN < quantile(GR_AAP_PRIV_NATMIN, 0.0025, na.rm = T)
  ) %>% 
  distinct(AREA) %>% pull() -> el_nat

panel %>% 
  filter(
    GR_AAP_PRIV_CONSTR > quantile(GR_AAP_PRIV_CONSTR, 0.9975, na.rm = T) | GR_AAP_PRIV_CONSTR < quantile(GR_AAP_PRIV_CONSTR, 0.0025, na.rm = T)
  ) %>% 
  distinct(AREA) %>% pull() -> el_con

panel %>% 
  filter(
    GR_AAP_PRIV_MANUF > quantile(GR_AAP_PRIV_MANUF, 0.9975, na.rm = T) | GR_AAP_PRIV_MANUF < quantile(GR_AAP_PRIV_MANUF, 0.0025, na.rm = T)
  ) %>% 
  distinct(AREA) %>% pull() -> el_man

panel %>% 
  filter(
    GR_AAP_PRIV_SERV > quantile(GR_AAP_PRIV_SERV, 0.9975, na.rm = T) | GR_AAP_PRIV_SERV < quantile(GR_AAP_PRIV_SERV, 0.0025, na.rm = T)
  ) %>% 
  distinct(AREA) %>% pull() -> el_ser

#nas
panel %>% 
  filter(
    is.na(GR_AAP_PRIV_NATMIN) & (YEAR!=1990)
  ) %>% 
  distinct(AREA) %>% pull() -> na_nat

panel %>% 
  filter(
    is.na(GR_AAP_PRIV_CONSTR) & (YEAR!=1990)
  ) %>% 
  distinct(AREA) %>% pull() -> na_con

panel %>% 
  filter(
    is.na(GR_AAP_PRIV_MANUF) & (YEAR!=1990)
  ) %>% 
  distinct(AREA) %>% pull() -> na_man

panel %>% 
  filter(
    is.na(GR_AAP_PRIV_SERV) & (YEAR!=1990)
  ) %>% 
  distinct(AREA) %>% pull() -> na_ser


panel %>% 
  filter(!(AREA%in%el_1)) %>% 
  filter(!(AREA%in%el_3)) %>% 
  filter(!(AREA%in%el_nat)) %>% 
  filter(!(AREA%in%el_con)) %>% 
  filter(!(AREA%in%el_man)) %>% 
  filter(!(AREA%in%el_ser)) %>% 
  filter(!(AREA%in%na_nat)) %>% 
  filter(!(AREA%in%na_con)) %>% 
  filter(!(AREA%in%na_man)) %>% 
  filter(!(AREA%in%na_ser)) %>% 
  filter(YEAR>1990) -> panel1

panel %>% 
  filter(!(AREA%in%el_1)) %>% 
  filter(!(FIPS%in%el_4)) %>% 
  filter(!(AREA%in%el_nat)) %>% 
  filter(!(AREA%in%el_con)) %>% 
  filter(!(AREA%in%el_man)) %>% 
  filter(!(AREA%in%el_ser)) %>% 
  filter(!(AREA%in%na_nat)) %>% 
  filter(!(AREA%in%na_con)) %>% 
  filter(!(AREA%in%na_man)) %>% 
  filter(!(AREA%in%na_ser)) %>% 
  filter(YEAR>1990) -> panel2


panel %>% 
  filter(!(AREA%in%el_1)) %>% 
  filter(!(AREA%in%el_2)) %>% 
  filter(!(AREA%in%el_3)) %>% 
  filter(!(AREA%in%el_nat)) %>% 
  filter(!(AREA%in%el_con)) %>% 
  filter(!(AREA%in%el_man)) %>% 
  filter(!(AREA%in%el_ser)) %>% 
  filter(!(AREA%in%na_nat)) %>% 
  filter(!(AREA%in%na_con)) %>% 
  filter(!(AREA%in%na_man)) %>% 
  filter(!(AREA%in%na_ser)) %>% 
  filter(YEAR>1990) -> panel3

panel %>% 
  filter(!(AREA%in%el_1)) %>% 
  filter(!(AREA%in%el_2)) %>% 
  filter(!(FIPS%in%el_4)) %>% 
  filter(!(AREA%in%el_nat)) %>% 
  filter(!(AREA%in%el_con)) %>% 
  filter(!(AREA%in%el_man)) %>% 
  filter(!(AREA%in%el_ser)) %>% 
  filter(!(AREA%in%na_nat)) %>% 
  filter(!(AREA%in%na_con)) %>% 
  filter(!(AREA%in%na_man)) %>% 
  filter(!(AREA%in%na_ser)) %>% 
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


saveRDS(panel1, file="data/f_panel_sectors_pa.rds") #pa stands for partially trimmed
saveRDS(panel2, file="data/f_panel_sectors_pa_wo_small.rds")
saveRDS(panel3, file="data/f_panel_sectors_tr.rds")



saveRDS(panel4, file="data/panel_sectors_tr_wo_small.rds")