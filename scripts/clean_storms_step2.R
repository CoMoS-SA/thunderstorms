library(tidyverse)
library(sf)
library(magrittr)
library(tictoc)
library(tmaptools, warn.conflicts =FALSE)

rm(list = ls())

data <- readRDS(file="data/clean_storm_step1.rds")
county_shape <- st_read("utilities/c_10nv20/") 
zone_shape <- st_read("utilities/z_08mr23/") 
states <- read_csv("utilities/states.csv")

states %<>% 
  mutate(
    State = toupper(State)
  ) %>% 
  rename(STATE=State, STATE_CODE = Abbreviation)

county_shape %<>%
  rename(STATE_CODE=STATE) %>% 
  mutate(COUNTYNAME=toupper(COUNTYNAME)) %>% 
  left_join(
    .,
    states,
    by = "STATE_CODE"
  ) %>% 
  filter(!is.na(STATE)) %>% 
  group_by(FIPS) %>% 
  mutate(
    geometry = st_union(geometry)
  )

county_shape %>% 
  select(-LON, -LAT) %>% 
  duplicated() -> aux

county_shape<- county_shape[!aux,]
remove(aux)

data %<>% 
  filter(!(STATE_FIPS%in%c(96,97,98,99))) %>%  #remove puerto rico, american samoa, guam and virgin islands
  left_join(
    .,
    states,
    by = "STATE"
  ) %>% 
  mutate(
    STATE_FIPS = case_when(
      nchar(as.character(STATE_FIPS)) == 1 ~ paste0("0",STATE_FIPS),
      T ~ as.character(STATE_FIPS),
    ),
    CZ_FIPS = case_when(
      nchar(as.character(CZ_FIPS)) == 1 ~ paste0("00",CZ_FIPS),
      nchar(as.character(CZ_FIPS)) == 2 ~ paste0("0",CZ_FIPS),
      T ~ as.character(CZ_FIPS),
    ),
    FIPS_MIX = ifelse(CZ_TYPE=="C",paste0(STATE_FIPS, CZ_FIPS),paste0(STATE_CODE, CZ_FIPS)) 
  )

data %<>% filter(CZ_TYPE=="C")

data %>% 
  distinct(FIPS_MIX) %>% pull() -> fips_c


data %<>% 
  mutate(
    FIPS_MIX = case_when(
      FIPS_MIX == "02222" ~ "02090",
      FIPS_MIX == "51515" ~ "51019",
      FIPS_MIX == "02155" ~ "02050",
      FIPS_MIX == "02221" ~ "02290",
      FIPS_MIX == "02029" ~ "02198",
      FIPS_MIX == "02028" ~ "02198",
      FIPS_MIX == "04029" ~ "04012",
      T ~ FIPS_MIX
    )
  )

setdiff(data$FIPS_MIX, county_shape$FIPS)
setdiff(county_shape$FIPS, data$FIPS_MIX)

data %>% 
  filter(FIPS_MIX %in% county_shape$FIPS) -> data_c_no_prob

data %>% 
  filter(!(FIPS_MIX %in% county_shape$FIPS)) -> data_c_prob

data_c_prob %<>% 
  filter(!(CZ_NAME%in%c("NVZ006", "NVZ009", "TXZ511")))

tibble(
  EVENT_ID = c(10355716,10335866,10355882,10346170,10346172,10346175,10316824,10335844,10335813,10326868,10331413,10331415,10352910,10352909,10316679,10325838,10326865,10326866,10326867,10326834,10326835),
  FIPS = list(
    c("48039","48071","48157","48167","48199","48241","48245","48291","48339","48351","48361","48373","48407","48457","48471","48473","48481"),
    "32003",
    c("48275", "48485", "48487", "48077", "48023", "48009"),
    "46083",
    "46083",
    "46135",
    c("06089", "06005", "06009", "06077", "06099", "06109", "06089", "06103"),
    c("32005", "32019", "32029", "32031", "32510"),
    c("32031", "32001", "32019", "32027", "32031", "32005", "32019", "32029", "32031", "32510"),
    c("20043", "20103", "20091", "20209", "20005"),
    c("27057", "27087", "27153", "27159", "27007", "27111", "27111", "27005", "27005", "27029", "27027", "27167"),
    c("27057", "27087", "27007", "27111", "27111", "27005", "27005", "27029", "27029", "27027", "27155", "27167"),
    "51157",
    "51157",
    c("08081", "08103", "08107"),
    c("20043", "20103", "20091", "20209", "20005"),
    c("20043", "20103", "20091", "20209", "20005"),
    c("20043", "20103", "20091", "20209", "20005"),
    c("20043", "20103", "20091", "20209", "20005"),
    c("20043", "20103", "20091", "20209", "20005"),
    c("20043", "20103", "20091", "20209", "20005")
  )
) -> fix

data_c_prob %<>% 
  left_join(
    .,
    fix,
    by="EVENT_ID"
  )

data_c_no_prob %<>% 
  mutate(
    FIPS = FIPS_MIX
  )

rbind(
  data_c_prob, 
  data_c_no_prob
) -> c_data_fixed

setdiff(unlist(c_data_fixed$FIPS), county_shape$FIPS)

saveRDS(c_data_fixed,file="data/clean_storm_step2.rds")




