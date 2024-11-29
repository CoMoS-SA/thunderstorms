library(tidyverse)
library(sf)
library(magrittr)
library(tictoc)
library(tmaptools, warn.conflicts =FALSE)

rm(list = ls())

data <- readRDS(file="data/clean_storm_step2.rds")

#manipulate times and count by episode
data %<>% 
  mutate(
    BEG_T = as.POSIXct(BEGIN_DATE_TIME, format="%d-%b-%y %H:%M:%S"),
    END_T = as.POSIXct(END_DATE_TIME, format="%d-%b-%y %H:%M:%S")
  )

#Some events happen when time shift is kicking in. We need to assign the entire episode to the next day
data %>% 
  filter(is.na(BEG_T)) %>% pull(EVENT_ID) -> prob_ev

data %>% 
  filter(is.na(END_T)) %>% pull(EVENT_ID) -> prob_ev_e

data %>% 
  filter(is.na(BEG_T)) %>% pull(EPISODE_ID) %>% unique() -> prob_ep

data %>% 
  filter(is.na(END_T)) %>% pull(EPISODE_ID) %>% unique() -> prob_ep_e

#first we manually add one our to problematic cases in order to avoid NA
data %<>% 
  mutate(
    BEGIN_DATE_TIME = case_when(
      BEGIN_DATE_TIME == "26-MAR-00 02:31:00" ~ "26-MAR-00 03:31:00",
      BEGIN_DATE_TIME == "27-MAR-05 02:35:00" ~ "27-MAR-05 03:35:00",
      BEGIN_DATE_TIME == "29-MAR-09 02:00:00" ~ "29-MAR-09 03:00:00",
      BEGIN_DATE_TIME == "29-MAR-09 02:20:00" ~ "29-MAR-09 03:20:00",
      BEGIN_DATE_TIME == "29-MAR-09 02:45:00" ~ "29-MAR-09 03:45:00",
      BEGIN_DATE_TIME == "31-MAR-13 02:20:00" ~ "31-MAR-13 03:20:00",
      BEGIN_DATE_TIME == "31-MAR-13 02:55:00" ~ "31-MAR-13 03:55:00",
      BEGIN_DATE_TIME == "29-MAR-20 02:01:00" ~ "29-MAR-20 03:01:00",
      BEGIN_DATE_TIME == "29-MAR-20 02:35:00" ~ "29-MAR-20 03:35:00",
      BEGIN_DATE_TIME == "29-MAR-20 02:49:00" ~ "29-MAR-20 03:49:00",
      BEGIN_DATE_TIME == "29-MAR-20 02:55:00" ~ "29-MAR-20 03:55:00",
      BEGIN_DATE_TIME == "29-MAR-20 02:00:00" ~ "29-MAR-20 03:00:00",
      BEGIN_DATE_TIME == "29-MAR-20 02:41:00" ~ "29-MAR-20 03:41:00",
      BEGIN_DATE_TIME == "28-MAR-21 02:30:00" ~ "28-MAR-21 03:30:00",
      BEGIN_DATE_TIME == "28-MAR-21 02:41:00" ~ "28-MAR-21 03:41:00",
      BEGIN_DATE_TIME == "28-MAR-21 02:05:00" ~ "28-MAR-21 03:05:00",
      BEGIN_DATE_TIME == "28-MAR-21 02:15:00" ~ "28-MAR-21 03:15:00",
      BEGIN_DATE_TIME == "28-MAR-21 02:28:00" ~ "28-MAR-21 03:28:00",
      BEGIN_DATE_TIME == "28-MAR-21 02:34:00" ~ "28-MAR-21 03:34:00",
      BEGIN_DATE_TIME == "28-MAR-21 02:35:00" ~ "28-MAR-21 03:35:00",
      BEGIN_DATE_TIME == "28-MAR-21 02:25:00" ~ "28-MAR-21 03:25:00",
      BEGIN_DATE_TIME == "28-MAR-21 02:00:00" ~ "28-MAR-21 03:00:00",
      BEGIN_DATE_TIME == "28-MAR-21 02:00:00" ~ "28-MAR-21 03:00:00",
      BEGIN_DATE_TIME == "27-MAR-05 02:55:00" ~ "27-MAR-05 03:55:00",
      BEGIN_DATE_TIME == "29-MAR-09 02:00:00" ~ "29-MAR-09 03:00:00",
      BEGIN_DATE_TIME == "29-MAR-15 02:30:00" ~ "29-MAR-15 03:30:00",
      BEGIN_DATE_TIME == "27-MAR-22 02:00:00" ~ "27-MAR-22 03:00:00",
      T ~ BEGIN_DATE_TIME
    ),
    END_DATE_TIME = case_when(
      END_DATE_TIME=="26-MAR-00 02:31:00" ~ "26-MAR-00 03:31:00",
      END_DATE_TIME=="27-MAR-05 02:37:00" ~ "27-MAR-05 03:37:00",
      END_DATE_TIME=="29-MAR-09 02:00:00" ~ "29-MAR-09 03:00:00",
      END_DATE_TIME=="29-MAR-09 02:20:00" ~ "29-MAR-09 03:20:00",
      END_DATE_TIME=="29-MAR-09 02:45:00" ~ "29-MAR-09 03:45:00",
      END_DATE_TIME=="31-MAR-13 02:20:00" ~ "31-MAR-13 03:20:00",
      END_DATE_TIME=="31-MAR-13 02:55:00" ~ "31-MAR-13 03:55:00",
      END_DATE_TIME=="29-MAR-20 02:01:00" ~ "29-MAR-20 03:01:00",
      END_DATE_TIME=="29-MAR-20 02:02:00" ~ "29-MAR-20 03:02:00",
      END_DATE_TIME=="29-MAR-20 02:35:00" ~ "29-MAR-20 03:35:00",
      END_DATE_TIME=="29-MAR-20 02:49:00" ~ "29-MAR-20 03:49:00",
      END_DATE_TIME=="29-MAR-20 02:55:00" ~ "29-MAR-20 03:55:00",
      END_DATE_TIME=="29-MAR-20 02:00:00" ~ "29-MAR-20 03:00:00",
      END_DATE_TIME=="29-MAR-20 02:41:00" ~ "29-MAR-20 03:41:00",
      END_DATE_TIME=="29-MAR-20 02:00:00" ~ "29-MAR-20 03:00:00",
      END_DATE_TIME=="28-MAR-21 02:35:00" ~ "28-MAR-21 03:35:00",
      END_DATE_TIME=="28-MAR-21 02:46:00" ~ "28-MAR-21 03:46:00",
      END_DATE_TIME=="28-MAR-21 02:06:00" ~ "28-MAR-21 03:06:00",
      END_DATE_TIME=="28-MAR-21 02:15:00" ~ "28-MAR-21 03:15:00",
      END_DATE_TIME=="28-MAR-21 02:28:00" ~ "28-MAR-21 03:28:00",
      END_DATE_TIME=="28-MAR-21 02:34:00" ~ "28-MAR-21 03:34:00",
      END_DATE_TIME=="28-MAR-21 02:35:00" ~ "28-MAR-21 03:35:00",
      END_DATE_TIME=="28-MAR-21 02:30:00" ~ "28-MAR-21 03:30:00",
      END_DATE_TIME=="28-MAR-21 02:00:00" ~ "28-MAR-21 03:00:00",
      END_DATE_TIME=="28-MAR-21 02:00:00" ~ "28-MAR-21 03:00:00",
      END_DATE_TIME=="27-MAR-05 02:55:00" ~ "27-MAR-05 03:55:00",
      END_DATE_TIME=="29-MAR-15 02:00:00" ~ "29-MAR-15 03:00:00",
      END_DATE_TIME=="29-MAR-15 02:00:00" ~ "29-MAR-15 03:00:00",
      END_DATE_TIME=="29-MAR-15 02:00:00" ~ "29-MAR-15 03:00:00",
      END_DATE_TIME=="27-MAR-16 02:00:00" ~ "27-MAR-16 03:00:00",
      END_DATE_TIME=="27-MAR-22 02:00:00" ~ "27-MAR-22 03:00:00",
      T ~ END_DATE_TIME
    )
  )

#transform problematic events in date format again, leaving the rest as it is 
#add one day to every event in the episode
#remove the manually added hour to the problematic events
data %<>% 
  mutate(
    BEG_T = case_when(
      EVENT_ID %in% prob_ev ~ as.POSIXct(BEGIN_DATE_TIME, format="%d-%b-%y %H:%M:%S"),
      T ~ BEG_T
    ),
    BEG_T = case_when(
      EPISODE_ID %in% prob_ep & (!(EVENT_ID %in% prob_ev)) ~ BEG_T + lubridate::days(1),
      EPISODE_ID %in% prob_ep & (EVENT_ID %in% prob_ev) ~ BEG_T + lubridate::days(1) - lubridate::hms("01:00:00"),
      T ~ BEG_T
    ),
    END_T = case_when(
      EVENT_ID %in% prob_ev_e ~ as.POSIXct(END_DATE_TIME, format="%d-%b-%y %H:%M:%S"),
      T ~ END_T
    ),
    END_T = case_when(
      EPISODE_ID %in% prob_ev_e & (!(EVENT_ID %in% prob_ev_e)) ~ END_T + lubridate::days(1),
      EPISODE_ID %in% prob_ev_e & (EVENT_ID %in% prob_ev_e) ~ END_T + lubridate::days(1) - lubridate::hms("01:00:00"),
      T ~ END_T
    )
  ) %>% 
  group_by(EPISODE_ID) %>% 
  mutate(
    n=ifelse(is.na(EPISODE_ID), 1, n())
  ) %>% 
  ungroup()

remove(prob_ep, prob_ep_e, prob_ev, prob_ev_e)

#fix damages
data %<>% 
  mutate(
    DAMAGE_PROPERTY = case_when(
      (str_sub(DAMAGE_PROPERTY, nchar(DAMAGE_PROPERTY), nchar(DAMAGE_PROPERTY)) == "K") & (DAMAGE_PROPERTY!="K") ~ 1000*parse_number(DAMAGE_PROPERTY),
      DAMAGE_PROPERTY=="K" ~ 0,
      str_sub(DAMAGE_PROPERTY, nchar(DAMAGE_PROPERTY), nchar(DAMAGE_PROPERTY)) == "h" ~ 100*parse_number(DAMAGE_PROPERTY),
      str_sub(DAMAGE_PROPERTY, nchar(DAMAGE_PROPERTY), nchar(DAMAGE_PROPERTY)) == "H" ~ 100*parse_number(DAMAGE_PROPERTY),
      str_sub(DAMAGE_PROPERTY, nchar(DAMAGE_PROPERTY), nchar(DAMAGE_PROPERTY)) == "M" ~ 1000000*parse_number(DAMAGE_PROPERTY),
      str_sub(DAMAGE_PROPERTY, nchar(DAMAGE_PROPERTY), nchar(DAMAGE_PROPERTY)) == "B" ~ 1000000000*parse_number(DAMAGE_PROPERTY),
      T ~ parse_number(DAMAGE_PROPERTY)
    ),
    DAMAGE_CROPS = case_when(
      (str_sub(DAMAGE_CROPS, nchar(DAMAGE_CROPS), nchar(DAMAGE_CROPS)) == "K") & (DAMAGE_CROPS!="K") ~ 1000*parse_number(DAMAGE_CROPS),
      DAMAGE_CROPS=="K" ~ 0,
      str_sub(DAMAGE_CROPS, nchar(DAMAGE_CROPS), nchar(DAMAGE_CROPS)) == "h" ~ 100*parse_number(DAMAGE_CROPS),
      str_sub(DAMAGE_CROPS, nchar(DAMAGE_CROPS), nchar(DAMAGE_CROPS)) == "H" ~ 100*parse_number(DAMAGE_CROPS),
      str_sub(DAMAGE_CROPS, nchar(DAMAGE_CROPS), nchar(DAMAGE_CROPS)) == "M" ~ 1000000*parse_number(DAMAGE_CROPS),
      str_sub(DAMAGE_CROPS, nchar(DAMAGE_CROPS), nchar(DAMAGE_CROPS)) == "B" ~ 1000000000*parse_number(DAMAGE_CROPS),
      T ~ parse_number(DAMAGE_CROPS)
    )
  ) 

data %>% 
  filter(n>1) -> to_group

data %>% 
  filter(n==1) -> not_to_group

#Need for several iterations to group everything, because of complicated time structures 

for (i in 1:11) { #takes eleven iterations
  tic()
  to_group %<>% 
    arrange(FIPS, EPISODE_ID, BEG_T) %>%
    group_by(FIPS, EPISODE_ID) %>% 
    mutate(
      L_END=lag(END_T)
    ) %>% 
    mutate(
      is = BEG_T<=L_END,
      pp = 1:n(),
      grp = ifelse(!is | is.na(is), pp, NA)
    ) %>%
    fill(grp, .direction = "down") %>% 
    select(-is, -pp)
  toc()
  
  to_group %<>% 
    group_by(EPISODE_ID, FIPS, grp) %>% 
    arrange(BEG_T) %>% 
    summarise(
      BEG_T = min(BEG_T),
      END_T = max(END_T),
      MAGNITUDE = max(MAGNITUDE),
      MAGNITUDE_TYPE = MAGNITUDE_TYPE[max(MAGNITUDE)],
      EVENT_TYPE = EVENT_TYPE[1],
      EVENT_ID = EVENT_ID[1],
      EPISODE_ID = EPISODE_ID[1],
      INJURIES_DIRECT = sum(INJURIES_DIRECT),
      INJURIES_INDIRECT = sum(INJURIES_INDIRECT),
      DEATHS_DIRECT = sum(DEATHS_DIRECT),
      DEATHS_INDIRECT = sum(DEATHS_INDIRECT),
      DAMAGE_PROPERTY = sum(DAMAGE_PROPERTY),
      DAMAGE_CROPS = sum(DAMAGE_CROPS),
      BEGIN_LON = first(na.omit(BEGIN_LON)),
      END_LON = last(na.omit(END_LON)),
      BEGIN_LAT = first(na.omit(BEGIN_LAT)),
      END_LAT = last(na.omit(END_LAT)), 
      STATE = first(STATE), #state is always the same
      FIPS = FIPS[1], #grouping also by FIPS
      STATE_CODE = first(STATE_CODE)
    ) %>% 
    ungroup() %>% select(-grp)
}

#count to report in manuscript
data %>% 
  unnest(FIPS) %>% 
  filter(YEAR>1990) %>% 
  filter(YEAR<2020) %>% 
  filter(STATE!="ALASKA") %>% 
  filter(STATE!="HAWAII") %>% 
  nrow()

#bind together and overwrite
data <- rbind(
  to_group ,
  not_to_group %>% 
    select(
      BEG_T,END_T,MAGNITUDE, MAGNITUDE_TYPE,EVENT_TYPE,EVENT_ID,EPISODE_ID,INJURIES_DIRECT,INJURIES_INDIRECT,
      DEATHS_DIRECT,DEATHS_INDIRECT,DAMAGE_PROPERTY,DAMAGE_CROPS,BEGIN_LON,END_LON,BEGIN_LAT,END_LAT,
      STATE_CODE, STATE, FIPS
    )
)

#count to report in manuscript
data %>% 
  unnest(FIPS) %>%
  mutate(
    YEAR = as.numeric(lubridate::year(BEG_T))
  ) %>% 
  filter(YEAR>1990) %>% 
  filter(YEAR<2020) %>% 
  filter(STATE!="ALASKA") %>% 
  filter(STATE!="HAWAII") %>% 
  nrow()

saveRDS(data,file="data/clean_storm_step3_nested.rds")

remove(to_group, not_to_group)

#Unnest all observations with multiple FIPS####
data %<>% 
  unnest(FIPS)

#### Obtain County Names ####
county_shape <- st_read("utilities/c_10nv20/") 
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
  ) %>% 
  as_tibble() %>% 
  select(-geometry)

county_shape %>% 
  select(-LON, -LAT) %>% 
  duplicated() -> aux

county_shape<- county_shape[!aux,]
remove(aux)

county_shape %<>% 
  mutate(
    COUNTYNAME=case_when(
      FIPS=="12087" ~ "MONROE",
      T ~ COUNTYNAME
    )
  ) %>% 
  filter(COUNTYNAME!="NIIHAU IN KAUAI") %>% 
  filter(COUNTYNAME!="LANAI IN MAUI") %>% 
  filter(COUNTYNAME!="KAHOOLAWE IN MAUI") %>% 
  filter(COUNTYNAME!="MOLOKAI IN MAUI") %>% 
  filter(COUNTYNAME!="NEW YORK (MANHATTAN)")

county_shape %>% 
  select(FIPS, COUNTYNAME) %>% 
  duplicated()-> aux

county_shape<- county_shape[!aux,]
remove(aux)

data %<>% 
  left_join(
    .,
    county_shape %>% 
      select(COUNTYNAME, FIPS) %>% 
      rename(COUNTY = COUNTYNAME),
    by="FIPS"
  )

data %<>% 
  mutate(
    COUNTY = case_when(
      FIPS%in%c("02063","02261") ~ "CHUGACH CENSUS AREA",
      T ~ COUNTY
    ),
    FIPS = case_when(
      FIPS%in%c("02063","02261") ~ "02261",
      T ~ FIPS
    )
  )

#### DISASTER DECLARATIONS #####
DDS <- read_csv("data/DisasterDeclarationsSummaries.csv")

DDS %<>% 
  mutate(
    COUNTY = gsub(' \\(.*$', '', declaredCountyArea),
    COUNTY = toupper(COUNTY),
    BEG_YEAR = lubridate::year(incidentBeginDate),
    END_YEAR = lubridate::year(incidentEndDate)
  ) %>% 
  filter(BEG_YEAR>1989) %>% 
  rename(STATE_CODE=state) %>% 
  filter(STATE_CODE%in%states$STATE_CODE) 


DDS %<>% 
  mutate(
    COUNTY = case_when(
      STATE_CODE=="VA" & COUNTY=="RADFORD" ~ "CITY OF RADFORD",
      STATE_CODE=="VA" & COUNTY=="COVINGTON" ~ "CITY OF COVINGTON",
      STATE_CODE=="VA" & declaredCountyArea=="Roanoke" ~ "CITY OF ROANOKE",
      STATE_CODE=="VA" & COUNTY=="SALEM" ~ "CITY OF SALEM",
      STATE_CODE=="VA" & COUNTY=="GALAX" ~ "CITY OF GALAX",
      STATE_CODE=="VA" & COUNTY=="BUENA VISTA" ~ "CITY OF BUENA VISTA",
      STATE_CODE=="VA" & COUNTY=="LEXINGTON" ~ "CITY OF LEXINGTON",
      STATE_CODE=="VA" & COUNTY=="HARRISONBURG" ~ "CITY OF HARRISONBURG",
      STATE_CODE=="VA" & COUNTY=="NORTON" ~ "CITY OF NORTON",
      STATE_CODE=="VA" & COUNTY=="BRISTOL" ~ "CITY OF BRISTOL",
      STATE_CODE=="VA" & COUNTY=="CHARLOTTESVILLE" ~ "CITY OF CHARLOTTESVILLE",
      STATE_CODE=="VA" & COUNTY=="DANVILLE" ~ "CITY OF DANVILLE",
      STATE_CODE=="VA" & declaredCountyArea=="Fairfax" ~ "CITY OF FAIRFAX",
      STATE_CODE=="VA" & COUNTY=="ALEXANDRIA" ~ "CITY OF ALEXANDRIA",
      STATE_CODE=="VA" & COUNTY=="FALLS CHURCH" ~ "CITY OF FALLS CHURCH",
      STATE_CODE=="VA" & COUNTY=="WINCHESTER" ~ "CITY OF WINCHESTER",
      STATE_CODE=="VA" & COUNTY=="VIRGINIA BEACH" ~ "CITY OF VIRGINIA BEACH",
      STATE_CODE=="VA" & COUNTY=="STAUNTON" ~ "CITY OF STAUNTON",
      STATE_CODE=="VA" & COUNTY=="LYNCHBURG" ~ "CITY OF LYNCHBURG",
      STATE_CODE=="VA" & COUNTY=="MARTINSVILLE" ~ "CITY OF MARTINSVILLE",
      STATE_CODE=="VA" & COUNTY=="FREDERICKSBURG" ~ "CITY OF FREDERICKSBURG",
      STATE_CODE=="VA" & declaredCountyArea=="Richmond" ~ "CITY OF RICHMOND",
      STATE_CODE=="VA" & COUNTY=="PETERSBURG" ~ "CITY OF PETERSBURG",
      STATE_CODE=="VA" & COUNTY=="NEWPORT NEWS" ~ "CITY OF NEWPORT NEWS",
      STATE_CODE=="VA" & COUNTY=="SUFFOLK" ~ "CITY OF SUFFOLK",
      STATE_CODE=="VA" & COUNTY=="WILLIAMSBURG"~ "CITY OF WILLIAMSBURG",
      STATE_CODE=="VA" & COUNTY=="CHESAPEAKE" ~ "CITY OF CHESAPEAKE",
      STATE_CODE=="VA" & COUNTY=="HAMPTON" ~ "CITY OF HAMPTON",
      STATE_CODE=="VA" & COUNTY=="COLONIAL HEIGHTS" ~ "CITY OF COLONIAL HEIGHTS",
      STATE_CODE=="VA" & COUNTY=="WAYNESBORO" ~ "CITY OF WAYNESBORO",
      STATE_CODE=="VA" & COUNTY=="NORFOLK" ~ "CITY OF NORFOLK",
      STATE_CODE=="VA" & COUNTY=="PORTSMOUTH" ~ "CITY OF PORTSMOUTH",
      STATE_CODE=="VA" & COUNTY=="MANASSAS" ~ "CITY OF MANASSAS",
      STATE_CODE=="VA" & COUNTY=="HOPEWELL" ~ "CITY OF HOPEWELL",
      STATE_CODE=="VA" & declaredCountyArea=="Franklin" ~ "CITY OF FRANKLIN",
      STATE_CODE=="VA" & COUNTY=="EMPORIA" ~ "CITY OF EMPORIA",
      STATE_CODE=="VA" & COUNTY=="POQUOSON" ~ "CITY OF POQUOSON",
      STATE_CODE=="AK" & COUNTY=="HOONAH ANGOON" ~ "HOONAH-ANGOON",
      STATE_CODE=="AK" & (COUNTY=="VALDEZ-CORDOVA" | COUNTY=="VALDEZ CITY SCHOOL DISTRICT" | COUNTY=="CHUGACH REGIONAL EDUCATIONAL ATTENDANCE AREA") ~ "CHUGACH CENSUS AREA",
      STATE_CODE=="AK" & COUNTY=="KUSILVAK CENSUS AREA" ~ "KUSILVAK",
      STATE_CODE=="AK" & COUNTY=="PETERSBURG" ~ "PETERSBURG CENSUS AREA",
      STATE_CODE=="HI" & COUNTY=="HONOLULU" ~ "OAHU IN HONOLULU",
      STATE_CODE=="HI" & COUNTY=="HAWAII" ~ "HAWAII IN HAWAII",
      STATE_CODE=="HI" & COUNTY=="KAUAI" ~ "KAUAI IN KAUAI",
      STATE_CODE=="HI" & COUNTY=="MAUI" ~ "MAUI IN MAUI",
      STATE_CODE=="IL" & COUNTY=="DEKALB" ~ "DE KALB",
      STATE_CODE=="GA" & COUNTY=="GEORGETOWN - QUITMAN" ~ "QUITMAN",
      STATE_CODE=="IN" & COUNTY=="LAPORTE" ~ "LA PORTE",
      STATE_CODE=="IN" & COUNTY=="DEKALB" ~ "DE KALB",
      STATE_CODE=="MD" & COUNTY=="PRINCE GEORGE'S" ~ "PRINCE GEORGES",
      STATE_CODE=="MD" & declaredCountyArea=="Baltimore" ~ "BALTIMORE CITY",
      STATE_CODE=="MD" & COUNTY=="ST. MARY'S" ~ "ST. MARYS",
      STATE_CODE=="MO" & declaredCountyArea=="St. Louis" ~ "ST. LOUIS CITY",
      STATE_CODE=="TN" & COUNTY=="DEKALB" ~ "DE KALB",
      STATE_CODE=="NM" & COUNTY=="DEBACA" ~ "DE BACA",
      T ~ COUNTY
    )
  )

setdiff(
  paste(data$COUNTY, data$STATE_CODE, sep=","),
  paste(DDS$COUNTY, DDS$STATE_CODE, sep=",")
)

DDS %<>% 
  filter(STATE_CODE!="HI") %>% 
  filter(STATE_CODE!="AK")

DDS %<>% 
  filter(BEG_YEAR>1990) %>% 
  filter(BEG_YEAR<2020)

setdiff(
  paste(DDS$COUNTY, DDS$STATE_CODE, sep=","),
  paste(county_shape$COUNTYNAME, county_shape$STATE_CODE, sep=",")
) 

DDS %<>% 
  mutate(
    COUNTY = case_when(
      STATE_CODE=="CT" & COUNTY=="MASHANTUCKET PEQUOT INDIAN RESERVATION"            ~ "NEW LONDON",
      STATE_CODE=="LA" & COUNTY=="WARD 9"                                            ~ "EAST BATON ROUGE",
      STATE_CODE=="FL" & COUNTY=="DADE"                                              ~ "MIAMI-DADE",
      STATE_CODE=="MO" & COUNTY=="KANSAS CITY"                                       ~ "JACKSON",
      STATE_CODE=="MO" & COUNTY=="JEFFERSON CITY"                                    ~ "COLE",
      STATE_CODE=="VA" & COUNTY=="CLIFTON FORGE"                                     ~ "ALLEGHANY",
      STATE_CODE=="SD" & COUNTY=="STANDING ROCK SIOUX TRIBE OF NORTH & SOUTH DAKOTA" ~ "CORSON",
      STATE_CODE=="SD" & COUNTY=="OGLALA SIOUX TRIBE OF THE PINE RIDGE RESERVATION"  ~ "OGLALA LAKOTA",
      STATE_CODE=="ND" & COUNTY=="TURTLE MOUNTAIN INDIAN RESERVATION"                ~ "ROLETTE",
      STATE_CODE=="ND" & COUNTY=="FORT BERTHOLD INDIAN RESERVATION"                  ~ "MOUNTRAIL",
      STATE_CODE=="MT" & COUNTY=="FORT BELKNAP INDIAN RESERVATION"                   ~ "BLAINE",
      STATE_CODE=="ID" & COUNTY=="FORT HALL INDIAN RESERVATION"                      ~ "BANNOCK",
      STATE_CODE=="MT" & COUNTY=="CROW INDIAN RESERVATION"                           ~ "BIG HORN",
      STATE_CODE=="MT" & COUNTY=="BLACKFEET INDIAN RESERVATION"                      ~ "GLACIER",
      STATE_CODE=="MT" & COUNTY=="FLATHEAD INDIAN RESERVATION"                       ~ "FLATHEAD",
      STATE_CODE=="MT" & COUNTY=="NORTHERN CHEYENNE INDIAN RESERVATION"              ~ "ROSEBUD",
      STATE_CODE=="MN" & COUNTY=="RED LAKE INDIAN RESERVATION"                       ~ "BELTRAMI",
      STATE_CODE=="MN" & COUNTY=="MILLE LACS INDIAN RESERVATION"                     ~ "MILLE LACS",
      STATE_CODE=="MN" & COUNTY=="UPPER SIOUX COMMUNITY"                             ~ "YELLOW MEDICINE",
      STATE_CODE=="MN" & COUNTY=="FOND DU LAC INDIAN RESERVATION"                    ~ "CARLTON",
      STATE_CODE=="MN" & COUNTY=="PRAIRIE ISLAND COMMUNITY"                          ~ "GOODHUE",
      STATE_CODE=="CO" & COUNTY=="UTE MOUNTAIN INDIAN RESERVATION"                   ~ "MONTEZUMA",
      STATE_CODE=="CO" & COUNTY=="SOUTHERN UTE INDIAN RESERVATION"                   ~ "LA PLATA",
      STATE_CODE=="AZ" & COUNTY=="WHITE MOUNTAIN APACHE TRIBE"                       ~ "NAVAJO",
      STATE_CODE=="VA" & COUNTY=="MANASSAS PARK"                                     ~ as.character(NA),
      STATE_CODE=="ID" & COUNTY=="NEZ PERCE INDIAN RESERVATION"                      ~ "NEZ PERCE",
      STATE_CODE=="WY" & COUNTY=="WIND RIVER INDIAN RESERVATION"                     ~ "FREMONT",
      STATE_CODE=="ND" & COUNTY=="SPIRIT LAKE RESERVATION"                           ~ "BENSON",
      STATE_CODE=="NM" & COUNTY=="PUEBLO OF ACOMA"                                   ~ "CIBOLA",
      STATE_CODE=="NM" & COUNTY=="PUEBLO OF PICURIS"                                 ~ "TAOS",
      STATE_CODE=="CO" & COUNTY=="NA"                                                ~ as.character(NA),
      STATE_CODE=="NM" & COUNTY=="NA"                                                ~ as.character(NA),
      STATE_CODE=="OR" & COUNTY=="NA"                                                ~ as.character(NA),
      STATE_CODE=="OK" & COUNTY=="NA"                                                ~ as.character(NA),
      STATE_CODE=="NM" & COUNTY=="COCHITI PUEBLO"                                    ~ "SANDOVAL",
      STATE_CODE=="TX" & COUNTY=="NA"                                                ~ as.character(NA),
      STATE_CODE=="NM" & COUNTY=="JEMEZ PUEBLO"                                      ~ "SANDOVAL",
      STATE_CODE=="NM" & COUNTY=="SANTA CLARA PUEBLO"                                ~ "RIO ARRIBA",
      STATE_CODE=="FL" & COUNTY=="NA"                                                ~ as.character(NA),
      STATE_CODE=="NY" & COUNTY=="CAYUGA NATION"                                     ~ "CAYUGA",
      STATE_CODE=="CA" & COUNTY=="HOOPA VALLEY INDIAN RESERVATION"                   ~ "HUMBOLDT",
      STATE_CODE=="CA" & COUNTY=="YUROK INDIAN RESERVATION"                          ~ "DEL NORTE",
      STATE_CODE=="VA" & COUNTY=="MATTAPONI INDIAN RESERVATION"                      ~ "KING WILLIAM",
      STATE_CODE=="WA" & COUNTY=="KALISPEL INDIAN RESERVATION"                       ~ "PEND OREILLE",
      STATE_CODE=="WA" & COUNTY=="YAKAMA RESERVATION"                                ~ "YAKIMA",
      STATE_CODE=="WA" & COUNTY=="SPOKANE INDIAN RESERVATION"                        ~ "PEND OREILLE",
      STATE_CODE=="VA" & COUNTY=="PAMUNKEY INDIAN RESERVATION"                       ~ "KING WILLIAM",
      STATE_CODE=="SC" & COUNTY=="CATAWBA INDIAN RESERVATION"                        ~ "YORK",
      STATE_CODE=="AL" & COUNTY=="POARCH BAND OF CREEK INDIANS"                      ~ "BALDWIN",
      STATE_CODE=="FL" & COUNTY=="HOLLYWOOD INDIAN RESERVATION"                      ~ "BROWARD",
      STATE_CODE=="FL" & COUNTY=="BIG CYPRESS INDIAN RESERVATION"                    ~ "BROWARD",
      STATE_CODE=="FL" & COUNTY=="FORT PIERCE INDIAN RESERVATION"                    ~ "ST. LUCIE",
      STATE_CODE=="FL" & COUNTY=="BRIGHTON INDIAN RESERVATION"                       ~ "GLADES",
      STATE_CODE=="FL" & COUNTY=="IMMOKALEE INDIAN RESERVATION"                      ~ "COLLIER",
      STATE_CODE=="FL" & COUNTY=="TAMPA RESERVATION"                                 ~ "HILLSBOROUGH",
      STATE_CODE=="NE" & COUNTY=="OMAHA INDIAN RESERVATION"                          ~ "THURSTON",
      STATE_CODE=="NM" & COUNTY=="MESCALERO TRIBE"                                   ~ "OTERO",
      STATE_CODE=="MS" & COUNTY=="MISSISSIPPI CHOCTAW INDIAN RESERVATION"            ~ "NESHOBA",
      STATE_CODE=="CA" & COUNTY=="KARUK RESERVATION"                                 ~ "SISKIYOU",
      STATE_CODE=="NM" & COUNTY=="ISLETA PUEBLO"                                     ~ "BERNALILLO",
      STATE_CODE=="NM" & COUNTY=="SANDIA PUEBLO"                                     ~ "BERNALILLO",
      STATE_CODE=="WA" & COUNTY=="SAUK-SUIATTLE INDIAN RESERVATION"                  ~ "SNOHOMISH",
      STATE_CODE=="WA" & COUNTY=="TULALIP INDIAN RESERVATION"                        ~ "SNOHOMISH",
      STATE_CODE=="WA" & COUNTY=="STILLAGUAMISH INDIAN RESERVATION"                  ~ "SNOHOMISH",
      STATE_CODE=="IA" & COUNTY=="SAC AND FOX TRIBE OF THE MISSISSIPPI IN IOWA"      ~ "LINN",
      STATE_CODE=="CA" & COUNTY=="SOBOBA INDIAN RESERVATION"                         ~ "RIVERSIDE",
      STATE_CODE=="MN" & COUNTY=="BOIS FORTE"                                        ~ "ST. LOUIS",
      STATE_CODE=="NV" & COUNTY=="MOAPA RIVER INDIAN RESERVATION"                    ~ "CLARK",
      STATE_CODE=="CA" & COUNTY=="TULE RIVER INDIAN RESERVATION"                     ~ "TULARE",
      STATE_CODE=="AZ" & COUNTY=="HAVASUPAI INDIAN RESERVATION"                      ~ "COCONINO",
      STATE_CODE=="MN" & COUNTY=="LEECH LAKE INDIAN RESERVATION"                     ~ "CASS",
      STATE_CODE=="NE" & COUNTY=="PONCA"                                             ~ "DIXON",
      STATE_CODE=="NE" & COUNTY=="SAC AND FOX INDIAN RESERVATION"                    ~ "RICHARDSON",
      STATE_CODE=="NE" & COUNTY=="SANTEE INDIAN RESERVATION"                         ~ "KNOX",
      STATE_CODE=="NE" & COUNTY=="WINNEBAGO INDIAN RESERVATION"                      ~ "THURSTON",
      STATE_CODE=="NE" & COUNTY=="PONCA TDSA"                                        ~ "DIXON",
      STATE_CODE=="WI" & COUNTY=="MENOMINEE INDIAN RESERVATION"                      ~ "MENOMINEE",
      STATE_CODE=="WI" & COUNTY=="ST. CROIX INDIAN RESERVATION"                      ~ "BURNETT",
      STATE_CODE=="CA" & COUNTY=="CAMPO INDIAN RESERVATION"                          ~ "SAN DIEGO",
      STATE_CODE=="CA" & COUNTY=="HOPLAND RANCHERIA"                                 ~ "MENDOCINO",
      STATE_CODE=="DC" & COUNTY=="NA"                                                ~ "DISTRICT OF COLUMBIA",
      STATE_CODE=="VA" & COUNTY=="SOUTH BOSTON"                                      ~ "HALIFAX",
      STATE_CODE=="SD" & COUNTY=="WEST WASHABAUGH"                                   ~ "JACKSON",
      STATE_CODE=="TX" & COUNTY=="ALABAMA AND COUSHATTA INDIAN RESERVATION"          ~ "POLK",
      STATE_CODE=="TX" & COUNTY=="YSLETA DEL SUR PUEBLO"                             ~ "EL PASO",
      STATE_CODE=="NV" & COUNTY=="PYRAMID LAKE INDIAN RESERVATION"                   ~ "WASHOE",
      STATE_CODE=="OR" & COUNTY=="WARM SPRINGS INDIAN RESERVATION"                   ~ "JEFFERSON",
      STATE_CODE=="NV" & COUNTY=="WASHOE INDIAN RESERVATION"                         ~ "WASHOE",
      STATE_CODE=="AZ" & COUNTY=="TOHONO O'ODHAM RESERVATION AND TRUST LANDS"        ~ "PIMA",
      STATE_CODE=="SD" & COUNTY=="CROW CREEK INDIAN RESERVATION"                     ~ "BUFFALO",
      STATE_CODE=="SD" & COUNTY=="LOWER BRULE INDIAN RESERVATION"                    ~ "LYMAN",
      STATE_CODE=="SD" & COUNTY=="FLANDREAU INDIAN RESERVATION"                      ~ "MOODY",
      STATE_CODE=="NM" & COUNTY=="PUEBLO OF SANTA CLARA"                             ~ "RIO ARRIBA",
      STATE_CODE=="NM" & COUNTY=="PUEBLO OF TAOS"                                    ~ "TAOS",
      STATE_CODE=="CT" & COUNTY=="PAUCATUCK EASTERN PEQUOT INDIAN RESERVATION"       ~ "NEW LONDON",
      STATE_CODE=="NM" & COUNTY=="PUEBLO OF POJOAQUE"                                ~ "SANTA FE",
      STATE_CODE=="NM" & COUNTY=="PUEBLO OF SAN FELIPE"                              ~ "SANDOVAL",
      STATE_CODE=="NM" & COUNTY=="PUEBLO OF SANTA ANA"                               ~ "SANDOVAL",
      STATE_CODE=="NM" & COUNTY=="SAN FELIPE PUEBLO"                                 ~ "SANDOVAL",
      STATE_CODE=="MN" & COUNTY=="GRAND PORTAGE INDIAN RESERVATION"                  ~ "COOK",
      STATE_CODE=="WI" & COUNTY=="RED CLIFF INDIAN RESERVATION"                      ~ "ASHLAND",
      STATE_CODE=="NM" & COUNTY=="SANTO DOMINGO PUEBLO"                              ~ "SANDOVAL",
      STATE_CODE=="ID" & COUNTY=="COEUR D'ALENE INDIAN RESERVATION"                  ~ "KOOTENAI",
      STATE_CODE=="WI" & COUNTY=="BAD RIVER INDIAN RESERVATION"                      ~ "ASHLAND",
      STATE_CODE=="CA" & COUNTY=="RESIGHINI RANCHERIA"                               ~ "DEL NORTE",
      STATE_CODE=="NV" & COUNTY=="RENO-SPARKS COLONY"                                ~ "WASHOE",
      STATE_CODE=="CA" & COUNTY=="LA JOLLA INDIAN RESERVATION"                       ~ "SAN DIEGO",
      STATE_CODE=="CA" & COUNTY=="CAHUILLA INDIAN RESERVATION"                       ~ "RIVERSIDE",
      STATE_CODE=="SD" & COUNTY=="YANKTON INDIAN RESERVATION"                        ~ "CHARLES MIX",
      T ~ COUNTY
    )
  )

setdiff(
  paste(DDS$COUNTY, DDS$STATE_CODE, sep=","),
  paste(data$COUNTY, data$STATE_CODE, sep=",")
)

#statewide

DDS %>% 
  filter(paste(DDS$COUNTY, DDS$STATE_CODE, sep=",") %in% setdiff(
    paste(DDS$COUNTY, DDS$STATE_CODE, sep=","),
    paste(data$COUNTY, data$STATE_CODE, sep=",")
  )) -> prob

DDS %>% 
  filter(!(paste(DDS$COUNTY, DDS$STATE_CODE, sep=",") %in% setdiff(
    paste(DDS$COUNTY, DDS$STATE_CODE, sep=","),
    paste(data$COUNTY, data$STATE_CODE, sep=",")
  ))) -> no_prob

to_fix <- tibble(
  COUNTY = c("DEVILS LAKE SIOUX INDIAN RESERVATION","ROCKY BOY'S INDIAN RESERVATION","WHITE EARTH INDIAN RESERVATION","STATEWIDE",
             "EASTERN BAND OF CHEROKEE INDIANS OF NORTH CAROLINA","UINTAH AND OURAY INDIAN RESERVATION","SILETZ INDIAN RESERVATION",
             "LAKE TRAVERSE","NAVAJO NATION RESERVATION","STATEWIDE","STATEWIDE","STATEWIDE","STATEWIDE","COLVILLE INDIAN RESERVATION",
             "STATEWIDE","CROW/NORTHERN CHEYENNE AREA","NAVAJO NATION RESERVATION","FORT PECK INDIAN RESERVATION","STANDING ROCK SIOUX TRIBE OF NORTH & SOUTH DAKOTA",
             "ROSEBUD INDIAN RESERVATION","CHEYENNE RIVER INDIAN RESERVATION","STATEWIDE","STATEWIDE","GILA RIVER INDIAN RESERVATION","STATEWIDE",
             "SAN CARLOS INDIAN RESERVATION","HOPI INDIAN RESERVATION","COOS,LOWER UNQUA AND SIUSLAW INDIAN RESERVATION","GRAND RONDE INDIAN RESERVATION",
             "LAKE TRAVERSE SISSETON INDIAN RESERVATION"),
  STATE_CODE = c("ND","MT","MN","TX","NC","UT","OR","SD","NM","NY","WI","MO","PA","WA","RI","MT","AZ",
                 "MT","ND","SD","SD","AZ","ND","AZ","IN","AZ","AZ","OR","OR","ND"),
  COUNTY_NEW = list(
    c("BENSON", "EDDY", "RAMSEY", "TOWNER"),
    c("CHOUTEAU", "HILL"),
    c("CLEARWATER", "MAHNOMEN", "BECKER"),
    county_shape %>% filter(STATE_CODE=="TX") %>% pull(COUNTYNAME) %>% unique(),
    c("SWAIN", "JACKSON", "HAYWOOD"),
    c("UINTAH", "DUCHESNE"),
    c("LINCOLN", "POLK", "TILLAMOOK"),
    c("CODINGTON", "DAY", "GRANT", "MARSHALL", "ROBERTS"),
    c("SAN JUAN", "MCKINLEY"),
    county_shape %>% filter(STATE_CODE=="NY") %>% pull(COUNTYNAME) %>% unique(),
    county_shape %>% filter(STATE_CODE=="WI") %>% pull(COUNTYNAME) %>% unique(),
    county_shape %>% filter(STATE_CODE=="MO") %>% pull(COUNTYNAME) %>% unique(),
    county_shape %>% filter(STATE_CODE=="PA") %>% pull(COUNTYNAME) %>% unique(),
    c("FERRY", "OKANOGAN", "STEVENS"),
    county_shape %>% filter(STATE_CODE=="RI") %>% pull(COUNTYNAME) %>% unique(),
    c("BIG HORN", "ROSEBUD", "TREASURE", "YELLOWSTONE"),
    c("APACHE", "COCONINO", "NAVAJO"),
    c("VALLEY", "ROOSEVELT", "DANIELS", "SHERIDAN"),
    c("SIOUX", "MORTON"),
    c("TODD", "TRIPP"),
    c("DEWEY", "ZIEBACH"),
    county_shape %>% filter(STATE_CODE=="AZ") %>% pull(COUNTYNAME) %>% unique(),
    county_shape %>% filter(STATE_CODE=="ND") %>% pull(COUNTYNAME) %>% unique(),
    c("PINAL", "MARICOPA"),
    county_shape %>% filter(STATE_CODE=="IN") %>% pull(COUNTYNAME) %>% unique(),
    c("GRAHAM", "GILA"),
    c("NAVAJO", "COCONINO"),
    c("COOS", "LANE"),
    c("POLK", "YAMHILL", "MARION"),
    c("SARGENT", "RICHLAND")
  )
)

prob %<>% 
  left_join(
    .,
    to_fix, 
    by=c("STATE_CODE", "COUNTY")
  ) %>% 
  unnest(COUNTY_NEW) %>% 
  select(-COUNTY) %>% 
  rename(COUNTY=COUNTY_NEW)

DDS <- rbind(
  no_prob,
  prob
)

#There are some events which appear multiple times, because of different activated programs. 
#Group by begin date, and select all activated programs

DDS %<>% 
  select(
    STATE_CODE,COUNTY,disasterNumber,incidentType,title,
    incidentBeginDate, incidentEndDate,
    ihProgramDeclared, iaProgramDeclared, paProgramDeclared, hmProgramDeclared
  ) %>% 
  rename(
    DISASTER_ID = disasterNumber,
    TYPE = incidentType,
    TITLE = title,
    DIS_BEG = incidentBeginDate,
    DIS_END = incidentEndDate
  )

DDS %<>% 
  mutate(DIS_BEG_GROUP = as.character(DIS_BEG)) %>% 
  group_by(STATE_CODE, COUNTY, DIS_BEG_GROUP) %>% 
  summarise(
    #select the lowest DIS_END
    DIS_BEG = DIS_BEG[1],
    DIS_END = min(DIS_END),
    DISASTER_ID = DISASTER_ID[1],
    TYPE = TYPE[1],
    TITLE = TITLE[1],
    ihProgramDeclared = any(ihProgramDeclared==1),
    iaProgramDeclared = any(iaProgramDeclared==1), 
    paProgramDeclared = any(paProgramDeclared==1), 
    hmProgramDeclared = any(hmProgramDeclared==1)
  ) %>% ungroup() 

data %>% 
  left_join(
    .,
    DDS,
    by=c("STATE_CODE", "COUNTY")
  ) -> dis_event

dis_event %<>% 
  mutate(
    is_in_disaster = ifelse(BEG_T >= DIS_BEG & END_T <= DIS_END & TYPE %in% c("Coastal Storm", "Severe Storm"), T, F),#, #DIS EV FIN_EV FIN_DIS
    is_in_disaster2 = ifelse(BEG_T >= DIS_BEG & END_T <= DIS_END & TYPE %in% c("Coastal Storm", "Severe Storm", "Flood", "Mud/Landslide"), T, F),
    is_in_disaster3 = ifelse(BEG_T >= DIS_BEG & END_T <= DIS_END & TYPE %in% c("Coastal Storm", "Severe Storm", "Flood", "Mud/Landslide", "Severe Ice Storm", "Snowstorm", "Tornado", "Hurricane"), T, F)
  ) %>% 
  filter(is_in_disaster3)

dis_event %<>% 
  group_by(EVENT_ID) %>% 
  summarise(
    COUNTY = COUNTY[1],
    BEG_T = BEG_T[1],
    END_T = END_T[1],
    MAGNITUDE = MAGNITUDE[1],
    MAGNITUDE_TYPE = MAGNITUDE_TYPE[1],
    EVENT_TYPE = EVENT_TYPE[1],
    EPISODE_ID = EPISODE_ID[1],
    INJURIES_DIRECT = INJURIES_DIRECT[1],
    INJURIES_INDIRECT = INJURIES_INDIRECT[1],
    DEATHS_DIRECT = DEATHS_DIRECT[1],
    DEATHS_INDIRECT = DEATHS_INDIRECT[1],
    DAMAGE_PROPERTY = DAMAGE_PROPERTY[1],
    DAMAGE_CROPS = DAMAGE_CROPS[1],
    BEGIN_LON = BEGIN_LON[1],
    END_LON = END_LON[1],
    BEGIN_LAT = BEGIN_LAT[1],
    END_LAT = END_LAT[1],
    STATE = STATE[1],
    STATE_CODE = STATE_CODE[1],
    DIS_BEG = min(DIS_BEG),
    DIS_END = max(DIS_END),
    DISASTER_ID = DISASTER_ID[1],
    TYPE = TYPE[1],
    TITLE = TITLE[1],
    is_in_disaster = any(is_in_disaster),
    is_in_disaster2 = any(is_in_disaster2),
    is_in_disaster3 = any(is_in_disaster3),
    ihProgramDeclared = any(ihProgramDeclared),
    iaProgramDeclared = any(iaProgramDeclared),
    paProgramDeclared = any(paProgramDeclared),
    hmProgramDeclared = any(hmProgramDeclared)
  )

#merge

data %<>% 
  left_join(
    .,
    dis_event %>% 
      select(EVENT_ID, DIS_BEG, DIS_END, DISASTER_ID, TYPE,
             TITLE, is_in_disaster, is_in_disaster2, is_in_disaster3,
             ihProgramDeclared, iaProgramDeclared, paProgramDeclared, hmProgramDeclared)
  ) #implicit merge by "EVENT_ID


data %<>% 
  rename(
    DISASTER_BEG = DIS_BEG,
    DISASTER_END = DIS_END,
    DISASTER_TYPE = TYPE,
    DISASTER_TITLE = TITLE, 
  ) %>% 
  mutate(
    IS_IN_DISASTER = ifelse((!is.na(DISASTER_ID)) & is_in_disaster, T, F),
    IS_IN_DISASTER2 = ifelse((!is.na(DISASTER_ID)) & is_in_disaster2, T, F),
    IS_IN_DISASTER3 = ifelse(is.na(DISASTER_ID), F, T), #having a non-NA DISASTER_ID is equal to having is_in_disaster3==T
    ihProgramDeclared = ifelse(IS_IN_DISASTER, ihProgramDeclared, F),
    iaProgramDeclared = ifelse(IS_IN_DISASTER, iaProgramDeclared, F), 
    paProgramDeclared = ifelse(IS_IN_DISASTER, paProgramDeclared, F), 
    hmProgramDeclared = ifelse(IS_IN_DISASTER, hmProgramDeclared, F), 
  ) %>% 
  select(-is_in_disaster, -is_in_disaster2, -is_in_disaster3) %>% 
  relocate(EVENT_ID, STATE_CODE, STATE, COUNTY)

#separate table for external events

DDS %>% 
  mutate(YEAR=str_sub(DIS_BEG, 1, 4)) %>% 
  group_by(YEAR, COUNTY, STATE_CODE) %>% 
  summarise(
    DISASTER_YEAR = any(TYPE%in%c("Coastal Storm", "Severe Storm")),
    DISASTER_YEAR2 = any(TYPE%in%c("Coastal Storm", "Severe Storm", "Flood", "Mud/Landslide")),
    DISASTER_YEAR3 = any(TYPE%in%c("Coastal Storm", "Severe Storm", "Flood", "Mud/Landslide", "Severe Ice Storm", "Snowstorm", "Tornado", "Hurricane")),
    DISASTER_YEAR_EXT = any(TYPE%in%c("Chemical", "Dam/levee Break", "Drought", "Earthquake", "Fire", "Fishing Losses", "Freezing", "Human Cause",
                                      "Other", "Terrorist", "Toxic Substances", "Tsunami")),
    DISASTER_YEAR_ANY = T,
  ) %>% 
  ungroup() -> dis_year

dis_year %<>% 
  left_join(
    .,
    county_shape %>% select(STATE_CODE, COUNTYNAME,FIPS),
    by=c("STATE_CODE"="STATE_CODE", "COUNTY"="COUNTYNAME")
  )

#### AREA ####

library(readxl)
county_area <- read_excel("utilities/LND01.xls")

county_area %<>% 
  select(Areaname, STCOU, LND110210D) %>% 
  group_by(Areaname) %>% 
  summarise(
    LND110210D = sum(LND110210D),
    STCOU = STCOU[1]
  )

#filtery by FIPS ending in 000
county_area %<>% 
  mutate(
    filt = str_sub(STCOU,-3,-1)
  ) %>% 
  filter(filt!="000") %>% 
  select(-filt) %>% 
  mutate(Areaname = ifelse(Areaname=="District of Columbia", "District of Columbia, DC", Areaname)) %>% 
  separate(Areaname, into = c("COUNTY", "STATE_CODE"), sep=", ") %>% 
  mutate(COUNTY=toupper(COUNTY)) %>% 
  mutate(
    STCOU = ifelse(STCOU=="46113", "46102", STCOU) #shannon to oglala lakota
  ) %>% 
  rbind( #manually add those missing
    .,
    tibble(
      COUNTY = c("CITY OF ROANOKE", "CITY OF FAIRFAX", "CITY OF RICHMOND", "CITY OF FRANKLIN", "CHUGACH CENSUS AREA", "KUSILVAK"),
      STATE_CODE = c("VA","VA","VA","VA","AK","AK"),
      LND110210D = c(42.85,6.27,62.57,8.367,40340,19673),
      STCOU = c("51770","51600","51760","51620","02063","02158")
    )
  )

data %<>% 
  left_join(
    .,
    county_area %>% 
      rename(COUNTY_AREA = LND110210D) %>% 
      select(COUNTY_AREA, STCOU), 
    by = c("FIPS"="STCOU")
  )

####  Final adjustments ####

data %<>% 
  mutate(
    YEAR = as.numeric(lubridate::year(BEG_T)),
    FIPS=case_when(
      FIPS %in% c("02275","02195") ~ "02280",
      FIPS=="08014" & YEAR<2002 ~ "08013", #Events in Broomfield prior to 2002 are imputed to Boulder County
      T ~ FIPS
    )
  ) %>% 
  filter(STATE!="ALASKA") %>% 
  filter(STATE!="HAWAII") %>% 
  relocate(EVENT_ID, STATE_CODE, STATE, COUNTY, FIPS, YEAR, EPISODE_ID)

#### Aggregation Needed to be comparable with Income data ####

data %<>% 
  mutate(
    COUNTY_AG = case_when(
      COUNTY%in%c("ALBEMARLE","CITY OF CHARLOTTESVILLE") ~ "ALBEMARLE_CITY OF CHARLOTTESVILLE",
      COUNTY%in%c("ALLEGHANY","CITY OF COVINGTON") ~ "ALLEGHANY_CITY OF COVINGTON",
      COUNTY%in%c("AUGUSTA","CITY OF STAUNTON","CITY OF WAYNESBORO") ~ "AUGUSTA_CITY OF STAUNTON_CITY OF WAYNESBORO",
      COUNTY%in%c("CAMPBELL","CITY OF LYNCHBURG") ~ "CAMPBELL_CITY OF LYNCHBURG",
      COUNTY%in%c("CARROLL","CITY OF GALAX") ~ "CARROLL_CITY OF GALAX",
      COUNTY%in%c("DINWIDDIE","CITY OF COLONIAL HEIGHTS","CITY OF PETERSBURG") ~ "DINWIDDIE_CITY OF COLONIAL HEIGHTS_CITY OF PETERSBURG",
      COUNTY%in%c("FAIRFAX","CITY OF FAIRFAX","CITY OF FALLS CHURCH") ~ "FAIRFAX_CITY OF FAIRFAX_CITY OF FALLS CHURCH",
      COUNTY%in%c("FREDERICK","CITY OF WINCHESTER") ~ "FREDERICK_CITY OF WINCHESTER",
      COUNTY%in%c("GREENSVILLE","CITY OF EMPORIA") ~ "GREENSVILLE_CITY OF EMPORIA",
      COUNTY%in%c("HENRY","CITY OF MARTINSVILLE") ~ "HENRY_CITY OF MARTINSVILLE",
      COUNTY%in%c("JAMES CITY","CITY OF WILLIAMSBURG") ~ "JAMES CITY_CITY OF WILLIAMSBURG",
      COUNTY%in%c("MONTGOMERY","CITY OF RADFORD") ~ "MONTGOMERY_CITY OF RADFORD",
      COUNTY%in%c("PITTSYLVANIA","CITY OF DANVILLE") ~ "PITTSYLVANIA_CITY OF DANVILLE",
      COUNTY%in%c("PRINCE GEORGE","CITY OF HOPEWELL") ~ "PRINCE GEORGE_CITY OF HOPEWELL",
      COUNTY%in%c("PRINCE WILLIAM","CITY OF MANASSAS") ~ "PRINCE WILLIAM_CITY OF MANASSAS",
      COUNTY%in%c("ROANOKE","CITY OF SALEM") ~ "ROANOKE_CITY OF SALEM",
      COUNTY%in%c("ROCKBRIDGE","CITY OF BUENA VISTA","CITY OF LEXINGTON") ~ "ROCKBRIDGE_CITY OF BUENA VISTA_CITY OF LEXINGTON",
      COUNTY%in%c("ROCKINGHAM","CITY OF HARRISONBURG") ~ "ROCKINGHAM_CITY OF HARRISONBURG",
      COUNTY%in%c("SOUTHAMPTON","CITY OF FRANKLIN") ~ "SOUTHAMPTON_CITY OF FRANKLIN",
      COUNTY%in%c("SPOTSYLVANIA","CITY OF FREDERICKSBURG") ~ "SPOTSYLVANIA_CITY OF FREDERICKSBURG",
      COUNTY%in%c("WASHINGTON","CITY OF BRISTOL") ~ "WASHINGTON_CITY OF BRISTOL",
      COUNTY%in%c("WISE","CITY OF NORTON") ~ "WISE_CITY OF NORTON",
      COUNTY%in%c("YORK","CITY OF POQUOSON") ~ "YORK_CITY OF POQUOSON",
      T ~ COUNTY
    ),
    FIPS_AG = case_when(
      FIPS%in%c("51003","51540") ~ "51901",
      FIPS%in%c("51005","51580") ~ "51903",
      FIPS%in%c("51015","51790","51820") ~ "51907",
      FIPS%in%c("51031","51680") ~ "51911",
      FIPS%in%c("51035","51640") ~ "51913",
      FIPS%in%c("51053","51570","51730") ~ "51918",
      FIPS%in%c("51059","51600","51610") ~ "51919",
      FIPS%in%c("51069","51840") ~ "51921",
      FIPS%in%c("51081","51595") ~ "51923",
      FIPS%in%c("51089","51690") ~ "51929",
      FIPS%in%c("51095","51830") ~ "51931",
      FIPS%in%c("51121","51750") ~ "51933",
      FIPS%in%c("51143","51590") ~ "51939",
      FIPS%in%c("51149","51670") ~ "51941",
      FIPS%in%c("51153","51683") ~ "51942",
      FIPS%in%c("51161","51775") ~ "51944",
      FIPS%in%c("51163","51530","51678") ~ "51945",
      FIPS%in%c("51165","51660") ~ "51947",
      FIPS%in%c("51175","51620") ~ "51949",
      FIPS%in%c("51177","51630") ~ "51951",
      FIPS%in%c("51191","51520") ~ "51953",
      FIPS%in%c("51195","51720") ~ "51955",
      FIPS%in%c("51199","51735") ~ "51958",
      T ~ FIPS
    )
  )

saveRDS(data,file="data/clean_storm_step3.rds")
saveRDS(dis_year,file="data/dis_year.rds")

#For report in manuscript

data %>% 
  filter(YEAR>1990) %>% 
  filter(YEAR<2020) %>% 
  filter(IS_IN_DISASTER) %>% 
  nrow() -> a

data %>% 
  filter(YEAR>1990) %>% 
  filter(YEAR<2020) %>% 
  nrow() -> b

data %>% 
  filter(YEAR>1990) %>% 
  filter(YEAR<2020) %>% 
  filter(IS_IN_DISASTER2) %>% 
  nrow() -> c

data %>% 
  filter(YEAR>1990) %>% 
  filter(YEAR<2020) %>% 
  filter(IS_IN_DISASTER3) %>% 
  nrow() -> d

a
a/b
c
c/b
d
d/b
