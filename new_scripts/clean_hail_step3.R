library(tidyverse)
library(sf)
library(magrittr)
library(tictoc)
library(tmaptools, warn.conflicts =FALSE)

rm(list = ls())

data <- readRDS(file="data/clean_hail_step2.rds")

#manipulate times and county by episode
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

prob_ep<- prob_ep[!is.na(prob_ep)]

data %>% 
  filter(is.na(END_T)) %>% pull(EPISODE_ID) %>% unique() -> prob_ep_e

prob_ep_e<- prob_ep_e[!is.na(prob_ep_e)]

#first we manually add one our to problematic cases in order to avoid NA
data %<>% 
  mutate(
    BEGIN_DATE_TIME = case_when(
      BEGIN_DATE_TIME == "29-MAR-92 02:40:00" ~ "29-MAR-92 03:40:00",
      BEGIN_DATE_TIME == "27-MAR-94 02:50:00" ~ "27-MAR-94 03:50:00",
      BEGIN_DATE_TIME == "30-MAR-97 02:10:00" ~ "30-MAR-97 03:10:00",
      BEGIN_DATE_TIME == "26-MAR-00 02:34:00" ~ "26-MAR-00 03:34:00",
      BEGIN_DATE_TIME == "26-MAR-00 02:40:00" ~ "26-MAR-00 03:40:00",
      BEGIN_DATE_TIME == "26-MAR-00 02:40:00" ~ "26-MAR-00 03:40:00",
      BEGIN_DATE_TIME == "26-MAR-00 02:40:00" ~ "26-MAR-00 03:40:00",
      BEGIN_DATE_TIME == "26-MAR-00 02:20:00" ~ "26-MAR-00 03:20:00",
      BEGIN_DATE_TIME == "26-MAR-00 02:20:00" ~ "26-MAR-00 03:20:00",
      BEGIN_DATE_TIME == "31-MAR-02 02:25:00" ~ "31-MAR-02 03:25:00",
      BEGIN_DATE_TIME == "31-MAR-02 02:25:00" ~ "31-MAR-02 03:25:00",
      BEGIN_DATE_TIME == "31-MAR-02 02:50:00" ~ "31-MAR-02 03:50:00",
      BEGIN_DATE_TIME == "27-MAR-05 02:35:00" ~ "27-MAR-05 03:35:00",
      BEGIN_DATE_TIME == "27-MAR-05 02:50:00" ~ "27-MAR-05 03:50:00",
      BEGIN_DATE_TIME == "27-MAR-05 02:31:00" ~ "27-MAR-05 03:31:00",
      BEGIN_DATE_TIME == "27-MAR-05 02:36:00" ~ "27-MAR-05 03:36:00",
      BEGIN_DATE_TIME == "27-MAR-11 02:41:00" ~ "27-MAR-11 03:41:00",
      BEGIN_DATE_TIME == "25-MAR-12 02:50:00" ~ "25-MAR-12 03:50:00",
      BEGIN_DATE_TIME == "25-MAR-12 02:20:00" ~ "25-MAR-12 03:20:00",
      BEGIN_DATE_TIME == "31-MAR-13 02:46:00" ~ "31-MAR-13 03:46:00",
      BEGIN_DATE_TIME == "31-MAR-13 02:30:00" ~ "31-MAR-13 03:30:00",
      BEGIN_DATE_TIME == "31-MAR-13 02:32:00" ~ "31-MAR-13 03:32:00",
      BEGIN_DATE_TIME == "31-MAR-13 02:52:00" ~ "31-MAR-13 03:52:00",
      BEGIN_DATE_TIME == "31-MAR-13 02:54:00" ~ "31-MAR-13 03:54:00",
      BEGIN_DATE_TIME == "31-MAR-13 02:55:00" ~ "31-MAR-13 03:55:00",
      T ~ BEGIN_DATE_TIME
    ),
    END_DATE_TIME = case_when(
      END_DATE_TIME == "29-MAR-92 02:40:00" ~ "29-MAR-92 03:40:00",
      END_DATE_TIME == "27-MAR-94 02:50:00" ~ "27-MAR-94 03:50:00",
      END_DATE_TIME == "30-MAR-97 02:10:00" ~ "30-MAR-97 03:10:00",
      END_DATE_TIME == "26-MAR-00 02:34:00" ~ "26-MAR-00 03:34:00",
      END_DATE_TIME == "26-MAR-00 02:40:00" ~ "26-MAR-00 03:40:00",
      END_DATE_TIME == "26-MAR-00 02:40:00" ~ "26-MAR-00 03:40:00",
      END_DATE_TIME == "26-MAR-00 02:40:00" ~ "26-MAR-00 03:40:00",
      END_DATE_TIME == "26-MAR-00 02:20:00" ~ "26-MAR-00 03:20:00",
      END_DATE_TIME == "26-MAR-00 02:20:00" ~ "26-MAR-00 03:20:00",
      END_DATE_TIME == "31-MAR-02 02:25:00" ~ "31-MAR-02 03:25:00",
      END_DATE_TIME == "31-MAR-02 02:25:00" ~ "31-MAR-02 03:25:00",
      END_DATE_TIME == "31-MAR-02 02:52:00" ~ "31-MAR-02 03:52:00",
      END_DATE_TIME == "27-MAR-05 02:40:00" ~ "27-MAR-05 03:40:00",
      END_DATE_TIME == "27-MAR-05 02:55:00" ~ "27-MAR-05 03:55:00",
      END_DATE_TIME == "27-MAR-05 02:31:00" ~ "27-MAR-05 03:31:00",
      END_DATE_TIME == "27-MAR-05 02:36:00" ~ "27-MAR-05 03:36:00",
      END_DATE_TIME == "27-MAR-11 02:51:00" ~ "27-MAR-11 03:51:00",
      END_DATE_TIME == "25-MAR-12 02:50:00" ~ "25-MAR-12 03:50:00",
      END_DATE_TIME == "25-MAR-12 02:20:00" ~ "25-MAR-12 03:20:00",
      END_DATE_TIME == "31-MAR-13 02:46:00" ~ "31-MAR-13 03:46:00",
      END_DATE_TIME == "31-MAR-13 02:30:00" ~ "31-MAR-13 03:30:00",
      END_DATE_TIME == "31-MAR-13 02:32:00" ~ "31-MAR-13 03:32:00",
      END_DATE_TIME == "31-MAR-13 02:52:00" ~ "31-MAR-13 03:52:00",
      END_DATE_TIME == "31-MAR-13 02:54:00" ~ "31-MAR-13 03:54:00",
      END_DATE_TIME == "31-MAR-13 02:55:00" ~ "31-MAR-13 03:55:00",
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

#One iteration

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


data <- rbind(
  to_group ,
  not_to_group %>% 
    select(
      BEG_T,END_T,MAGNITUDE, MAGNITUDE_TYPE,EVENT_TYPE,EVENT_ID,EPISODE_ID,INJURIES_DIRECT,INJURIES_INDIRECT,
      DEATHS_DIRECT,DEATHS_INDIRECT,DAMAGE_PROPERTY,DAMAGE_CROPS,BEGIN_LON,END_LON,BEGIN_LAT,END_LAT,
      STATE_CODE, STATE, FIPS
    )
)

saveRDS(data,file="data/clean_hail_step3_nested.rds")

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

#merge 
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

#filter FIPS ending in 000
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
  rbind(
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
    YEAR = as.numeric(lubridate::year(BEG_T)), #create YEAR column 
    FIPS=case_when(
      FIPS %in% c("02275","02195") ~ "02280",
      FIPS=="08014" & YEAR<2002 ~ "08013", #Events in Broomfield prior to 2002 are imputed to Boulder County
      T ~ FIPS
    )
  ) %>% 
  filter(STATE!="ALASKA") %>% 
  filter(STATE!="HAWAII") %>% 
  relocate(EVENT_ID, STATE_CODE, STATE, COUNTY, FIPS, YEAR, EPISODE_ID)

#### Aggregation Needed to be comparable with income data ####

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

saveRDS(data,file="data/clean_hail_step3.rds")
