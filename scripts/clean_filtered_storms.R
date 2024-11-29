library(tidyverse)
library(sf)
library(magrittr)
library(tictoc)

filtered_storms <- readRDS(file="data/filtered_storms.rds")

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

filtered_storms %<>% 
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


#remove 27 observation 
filtered_storms %<>% filter(CZ_TYPE=="C")

filtered_storms %>% 
  distinct(FIPS_MIX) %>% pull() -> fips_c

filtered_storms %<>% 
  mutate(
    FIPS_MIX = case_when(
      FIPS_MIX == "02222" ~ "02090",
      T ~ FIPS_MIX
    )
  )

setdiff(filtered_storms$FIPS_MIX, county_shape$FIPS)
setdiff(county_shape$FIPS, filtered_storms$FIPS_MIX)

filtered_storms %>% 
  filter(FIPS_MIX %in% county_shape$FIPS) -> data_c_no_prob

filtered_storms %>% 
  filter(!(FIPS_MIX %in% county_shape$FIPS)) -> data_c_prob

data_c_prob %<>% 
  filter(!(CZ_NAME=="LOWER KOYUKUK MIDDLE YKN VLYS")) %>%  #in alaska
  filter(!(CZ_NAME=="TXZ513")) #non existing code

tibble(
  EVENT_ID = c(10319985,10355938),
  FIPS = list(
    c("12103")
)) -> fix

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


filtered_storms <- c_data_fixed

#manipulate times and count by episode
filtered_storms %<>% 
  mutate(
    BEG_T = as.POSIXct(BEGIN_DATE_TIME, format="%d-%b-%y %H:%M:%S"),
    END_T = as.POSIXct(END_DATE_TIME, format="%d-%b-%y %H:%M:%S")
  )

#Some events happen when time shift is kicking in. We need to assign the entire episode to the next day
filtered_storms %>% 
  filter(is.na(BEG_T)) %>% pull(EVENT_ID) -> prob_ev

filtered_storms %>% 
  filter(is.na(END_T)) %>% pull(EVENT_ID) -> prob_ev_e

filtered_storms %>% 
  filter(is.na(BEG_T)) %>% pull(EPISODE_ID) %>% unique() -> prob_ep

filtered_storms %>% 
  filter(is.na(END_T)) %>% pull(EPISODE_ID) %>% unique() -> prob_ep_e


filtered_storms %>% 
  filter(EVENT_ID%in%prob_ev) %>% 
  select(BEGIN_DATE_TIME)

#they are all in 2020 or 2021, filter out

filtered_storms %<>% 
  filter(!(EVENT_ID%in%prob_ev))


filtered_storms %<>% 
  group_by(EPISODE_ID) %>% 
  mutate(
    n=ifelse(is.na(EPISODE_ID), 1, n())
  ) %>% 
  ungroup()


filtered_storms %>% 
  filter(n>1) -> to_group

filtered_storms %>% 
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


filtered_storms <- rbind(
  to_group ,
  not_to_group %>% 
    select(
      BEG_T,END_T,MAGNITUDE, MAGNITUDE_TYPE,EVENT_TYPE,EVENT_ID,EPISODE_ID,BEGIN_LON,END_LON,BEGIN_LAT,END_LAT,
      STATE_CODE, STATE, FIPS
    )
)

filtered_storms %<>% 
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
filtered_storms %<>% 
  left_join(
    .,
    county_shape %>% 
      select(COUNTYNAME, FIPS) %>% 
      rename(COUNTY = COUNTYNAME),
    by="FIPS"
  )

filtered_storms %<>% 
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

filtered_storms %<>% 
  left_join(
    .,
    county_area %>% 
      rename(COUNTY_AREA = LND110210D) %>% 
      select(COUNTY_AREA, STCOU), 
    by = c("FIPS"="STCOU")
  )



####  Final adjustments ####

filtered_storms %<>% 
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


#### Aggregation Needed to be comparable with Income data ####

filtered_storms %<>% 
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

saveRDS(filtered_storms,file="data/clean_filtered_storms.rds")

