library(readxl)
library(tidyverse)
library(sf)
library(magrittr)
library(tictoc)
library(tmaptools, warn.conflicts =FALSE)

rm(list = ls())

states <- read_csv("utilities/states.csv")
datastorms <- readRDS(file="data/clean_storm_step3.Rda")
states %<>% 
  rename(STATE_CODE = Abbreviation, STATE=State) %>%
  mutate(STATE=toupper(STATE)) %>% 
  left_join(
    .,
    datastorms %>% 
      mutate(STATE_FIPS = str_sub(FIPS,1,2)) %>% 
      distinct(STATE_FIPS, STATE_CODE),
    by="STATE_CODE"
  ) %>% 
  filter(!is.na(STATE_FIPS))

#### Temperature ####
temp <- read_table("data/climdiv/climdiv-tmpccy-v1.0.0-20230306.txt", 
                   col_names = FALSE)
temp <- temp[,-14]
temp %<>% 
  rename(
    IDOP=X1,
    JAN = X2,
    FEB = X3,
    MAR = X4,
    APR = X5,
    MAY = X6,
    JUN = X7,
    JUL = X8,
    AUG = X9,
    SEP = X10,
    OCT = X11,
    NOV = X12,
    DEC = X13
  )

temp %<>% 
  mutate(
    STATE_CODE_LOC = str_sub(IDOP,1,2),
    COUNTY_FIPS = str_sub(IDOP,3,5),
    YEAR = str_sub(IDOP,8,11)
  ) %>% 
  select(-IDOP) %>% 
  filter(YEAR>1989) %>% 
  mutate(
    STATE = case_when(
      STATE_CODE_LOC=="01" ~ "Alabama",
      STATE_CODE_LOC=="02" ~ "Arizona",
      STATE_CODE_LOC=="03" ~ "Arkansas",
      STATE_CODE_LOC=="04" ~ "California",
      STATE_CODE_LOC=="05" ~ "Colorado",
      STATE_CODE_LOC=="06" ~ "Connecticut",
      STATE_CODE_LOC=="07" ~ "Delaware",
      STATE_CODE_LOC=="08" ~ "Florida",
      STATE_CODE_LOC=="09" ~ "Georgia",
      STATE_CODE_LOC=="10" ~ "Idaho",
      STATE_CODE_LOC=="11" ~ "Illinois",
      STATE_CODE_LOC=="12" ~ "Indiana",
      STATE_CODE_LOC=="13" ~ "Iowa",
      STATE_CODE_LOC=="14" ~ "Kansas",
      STATE_CODE_LOC=="15" ~ "Kentucky",
      STATE_CODE_LOC=="16" ~ "Louisiana",
      STATE_CODE_LOC=="17" ~ "Maine",
      STATE_CODE_LOC=="18" ~ "Maryland",
      STATE_CODE_LOC=="19" ~ "Massachusetts",
      STATE_CODE_LOC=="20" ~ "Michigan",
      STATE_CODE_LOC=="21" ~ "Minnesota",
      STATE_CODE_LOC=="22" ~ "Mississippi",
      STATE_CODE_LOC=="23" ~ "Missouri",
      STATE_CODE_LOC=="24" ~ "Montana",
      STATE_CODE_LOC=="25" ~ "Nebraska",
      STATE_CODE_LOC=="26" ~ "Nevada",
      STATE_CODE_LOC=="27" ~ "New Hampshire",
      STATE_CODE_LOC=="28" ~ "New Jersey",
      STATE_CODE_LOC=="29" ~ "New Mexico",
      STATE_CODE_LOC=="30" ~ "New York",
      STATE_CODE_LOC=="31" ~ "North Carolina",
      STATE_CODE_LOC=="32" ~ "North Dakota",
      STATE_CODE_LOC=="33" ~ "Ohio",
      STATE_CODE_LOC=="34" ~ "Oklahoma",
      STATE_CODE_LOC=="35" ~ "Oregon",
      STATE_CODE_LOC=="36" ~ "Pennsylvania",
      STATE_CODE_LOC=="37" ~ "Rhode Island",
      STATE_CODE_LOC=="38" ~ "South Carolina",
      STATE_CODE_LOC=="39" ~ "South Dakota",
      STATE_CODE_LOC=="40" ~ "Tennessee",
      STATE_CODE_LOC=="41" ~ "Texas",
      STATE_CODE_LOC=="42" ~ "Utah",
      STATE_CODE_LOC=="43" ~ "Vermont",
      STATE_CODE_LOC=="44" ~ "Virginia",
      STATE_CODE_LOC=="45" ~ "Washington",
      STATE_CODE_LOC=="46" ~ "West Virginia",
      STATE_CODE_LOC=="47" ~ "Wisconsin",
      STATE_CODE_LOC=="48" ~ "Wyoming",
      STATE_CODE_LOC=="50" ~ "Alaska",
    )
  ) %>% 
  mutate(STATE=toupper(STATE))

temp %<>% 
  left_join(
    .,
    states,
    by="STATE"
  ) %>% 
  mutate(
    FIPS=paste0(STATE_FIPS, COUNTY_FIPS)
  )

#Remove alaska, hawaii is not present
temp %<>% 
  filter(STATE!="ALASKA")

#Manage temperatures, create one for DC and one for Lexington

temp %>% 
  filter(FIPS%in%c("51163", "24031")) %>% 
  mutate(to_merge=FIPS) -> x

temp %>% 
  filter(!(FIPS%in%c("51163", "24031"))) -> x2

x %>% 
  distinct(FIPS, STATE_CODE, STATE, to_merge) %>% 
  rbind(
    .,
    tibble(
      STATE = c("DISTRICT OF COLUMBIA", "VIRGINIA"),
      STATE_CODE = c("DC", "VA"),
      FIPS = c("11001", "51678"),
      to_merge = c("24031", "51163")
    )
  ) -> x3

left_join(
  x3,
  x,
  by="to_merge"
) %>%
  select(-to_merge, -FIPS.y, -STATE_CODE.y, -STATE.y) %>% 
  rename(FIPS = FIPS.x, STATE_CODE = STATE_CODE.x, STATE=STATE.x) -> x4

temp <- rbind(x4,x2)

#County 24511 unknown

temp %<>% filter(FIPS!="24511")

#### Precipitations ####

prec <- read_table("data/climdiv/climdiv-pcpncy-v1.0.0-20230306.txt", 
                   col_names = FALSE)

prec <- prec[,-14]
prec %<>% 
  rename(
    IDOP=X1,
    JAN = X2,
    FEB = X3,
    MAR = X4,
    APR = X5,
    MAY = X6,
    JUN = X7,
    JUL = X8,
    AUG = X9,
    SEP = X10,
    OCT = X11,
    NOV = X12,
    DEC = X13
  )

prec %<>% 
  mutate(
    STATE_CODE_LOC = str_sub(IDOP,1,2),
    COUNTY_FIPS = str_sub(IDOP,3,5),
    YEAR = str_sub(IDOP,8,11)
  ) %>% 
  select(-IDOP) %>% 
  filter(YEAR>1989) %>% 
  mutate(
    STATE = case_when(
      STATE_CODE_LOC=="01" ~ "Alabama",
      STATE_CODE_LOC=="02" ~ "Arizona",
      STATE_CODE_LOC=="03" ~ "Arkansas",
      STATE_CODE_LOC=="04" ~ "California",
      STATE_CODE_LOC=="05" ~ "Colorado",
      STATE_CODE_LOC=="06" ~ "Connecticut",
      STATE_CODE_LOC=="07" ~ "Delaware",
      STATE_CODE_LOC=="08" ~ "Florida",
      STATE_CODE_LOC=="09" ~ "Georgia",
      STATE_CODE_LOC=="10" ~ "Idaho",
      STATE_CODE_LOC=="11" ~ "Illinois",
      STATE_CODE_LOC=="12" ~ "Indiana",
      STATE_CODE_LOC=="13" ~ "Iowa",
      STATE_CODE_LOC=="14" ~ "Kansas",
      STATE_CODE_LOC=="15" ~ "Kentucky",
      STATE_CODE_LOC=="16" ~ "Louisiana",
      STATE_CODE_LOC=="17" ~ "Maine",
      STATE_CODE_LOC=="18" ~ "Maryland",
      STATE_CODE_LOC=="19" ~ "Massachusetts",
      STATE_CODE_LOC=="20" ~ "Michigan",
      STATE_CODE_LOC=="21" ~ "Minnesota",
      STATE_CODE_LOC=="22" ~ "Mississippi",
      STATE_CODE_LOC=="23" ~ "Missouri",
      STATE_CODE_LOC=="24" ~ "Montana",
      STATE_CODE_LOC=="25" ~ "Nebraska",
      STATE_CODE_LOC=="26" ~ "Nevada",
      STATE_CODE_LOC=="27" ~ "New Hampshire",
      STATE_CODE_LOC=="28" ~ "New Jersey",
      STATE_CODE_LOC=="29" ~ "New Mexico",
      STATE_CODE_LOC=="30" ~ "New York",
      STATE_CODE_LOC=="31" ~ "North Carolina",
      STATE_CODE_LOC=="32" ~ "North Dakota",
      STATE_CODE_LOC=="33" ~ "Ohio",
      STATE_CODE_LOC=="34" ~ "Oklahoma",
      STATE_CODE_LOC=="35" ~ "Oregon",
      STATE_CODE_LOC=="36" ~ "Pennsylvania",
      STATE_CODE_LOC=="37" ~ "Rhode Island",
      STATE_CODE_LOC=="38" ~ "South Carolina",
      STATE_CODE_LOC=="39" ~ "South Dakota",
      STATE_CODE_LOC=="40" ~ "Tennessee",
      STATE_CODE_LOC=="41" ~ "Texas",
      STATE_CODE_LOC=="42" ~ "Utah",
      STATE_CODE_LOC=="43" ~ "Vermont",
      STATE_CODE_LOC=="44" ~ "Virginia",
      STATE_CODE_LOC=="45" ~ "Washington",
      STATE_CODE_LOC=="46" ~ "West Virginia",
      STATE_CODE_LOC=="47" ~ "Wisconsin",
      STATE_CODE_LOC=="48" ~ "Wyoming",
      STATE_CODE_LOC=="50" ~ "Alaska",
    )
  ) %>% 
  mutate(STATE=toupper(STATE))

prec %<>% 
  left_join(
    .,
    states,
    by="STATE"
  ) %>% 
  mutate(
    FIPS=paste0(STATE_FIPS, COUNTY_FIPS)
  )

#Remove alaska, hawaii is not present
prec %<>% 
  filter(STATE!="ALASKA")

prec %>% 
  filter(FIPS%in%c("51163", "24031")) %>% 
  mutate(to_merge=FIPS) -> x

prec %>% 
  filter(!(FIPS%in%c("51163", "24031"))) -> x2

x %>% 
  distinct(FIPS, STATE_CODE, STATE, to_merge) %>% 
  rbind(
    .,
    tibble(
      STATE = c("DISTRICT OF COLUMBIA", "VIRGINIA"),
      STATE_CODE = c("DC", "VA"),
      FIPS = c("11001", "51678"),
      to_merge = c("24031", "51163")
    )
  ) -> x3

left_join(
  x3,
  x,
  by="to_merge"
) %>%
  select(-to_merge, -FIPS.y, -STATE_CODE.y, -STATE.y) %>% 
  rename(FIPS = FIPS.x, STATE_CODE = STATE_CODE.x, STATE=STATE.x) -> x4

prec <- rbind(x4,x2)

#County 24511 unknown

prec %<>% filter(FIPS!="24511")

#### Put toghether ####

temp %<>% 
  select(STATE, STATE_CODE, FIPS, YEAR, JAN:DEC) %>% 
  pivot_longer(JAN:DEC) %>% 
  group_by(FIPS, YEAR) %>% 
  summarise(
    STATE = STATE[1],
    FIPS = FIPS[1],
    AVG_TEMP = mean(value)
  )

prec %<>% 
  select(STATE, STATE_CODE, FIPS, YEAR, JAN:DEC) %>% 
  pivot_longer(JAN:DEC) %>% 
  group_by(FIPS, YEAR) %>% 
  summarise(
    STATE = STATE[1],
    FIPS = FIPS[1],
    TOT_PREC = mean(value)
  )

temp %>% 
  left_join(
    .,
    prec %>% select(-STATE),
    by=c("FIPS", "YEAR")
  ) %>% 
  ungroup()-> data


#### Aggregation Needed to be comparable with Income data ####

data %<>% 
  mutate(
    FIPS = case_when(
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
      FIPS%in%c("51153","51683","51685") ~ "51942",
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

data %<>% 
  group_by(FIPS,YEAR) %>% 
  summarise(
    STATE=STATE[1],
    AVG_TEMP = mean(AVG_TEMP),
    TOT_PREC = sum(TOT_PREC)
  ) %>% ungroup()

saveRDS(data, file="data/cleaned_temp_prec.rds")

