library(readxl)
library(tidyverse)
library(sf)
library(magrittr)
library(tictoc)
library(tmaptools, warn.conflicts =FALSE)

rm(list = ls())

#https://www.bls.gov/cew/downloadable-data-files.htm
to_load <- paste0(
  "data/QCEW/",
  as.character(1990:2021),
  "_all_county_high_level/allhlcn",
  str_sub(as.character(1990:2021),-2,-1),
  ".xlsx"
)

list <- list()
for (i in 1:length(to_load)) {
  list[[i]]<- read_excel(to_load[i])
}

data <- data.table::rbindlist(list)

data %<>% 
  filter(`Area Type`=="County") %>% 
  select(-`Area Type`)

#Fix Industry post-2016
data %<>% 
  mutate(
    Industry = case_when(
      Industry == "10 Total, all industries" ~ "Total, all industries",
      Industry == "101 Goods-producing" ~ "Goods-producing",
      Industry == "1011 Natural resources and mining" ~ "Natural resources and mining",
      Industry == "1012 Construction" ~ "Construction",
      Industry == "1013 Manufacturing" ~ "Manufacturing",
      Industry == "102 Service-providing" ~ "Service-providing",
      Industry == "1021 Trade, transportation, and utilities" ~ "Trade, transportation, and utilities",
      Industry == "1022 Information" ~ "Information",
      Industry == "1023 Financial activities" ~ "Financial activities",
      Industry == "1024 Professional and business services" ~ "Professional and business services",
      Industry == "1025 Education and health services" ~ "Education and health services",
      Industry == "1026 Leisure and hospitality" ~ "Leisure and hospitality",
      Industry == "1027 Other services" ~ "Other services",
      Industry == "1029 Unclassified" ~ "Unclassified",
      T ~ Industry
    )
  )

data %<>% 
  as_tibble() %>% 
  select(-Own, -NAICS, -Qtr,
         -`Total Wage Location Quotient Relative to U.S.`, -`Employment Location Quotient Relative to U.S.`,
         -`Annual Average Establishment Count`, -`Annual Average Status Code`
  ) %>% 
  rename(
    "FIPS"="Area\r\nCode",
    "STATE_FIPS"="St",
    "COUNTY_FIPS"="Cnty",
    "YEAR"="Year",
    "STATE"="St Name",
    "AREA"="Area",
    "AAE"="Annual Average Employment",
    "ATW"="Annual Total Wages",
    "AAWW"="Annual Average Weekly Wage",
    "AAP"="Annual Average Pay"
  ) %>% 
  mutate(
    Ownership = case_when(
      Ownership=="Total Covered" ~ "TOT",
      Ownership=="Federal Government" ~ "FEDGOV",
      Ownership=="Local Government" ~ "LOCGOV",
      Ownership=="State Government" ~ "STATEGOV",
      Ownership=="Private" ~ "PRIV"
    ),
    Industry = case_when(
      Industry=="Total, all industries" ~ "TOT",
      Industry=="Goods-producing" ~ "GOODPROD",
      Industry=="Natural resources and mining" ~ "NATMIN",
      Industry=="Construction" ~ "CONSTR",
      Industry=="Manufacturing" ~ "MANUF",
      Industry=="Service-providing" ~ "SERV",
      Industry=="Trade, transportation, and utilities" ~ "TRADETRANS",
      Industry=="Information" ~ "INFO",
      Industry=="Financial activities" ~ "FIN",
      Industry=="Professional and business services" ~ "PROBUS",
      Industry=="Education and health services" ~ "EDUHEALTH",
      Industry=="Leisure and hospitality" ~ "LEISHOSP",
      Industry=="Other services" ~ "OTHSERV",
      Industry=="Unclassified" ~ "UNCL"
    )
  )

data %<>% 
  filter(COUNTY_FIPS!="999") %>% 
  filter(!(FIPS%in%c("51515","51560"))) %>% 
  filter(STATE!="Alaska") %>% 
  filter(STATE!="Hawaii")


data$AAE[data$FIPS=="46102" & data$YEAR=="2015"] <- data$AAE[data$FIPS=="46102" & data$YEAR=="2015"] + data$AAE[data$FIPS=="46113" & data$YEAR=="2015"]
data$ATW[data$FIPS=="46102" & data$YEAR=="2015"] <- data$ATW[data$FIPS=="46102" & data$YEAR=="2015"] + data$ATW[data$FIPS=="46113" & data$YEAR=="2015"]
data$AAP[data$FIPS=="46102" & data$YEAR=="2015"] <- apply(cbind(data$AAP[data$FIPS=="46102" & data$YEAR=="2015"], data$AAP[data$FIPS=="46113" & data$YEAR=="2015"]),1,mean)  
data$AAWW[data$FIPS=="46102" & data$YEAR=="2015"] <- apply(cbind(data$AAWW[data$FIPS=="46102" & data$YEAR=="2015"], data$AAWW[data$FIPS=="46113" & data$YEAR=="2015"]),1,mean)  
data %<>% filter(!(FIPS=="46113" & YEAR=="2015"))

#rename fips accordingly
data %<>% 
  mutate(
    FIPS = case_when(
      FIPS=="46113" ~ "46102",
      T ~ FIPS
    )
  )

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
  pivot_wider(names_from= c(Ownership, Industry), values_from = c("AAE", "ATW", "AAWW", "AAP" ))

data %<>% 
  group_by(FIPS,YEAR) %>% 
  summarise(
    STATE_FIPS = unique(STATE_FIPS),
    STATE = unique(STATE),
    AREA = AREA[1], #only choose the first name, it does not matter, merged are operated through FIPS
    across(starts_with("AAE") | starts_with("ATW"), sum),
    across(starts_with("AAP") | starts_with("AAWW"), sum)
  ) %>% ungroup()


#### Check comparability ####
datastorms <- readRDS(file="data/clean_storm_step3.Rda")

setdiff(
  data$FIPS, 
  datastorms$FIPS_AG
) # empty!

setdiff(
  datastorms$FIPS_AG,
  data$FIPS
) #empty!

#### Deflate ####

PCE_DEFLATOR <- read_csv("utilities/PCE_deflator.csv")

PCE_DEFLATOR %<>% 
  mutate(
    DATE=str_sub(DATE,1,4)
  ) %>% 
  rename(PCE_DEFLATOR = DPCERD3A086NBEA)

data %<>% 
  left_join(
    .,
    PCE_DEFLATOR,
    by=c("YEAR"="DATE")
  )

data %<>% 
  mutate(PCE_DEFLATOR = PCE_DEFLATOR/100) %>% 
  mutate(across(starts_with("ATW")|starts_with("AAWW")|starts_with("AAP"), ~ .x/PCE_DEFLATOR))


#Fill eight 0 values through linear interpolation. NAs at the begin or at the end of the series are maintaned

data %<>% 
  arrange(FIPS, YEAR) %>% 
  mutate(
    across(
      AAE_TOT_TOT:AAWW_PRIV_UNCL,
      ~ ifelse(.x==0, as.numeric(NA), .x)
    )
  ) %>% 
  group_by(FIPS) %>% 
  mutate(
    across(
      AAE_TOT_TOT:AAWW_PRIV_UNCL,
      ~ zoo::na.approx(.x, na.rm=F)
    )
  )

data %<>% 
  arrange(FIPS, YEAR) %>% 
  group_by(FIPS) %>% 
  mutate(
    across(starts_with("AAE")|starts_with("ATW")|starts_with("AAWW")|starts_with("AAP"), ~ ((.x/lag(.x))-1)*100, .names = "GR_{.col}"
    )
  ) %>% ungroup()

saveRDS(data, file="data/wages_employment.rds")
#Table for variables 

#FIRST TERM

#"AAE"="Annual Average Employment",
#"ATW"="Annual Total Wages",
#"AAWW"="Annual Average Weekly Wage",
#"AAP"="Annual Average Pay"

#SECOND TERM
#"Total Covered" ~ "TOT",
#"Federal Government" ~ "FEDGOV",
#"Local Government" ~ "LOCGOV",
#"State Government" ~ "STATEGOV",
#"Private" ~ "PRIV"

#THIRD TERM

#"Total, all industries" ~ "TOT",
#"Goods-producing" ~ "GOODPROD",
#"Natural resources and mining" ~ "NATMIN",
#"Construction" ~ "CONSTR",
#"Manufacturing" ~ "MANUF",
#"Service-providing" ~ "SERV",
#"Trade, transportation, and utilities" ~ "TRADETRANS",
#"Information" ~ "INFO",
#"Financial activities" ~ "FIN",
#"Professional and business services" ~ "PROBUS",
#"Education and health services" ~ "EDUHEALTH",
#"Leisure and hospitality" ~ "LEISHOSP",
#"Other services" ~ "OTHSERV",
#"Unclassified" ~ "UNCL"

