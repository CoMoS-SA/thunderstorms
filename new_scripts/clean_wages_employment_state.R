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
  filter(`Area Type`=="State") %>% 
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
    "STATE_CODE"="St",
    "COUNTY_CODE"="Cnty",
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
  filter(STATE!="Alaska") %>% 
  filter(STATE!="Hawaii")

data %<>% 
  pivot_wider(names_from= c(Ownership, Industry), values_from = c("AAE", "ATW", "AAWW", "AAP" ))

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

#Fill 0 values through linear interpolation. NAs at the begin or at the end of the series are maintaned

data %<>% 
  arrange(FIPS, YEAR) %>% 
  mutate(
    across(
      AAE_TOT_TOT:AAP_PRIV_UNCL,
      ~ ifelse(.x==0, as.numeric(NA), .x)
    )
  ) %>% 
  group_by(FIPS) %>% 
  mutate(
    across(
      AAE_TOT_TOT:AAP_PRIV_UNCL,
      ~ zoo::na.approx(.x, na.rm=F)
    )
  )

#Compute growth rates 
data %<>% 
  arrange(STATE, YEAR) %>% 
  group_by(STATE) %>% 
  mutate(
    across(starts_with("AAE")|starts_with("ATW")|starts_with("AAWW")|starts_with("AAP"), ~ ((.x/lag(.x))-1)*100, .names = "GR_{.col}"
    )
  ) %>% ungroup() %>% 
  mutate(STATE=toupper(STATE))

saveRDS(data, file="data/wages_employment_state.rds")
