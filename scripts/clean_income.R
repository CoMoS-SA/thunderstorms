library(readxl)
library(tidyverse)
library(sf)
library(magrittr)
library(tictoc)
library(tmaptools, warn.conflicts =FALSE)

rm(list = ls())

# https://apps.bea.gov/regional/downloadzip.cfm
data <- read_csv("data/CAINC1/CAINC1__ALL_AREAS_1969_2021.csv")

data[data$GeoFIPS=="35013",]$GeoName <- "DONA ANA, NM"

data %<>% 
  select(-Region, -LineCode, -TableName, -IndustryClassification, -Unit)

data %<>% 
  filter(!is.na(GeoName)) %>% 
  filter(str_sub(GeoFIPS,-3,-1)!="000")

#remove Alaska and Hawaii
data %<>% 
  filter(!(startsWith(GeoFIPS, "02")|startsWith(GeoFIPS, "15")))


data %<>% 
  pivot_longer(`1969`:`2021`) %>% 
  mutate(
    Description = case_when(
      Description=="Personal income (thousands of dollars)" ~ "INCOME",
      Description=="Population (persons) 1/" ~ "POPULATION",
      Description=="Per capita personal income (dollars) 2/" ~ "INCOME_PC"
    )
  ) %>% 
  pivot_wider(names_from= Description, values_from = value) %>% 
  rename(
    FIPS=GeoFIPS,
    YEAR = name
  ) %>% 
  mutate(
    INCOME = as.numeric(INCOME), #it is expressed in thousand of dollars
    POPULATION = as.numeric(POPULATION),
    INCOME_PC = as.numeric(INCOME_PC),
    YEAR = as.numeric(YEAR)
  ) %>% 
  filter(YEAR>1989)

data %<>% 
  filter(!is.na(INCOME))
  
#### Deflation ####
#https://fred.stlouisfed.org/series/DPCERD3A086NBEA PCE deflator

PCE_DEFLATOR <- read_csv("utilities/PCE_deflator.csv")

PCE_DEFLATOR %<>% 
  mutate(
    DATE=str_sub(DATE,1,4)
  ) %>% 
  rename(PCE_DEFLATOR = DPCERD3A086NBEA)

data %<>% 
  mutate(YEAR=as.character(YEAR)) %>% 
  left_join(
    .,
    PCE_DEFLATOR,
    by=c("YEAR"="DATE")
  ) %>% 
  mutate(YEAR=as.numeric(YEAR))

data %<>% 
  mutate(PCE_DEFLATOR = PCE_DEFLATOR/100) %>% 
  mutate(across(c(INCOME, INCOME_PC), ~ .x/PCE_DEFLATOR))

data %<>% 
  arrange(FIPS, YEAR) %>% 
  group_by(FIPS) %>% 
  mutate(
    across(c(INCOME, POPULATION, INCOME_PC), ~ ((.x/lag(.x))-1)*100, .names = "GR_{.col}"
    )
  )

saveRDS(data, file="data/income.rds")