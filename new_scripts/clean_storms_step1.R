library(tidyverse)
library(sf)
library(magrittr)
library(tictoc)

rm(list = ls())

StormEvents1990 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d1990_c20220425.csv")
StormEvents1991 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d1991_c20220425.csv")
StormEvents1992 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d1992_c20220425.csv")
StormEvents1993 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d1993_c20220425.csv")
StormEvents1994 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d1994_c20220425.csv")
StormEvents1995 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d1995_c20220425.csv")
StormEvents1996 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d1996_c20220425.csv")
StormEvents1997 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d1997_c20220425.csv")
StormEvents1998 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d1998_c20220425.csv")
StormEvents1999 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d1999_c20220425.csv")
StormEvents2000 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2000_c20220425.csv")
StormEvents2001 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2001_c20220425.csv")
StormEvents2002 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2002_c20220425.csv")
StormEvents2003 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2003_c20220425.csv")
StormEvents2004 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2004_c20220425.csv")
StormEvents2005 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2005_c20220425.csv")
StormEvents2006 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2006_c20220425.csv")
StormEvents2007 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2007_c20220425.csv")
StormEvents2008 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2008_c20220425.csv")
StormEvents2009 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2009_c20220425.csv")
StormEvents2010 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2010_c20220425.csv")
StormEvents2011 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2011_c20220425.csv")
StormEvents2012 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2012_c20221216.csv")
StormEvents2013 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2013_c20230118.csv")
StormEvents2014 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2014_c20230330.csv")
StormEvents2015 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2015_c20220425.csv")
StormEvents2016 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2016_c20220719.csv")
StormEvents2017 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2017_c20230317.csv")
StormEvents2018 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2018_c20230118.csv")
StormEvents2019 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2019_c20230118.csv")
StormEvents2020 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2020_c20230118.csv")
StormEvents2021 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2021_c20230317.csv")
StormEvents2022 <- read_csv("data/details/StormEvents_details-ftp_v1.0_d2022_c20230317.csv")


# Bind togheter
data <- do.call(rbind, lapply( paste0("StormEvents", 1990:2022) , get) )

data %>% 
  filter(YEAR>1990) %>% 
  filter(YEAR<2020) -> a

table(a$EVENT_TYPE) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  mutate(Var1 = paste0(toupper(str_sub(Var1,1,1)), tolower(str_sub(Var1,2,-1))))-> a

cbind(
  a[1:35,],a[36:70,]
) %>% 
  `colnames<-`(c("Event type", "Frequency", "Event type", "Frequency")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  writeLines("tables/sed_frequency_table.tex")

data %<>% 
  filter(EVENT_TYPE %in% c("Thunderstorm Wind")) %>% 
  filter(!is.na(MAGNITUDE)) %>% 
  filter(MAGNITUDE>46)

saveRDS(data,file="data/clean_storm_step1.rds")
