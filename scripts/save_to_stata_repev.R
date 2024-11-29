library(tidyverse)
library(magrittr)
library(stargazer)
library(ggplot2)
library(grDevices)
library(knitr)
library(kableExtra)
library(fixest)
library(readxl)

rm(list = ls())

source("scripts/functions_f.R")
data <- readRDS(file="data/f_panel_tr.rds")

data %>% 
  as_tibble() %>% 
  mutate(yr = as.numeric(as.character(Year))) -> dt

# Pooled

dt %>%
  filter(
    S4 >= mean(S4[S4>0])
  ) %>% 
  select(yr, FIPS) -> aux

bot <- quantile(dt$S4[dt$S4>0], 0.25)

out <- list()
for(i in 1:nrow(aux)){
  dt %>% 
    as_tibble() %>% 
    filter(FIPS==aux$FIPS[i]) %>% 
    filter(Year%in%c((aux$yr[i]-3):(aux$yr[i]+7))) %>% 
    filter(S4<bot) %>% 
    pull(yr) -> out[[i]]
  print(i)
}

to_sel <- aux$FIPS[which(map_dbl(out, length)>=5)] 

perc_p <- length(unique(to_sel))/length(unique(dt$FIPS))

data %>% 
  filter(FIPS %in% to_sel) -> dt_p


# Within mean 

dt %>% 
  group_by(FIPS) %>%
  mutate(
    #mn = median(S4[S4>0]),
    mn = mean(S4[S4>0]),
    bot = quantile(S4[S4>0], 0.25)
  ) %>% 
  select(yr, FIPS, mn, bot, S4) %>% 
  filter(S4>=mn) -> aux


out <- list()
for(i in 1:nrow(aux)){
  dt %>% 
    filter(FIPS==aux$FIPS[i]) %>% 
    filter(yr%in%c((aux$yr[i]-3):(aux$yr[i]+7))) %>% 
    filter(S4<aux$bot[i]) %>% 
    pull(yr) -> out[[i]]
  print(i)
}

to_sel <- aux$FIPS[which(map_dbl(out, length)>=5)] 

perc_wm <- length(unique(to_sel))/length(unique(dt$FIPS))

data %>% 
  filter(FIPS %in% to_sel) -> dt_wm


# Within mean non overlapping

dt %>% 
  group_by(FIPS) %>%
  mutate(
    #mn = median(S4[S4>0]),
    mn = mean(S4[S4>0]),
    bot = quantile(S4[S4>0], 0.25)
  ) %>% 
  select(yr, FIPS, mn, bot, S4) %>% 
  filter(S4>=mn) -> aux


out <- list()
for(i in 1:nrow(aux)){
  dt %>% 
    filter(FIPS==aux$FIPS[i]) %>% 
    filter(yr%in%c((aux$yr[i]-3):(aux$yr[i]+7))) %>% 
    filter(S4<aux$bot[i]) %>% 
    pull(yr) -> out[[i]]
  print(i)
}

aux[map_dbl(out, length)>=5, ] %>% 
  group_by(FIPS) %>% 
  summarise(n=n()) %>% 
  filter(n==1) %>% 
  pull(FIPS) -> to_sel

perc_wmo <- length(unique(to_sel))/length(unique(dt$FIPS))

data %>% 
  filter(FIPS %in% to_sel) -> dt_wmo


# Within top quartile 

dt %>% 
  group_by(FIPS) %>%
  mutate(
    #mn = median(S4[S4>0]),
    mn = quantile(S4[S4>0], 0.75),
    bot = quantile(S4[S4>0], 0.25)
  ) %>% 
  select(yr, FIPS, mn, bot, S4) %>% 
  filter(S4>=mn) -> aux


out <- list()
for(i in 1:nrow(aux)){
  dt %>% 
    filter(FIPS==aux$FIPS[i]) %>% 
    filter(yr%in%c((aux$yr[i]-3):(aux$yr[i]+7))) %>% 
    filter(S4<aux$bot[i]) %>% 
    pull(yr) -> out[[i]]
  print(i)
}

to_sel <- aux$FIPS[which(map_dbl(out, length)>=5)] 

perc_wt <- length(unique(to_sel))/length(unique(dt$FIPS))

data %>% 
  filter(FIPS %in% to_sel) -> dt_wt

# Prepare data

dt_p %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )

dt_wm %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )

dt_wmo %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )

dt_wt %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )


#### dt_p ####

dt_p %<>% 
  mutate(
    state_trend_ALABAMA = ifelse(STATE=="ALABAMA", time, 0),
    state_trend_ARIZONA = ifelse(STATE=="ARIZONA", time, 0),
    state_trend_ARKANSAS = ifelse(STATE=="ARKANSAS", time, 0),
    state_trend_CALIFORNIA = ifelse(STATE=="CALIFORNIA", time, 0),
    state_trend_COLORADO = ifelse(STATE=="COLORADO", time, 0),
    state_trend_CONNECTICUT = ifelse(STATE=="CONNECTICUT", time, 0),
    state_trend_DELAWARE = ifelse(STATE=="DELAWARE", time, 0),
    state_trend_FLORIDA = ifelse(STATE=="FLORIDA", time, 0),
    state_trend_GEORGIA = ifelse(STATE=="GEORGIA", time, 0),
    state_trend_IDAHO = ifelse(STATE=="IDAHO", time, 0),
    state_trend_ILLINOIS = ifelse(STATE=="ILLINOIS", time, 0),
    state_trend_INDIANA = ifelse(STATE=="INDIANA", time, 0),
    state_trend_IOWA = ifelse(STATE=="IOWA", time, 0),
    state_trend_KANSAS = ifelse(STATE=="KANSAS", time, 0),
    state_trend_KENTUCKY = ifelse(STATE=="KENTUCKY", time, 0),
    state_trend_LOUISIANA = ifelse(STATE=="LOUISIANA", time, 0),
    state_trend_MAINE = ifelse(STATE=="MAINE", time, 0),
    state_trend_MARYLAND = ifelse(STATE=="MARYLAND", time, 0),
    state_trend_MASSACHUSETTS = ifelse(STATE=="MASSACHUSETTS", time, 0),
    state_trend_MICHIGAN = ifelse(STATE=="MICHIGAN", time, 0),
    state_trend_MINNESOTA = ifelse(STATE=="MINNESOTA", time, 0),
    state_trend_MISSISSIPPI = ifelse(STATE=="MISSISSIPPI", time, 0),
    state_trend_MISSOURI = ifelse(STATE=="MISSOURI", time, 0),
    state_trend_MONTANA = ifelse(STATE=="MONTANA", time, 0),
    state_trend_NEBRASKA = ifelse(STATE=="NEBRASKA", time, 0),
    state_trend_NEVADA = ifelse(STATE=="NEVADA", time, 0),
    state_trend_NEW_HAMPSHIRE = ifelse(STATE=="NEW HAMPSHIRE", time, 0),
    state_trend_NEW_JERSEY = ifelse(STATE=="NEW JERSEY", time, 0),
    state_trend_NEW_MEXICO = ifelse(STATE=="NEW MEXICO", time, 0),
    state_trend_NEW_YORK = ifelse(STATE=="NEW YORK", time, 0),
    state_trend_NORTH_CAROLINA = ifelse(STATE=="NORTH CAROLINA", time, 0),
    state_trend_NORTH_DAKOTA = ifelse(STATE=="NORTH DAKOTA", time, 0),
    state_trend_OHIO = ifelse(STATE=="OHIO", time, 0),
    state_trend_OKLAHOMA = ifelse(STATE=="OKLAHOMA", time, 0),
    state_trend_OREGON = ifelse(STATE=="OREGON", time, 0),
    state_trend_PENNSYLVANIA = ifelse(STATE=="PENNSYLVANIA", time, 0),
    state_trend_RHODE_ISLAND = ifelse(STATE=="RHODE ISLAND", time, 0),
    state_trend_SOUTH_CAROLINA = ifelse(STATE=="SOUTH CAROLINA", time, 0),
    state_trend_SOUTH_DAKOTA = ifelse(STATE=="SOUTH DAKOTA", time, 0),
    state_trend_TENNESSEE = ifelse(STATE=="TENNESSEE", time, 0),
    state_trend_TEXAS = ifelse(STATE=="TEXAS", time, 0),
    state_trend_UTAH = ifelse(STATE=="UTAH", time, 0),
    state_trend_VERMONT = ifelse(STATE=="VERMONT", time, 0),
    state_trend_VIRGINIA = ifelse(STATE=="VIRGINIA", time, 0),
    state_trend_WASHINGTON = ifelse(STATE=="WASHINGTON", time, 0),
    state_trend_WEST_VIRGINIA = ifelse(STATE=="WEST VIRGINIA", time, 0),
    state_trend_WISCONSIN = ifelse(STATE=="WISCONSIN", time, 0),
    state_trend_WYOMING = ifelse(STATE=="WYOMING", time, 0)
  ) 

dt_p %<>% 
  mutate(
    lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(S4,1)),
    lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(S4,2)),
    lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(S4,3)),
    lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(S4,4)),
    lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(S4,5)),
    lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(S4,6)),
    lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(S4,7)),
    lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(S4,8)),
    lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(S4,9)),
    lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(S4,10))
  ) 

write.csv(dt_p, "data/panel_re_dt_p.csv", row.names = FALSE)
haven::write_dta(dt_p, "stata_rob/panel_re_dt_p.dta")


#### dt_wm ####

dt_wm %<>% 
  mutate(
    state_trend_ALABAMA = ifelse(STATE=="ALABAMA", time, 0),
    state_trend_ARIZONA = ifelse(STATE=="ARIZONA", time, 0),
    state_trend_ARKANSAS = ifelse(STATE=="ARKANSAS", time, 0),
    state_trend_CALIFORNIA = ifelse(STATE=="CALIFORNIA", time, 0),
    state_trend_COLORADO = ifelse(STATE=="COLORADO", time, 0),
    state_trend_CONNECTICUT = ifelse(STATE=="CONNECTICUT", time, 0),
    state_trend_DELAWARE = ifelse(STATE=="DELAWARE", time, 0),
    state_trend_FLORIDA = ifelse(STATE=="FLORIDA", time, 0),
    state_trend_GEORGIA = ifelse(STATE=="GEORGIA", time, 0),
    state_trend_IDAHO = ifelse(STATE=="IDAHO", time, 0),
    state_trend_ILLINOIS = ifelse(STATE=="ILLINOIS", time, 0),
    state_trend_INDIANA = ifelse(STATE=="INDIANA", time, 0),
    state_trend_IOWA = ifelse(STATE=="IOWA", time, 0),
    state_trend_KANSAS = ifelse(STATE=="KANSAS", time, 0),
    state_trend_KENTUCKY = ifelse(STATE=="KENTUCKY", time, 0),
    state_trend_LOUISIANA = ifelse(STATE=="LOUISIANA", time, 0),
    state_trend_MAINE = ifelse(STATE=="MAINE", time, 0),
    state_trend_MARYLAND = ifelse(STATE=="MARYLAND", time, 0),
    state_trend_MASSACHUSETTS = ifelse(STATE=="MASSACHUSETTS", time, 0),
    state_trend_MICHIGAN = ifelse(STATE=="MICHIGAN", time, 0),
    state_trend_MINNESOTA = ifelse(STATE=="MINNESOTA", time, 0),
    state_trend_MISSISSIPPI = ifelse(STATE=="MISSISSIPPI", time, 0),
    state_trend_MISSOURI = ifelse(STATE=="MISSOURI", time, 0),
    state_trend_MONTANA = ifelse(STATE=="MONTANA", time, 0),
    state_trend_NEBRASKA = ifelse(STATE=="NEBRASKA", time, 0),
    state_trend_NEVADA = ifelse(STATE=="NEVADA", time, 0),
    state_trend_NEW_HAMPSHIRE = ifelse(STATE=="NEW HAMPSHIRE", time, 0),
    state_trend_NEW_JERSEY = ifelse(STATE=="NEW JERSEY", time, 0),
    state_trend_NEW_MEXICO = ifelse(STATE=="NEW MEXICO", time, 0),
    state_trend_NEW_YORK = ifelse(STATE=="NEW YORK", time, 0),
    state_trend_NORTH_CAROLINA = ifelse(STATE=="NORTH CAROLINA", time, 0),
    state_trend_NORTH_DAKOTA = ifelse(STATE=="NORTH DAKOTA", time, 0),
    state_trend_OHIO = ifelse(STATE=="OHIO", time, 0),
    state_trend_OKLAHOMA = ifelse(STATE=="OKLAHOMA", time, 0),
    state_trend_OREGON = ifelse(STATE=="OREGON", time, 0),
    state_trend_PENNSYLVANIA = ifelse(STATE=="PENNSYLVANIA", time, 0),
    state_trend_RHODE_ISLAND = ifelse(STATE=="RHODE ISLAND", time, 0),
    state_trend_SOUTH_CAROLINA = ifelse(STATE=="SOUTH CAROLINA", time, 0),
    state_trend_SOUTH_DAKOTA = ifelse(STATE=="SOUTH DAKOTA", time, 0),
    state_trend_TENNESSEE = ifelse(STATE=="TENNESSEE", time, 0),
    state_trend_TEXAS = ifelse(STATE=="TEXAS", time, 0),
    state_trend_UTAH = ifelse(STATE=="UTAH", time, 0),
    state_trend_VERMONT = ifelse(STATE=="VERMONT", time, 0),
    state_trend_VIRGINIA = ifelse(STATE=="VIRGINIA", time, 0),
    state_trend_WASHINGTON = ifelse(STATE=="WASHINGTON", time, 0),
    state_trend_WEST_VIRGINIA = ifelse(STATE=="WEST VIRGINIA", time, 0),
    state_trend_WISCONSIN = ifelse(STATE=="WISCONSIN", time, 0),
    state_trend_WYOMING = ifelse(STATE=="WYOMING", time, 0)
  ) 

dt_wm %<>% 
  mutate(
    lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(S4,1)),
    lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(S4,2)),
    lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(S4,3)),
    lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(S4,4)),
    lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(S4,5)),
    lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(S4,6)),
    lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(S4,7)),
    lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(S4,8)),
    lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(S4,9)),
    lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(S4,10))
  ) 

write.csv(dt_wm, "data/panel_re_dt_wm.csv", row.names = FALSE)
haven::write_dta(dt_wm, "stata_rob/panel_re_dt_wm.dta")

#### dt_wmo ####

dt_wmo %<>% 
  mutate(
    state_trend_ALABAMA = ifelse(STATE=="ALABAMA", time, 0),
    state_trend_ARIZONA = ifelse(STATE=="ARIZONA", time, 0),
    state_trend_ARKANSAS = ifelse(STATE=="ARKANSAS", time, 0),
    state_trend_CALIFORNIA = ifelse(STATE=="CALIFORNIA", time, 0),
    state_trend_COLORADO = ifelse(STATE=="COLORADO", time, 0),
    state_trend_CONNECTICUT = ifelse(STATE=="CONNECTICUT", time, 0),
    state_trend_DELAWARE = ifelse(STATE=="DELAWARE", time, 0),
    state_trend_FLORIDA = ifelse(STATE=="FLORIDA", time, 0),
    state_trend_GEORGIA = ifelse(STATE=="GEORGIA", time, 0),
    state_trend_IDAHO = ifelse(STATE=="IDAHO", time, 0),
    state_trend_ILLINOIS = ifelse(STATE=="ILLINOIS", time, 0),
    state_trend_INDIANA = ifelse(STATE=="INDIANA", time, 0),
    state_trend_IOWA = ifelse(STATE=="IOWA", time, 0),
    state_trend_KANSAS = ifelse(STATE=="KANSAS", time, 0),
    state_trend_KENTUCKY = ifelse(STATE=="KENTUCKY", time, 0),
    state_trend_LOUISIANA = ifelse(STATE=="LOUISIANA", time, 0),
    state_trend_MAINE = ifelse(STATE=="MAINE", time, 0),
    state_trend_MARYLAND = ifelse(STATE=="MARYLAND", time, 0),
    state_trend_MASSACHUSETTS = ifelse(STATE=="MASSACHUSETTS", time, 0),
    state_trend_MICHIGAN = ifelse(STATE=="MICHIGAN", time, 0),
    state_trend_MINNESOTA = ifelse(STATE=="MINNESOTA", time, 0),
    state_trend_MISSISSIPPI = ifelse(STATE=="MISSISSIPPI", time, 0),
    state_trend_MISSOURI = ifelse(STATE=="MISSOURI", time, 0),
    state_trend_MONTANA = ifelse(STATE=="MONTANA", time, 0),
    state_trend_NEBRASKA = ifelse(STATE=="NEBRASKA", time, 0),
    state_trend_NEVADA = ifelse(STATE=="NEVADA", time, 0),
    state_trend_NEW_HAMPSHIRE = ifelse(STATE=="NEW HAMPSHIRE", time, 0),
    state_trend_NEW_JERSEY = ifelse(STATE=="NEW JERSEY", time, 0),
    state_trend_NEW_MEXICO = ifelse(STATE=="NEW MEXICO", time, 0),
    state_trend_NEW_YORK = ifelse(STATE=="NEW YORK", time, 0),
    state_trend_NORTH_CAROLINA = ifelse(STATE=="NORTH CAROLINA", time, 0),
    state_trend_NORTH_DAKOTA = ifelse(STATE=="NORTH DAKOTA", time, 0),
    state_trend_OHIO = ifelse(STATE=="OHIO", time, 0),
    state_trend_OKLAHOMA = ifelse(STATE=="OKLAHOMA", time, 0),
    state_trend_OREGON = ifelse(STATE=="OREGON", time, 0),
    state_trend_PENNSYLVANIA = ifelse(STATE=="PENNSYLVANIA", time, 0),
    state_trend_RHODE_ISLAND = ifelse(STATE=="RHODE ISLAND", time, 0),
    state_trend_SOUTH_CAROLINA = ifelse(STATE=="SOUTH CAROLINA", time, 0),
    state_trend_SOUTH_DAKOTA = ifelse(STATE=="SOUTH DAKOTA", time, 0),
    state_trend_TENNESSEE = ifelse(STATE=="TENNESSEE", time, 0),
    state_trend_TEXAS = ifelse(STATE=="TEXAS", time, 0),
    state_trend_UTAH = ifelse(STATE=="UTAH", time, 0),
    state_trend_VERMONT = ifelse(STATE=="VERMONT", time, 0),
    state_trend_VIRGINIA = ifelse(STATE=="VIRGINIA", time, 0),
    state_trend_WASHINGTON = ifelse(STATE=="WASHINGTON", time, 0),
    state_trend_WEST_VIRGINIA = ifelse(STATE=="WEST VIRGINIA", time, 0),
    state_trend_WISCONSIN = ifelse(STATE=="WISCONSIN", time, 0),
    state_trend_WYOMING = ifelse(STATE=="WYOMING", time, 0)
  ) 

dt_wmo %<>% 
  mutate(
    lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(S4,1)),
    lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(S4,2)),
    lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(S4,3)),
    lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(S4,4)),
    lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(S4,5)),
    lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(S4,6)),
    lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(S4,7)),
    lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(S4,8)),
    lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(S4,9)),
    lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(S4,10))
  ) 

write.csv(dt_wmo, "data/panel_re_dt_wmo.csv", row.names = FALSE)
haven::write_dta(dt_wmo, "stata_rob/panel_re_dt_wmo.dta")

#### dt_wt #####

dt_wt %<>% 
  mutate(
    state_trend_ALABAMA = ifelse(STATE=="ALABAMA", time, 0),
    state_trend_ARIZONA = ifelse(STATE=="ARIZONA", time, 0),
    state_trend_ARKANSAS = ifelse(STATE=="ARKANSAS", time, 0),
    state_trend_CALIFORNIA = ifelse(STATE=="CALIFORNIA", time, 0),
    state_trend_COLORADO = ifelse(STATE=="COLORADO", time, 0),
    state_trend_CONNECTICUT = ifelse(STATE=="CONNECTICUT", time, 0),
    state_trend_DELAWARE = ifelse(STATE=="DELAWARE", time, 0),
    state_trend_FLORIDA = ifelse(STATE=="FLORIDA", time, 0),
    state_trend_GEORGIA = ifelse(STATE=="GEORGIA", time, 0),
    state_trend_IDAHO = ifelse(STATE=="IDAHO", time, 0),
    state_trend_ILLINOIS = ifelse(STATE=="ILLINOIS", time, 0),
    state_trend_INDIANA = ifelse(STATE=="INDIANA", time, 0),
    state_trend_IOWA = ifelse(STATE=="IOWA", time, 0),
    state_trend_KANSAS = ifelse(STATE=="KANSAS", time, 0),
    state_trend_KENTUCKY = ifelse(STATE=="KENTUCKY", time, 0),
    state_trend_LOUISIANA = ifelse(STATE=="LOUISIANA", time, 0),
    state_trend_MAINE = ifelse(STATE=="MAINE", time, 0),
    state_trend_MARYLAND = ifelse(STATE=="MARYLAND", time, 0),
    state_trend_MASSACHUSETTS = ifelse(STATE=="MASSACHUSETTS", time, 0),
    state_trend_MICHIGAN = ifelse(STATE=="MICHIGAN", time, 0),
    state_trend_MINNESOTA = ifelse(STATE=="MINNESOTA", time, 0),
    state_trend_MISSISSIPPI = ifelse(STATE=="MISSISSIPPI", time, 0),
    state_trend_MISSOURI = ifelse(STATE=="MISSOURI", time, 0),
    state_trend_MONTANA = ifelse(STATE=="MONTANA", time, 0),
    state_trend_NEBRASKA = ifelse(STATE=="NEBRASKA", time, 0),
    state_trend_NEVADA = ifelse(STATE=="NEVADA", time, 0),
    state_trend_NEW_HAMPSHIRE = ifelse(STATE=="NEW HAMPSHIRE", time, 0),
    state_trend_NEW_JERSEY = ifelse(STATE=="NEW JERSEY", time, 0),
    state_trend_NEW_MEXICO = ifelse(STATE=="NEW MEXICO", time, 0),
    state_trend_NEW_YORK = ifelse(STATE=="NEW YORK", time, 0),
    state_trend_NORTH_CAROLINA = ifelse(STATE=="NORTH CAROLINA", time, 0),
    state_trend_NORTH_DAKOTA = ifelse(STATE=="NORTH DAKOTA", time, 0),
    state_trend_OHIO = ifelse(STATE=="OHIO", time, 0),
    state_trend_OKLAHOMA = ifelse(STATE=="OKLAHOMA", time, 0),
    state_trend_OREGON = ifelse(STATE=="OREGON", time, 0),
    state_trend_PENNSYLVANIA = ifelse(STATE=="PENNSYLVANIA", time, 0),
    state_trend_RHODE_ISLAND = ifelse(STATE=="RHODE ISLAND", time, 0),
    state_trend_SOUTH_CAROLINA = ifelse(STATE=="SOUTH CAROLINA", time, 0),
    state_trend_SOUTH_DAKOTA = ifelse(STATE=="SOUTH DAKOTA", time, 0),
    state_trend_TENNESSEE = ifelse(STATE=="TENNESSEE", time, 0),
    state_trend_TEXAS = ifelse(STATE=="TEXAS", time, 0),
    state_trend_UTAH = ifelse(STATE=="UTAH", time, 0),
    state_trend_VERMONT = ifelse(STATE=="VERMONT", time, 0),
    state_trend_VIRGINIA = ifelse(STATE=="VIRGINIA", time, 0),
    state_trend_WASHINGTON = ifelse(STATE=="WASHINGTON", time, 0),
    state_trend_WEST_VIRGINIA = ifelse(STATE=="WEST VIRGINIA", time, 0),
    state_trend_WISCONSIN = ifelse(STATE=="WISCONSIN", time, 0),
    state_trend_WYOMING = ifelse(STATE=="WYOMING", time, 0)
  ) 

dt_wt %<>% 
  mutate(
    lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(S4,1)),
    lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(S4,2)),
    lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(S4,3)),
    lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(S4,4)),
    lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(S4,5)),
    lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(S4,6)),
    lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(S4,7)),
    lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(S4,8)),
    lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(S4,9)),
    lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(S4,10))
  ) 

write.csv(dt_wt, "data/panel_re_dt_wt.csv", row.names = FALSE)
haven::write_dta(dt_wt, "stata_rob/panel_re_dt_wt.dta")