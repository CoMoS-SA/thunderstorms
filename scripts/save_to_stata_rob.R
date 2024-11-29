library(tidyverse)
library(magrittr)

data <- readRDS(file="data/f_panel_tr.rds")

data %<>% 
  mutate(time = as.numeric(Year)) 

data %<>% 
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

data %<>% 
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


#### Area Riskiness ####


data %>% 
  group_by(Area) %>% 
  summarise(
    quart = mean(S4)
  ) %>% 
  ungroup() %>% 
  mutate(
    quart = ntile(quart, 4)
  ) -> aux

data %<>% 
  left_join(.,aux, by="Area") %>% 
  mutate(tmm_risk=as.factor(case_when(quart==1~1, quart%in%c(2,3)~2, T~3)))

remove(aux)

#### Area Richness ####

data %<>% select(-quart)

data %>% 
  filter(Year==1991) %>% 
  mutate(
    quart = ntile(INCOME_PC, 4)
  ) %>% select(Area, quart) -> aux

data %<>% 
  left_join(.,aux, by="Area") %>% 
  mutate(tmm_rich=as.factor(case_when(quart==1~1, quart%in%c(2,3)~2, T~3)))


#### Disasters ####

data %<>% select(-quart)

data %<>% 
  mutate(
    dmm_IS_D = IS_D
  ) %>% 
  mutate(
    dmm_IS_D_lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(dmm_IS_D,1)),
    dmm_IS_D_lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(dmm_IS_D,2)),
    dmm_IS_D_lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(dmm_IS_D,3)),
    dmm_IS_D_lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(dmm_IS_D,4)),
    dmm_IS_D_lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(dmm_IS_D,5)),
    dmm_IS_D_lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(dmm_IS_D,6)),
    dmm_IS_D_lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(dmm_IS_D,7)),
    dmm_IS_D_lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(dmm_IS_D,8)),
    dmm_IS_D_lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(dmm_IS_D,9)),
    dmm_IS_D_lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(dmm_IS_D,10))
  ) %>% 
  mutate(
    dmm_IS_D2 = IS_D2
  ) %>% 
  mutate(
    dmm_IS_D2_lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(dmm_IS_D2,1)),
    dmm_IS_D2_lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(dmm_IS_D2,2)),
    dmm_IS_D2_lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(dmm_IS_D2,3)),
    dmm_IS_D2_lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(dmm_IS_D2,4)),
    dmm_IS_D2_lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(dmm_IS_D2,5)),
    dmm_IS_D2_lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(dmm_IS_D2,6)),
    dmm_IS_D2_lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(dmm_IS_D2,7)),
    dmm_IS_D2_lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(dmm_IS_D2,8)),
    dmm_IS_D2_lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(dmm_IS_D2,9)),
    dmm_IS_D2_lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(dmm_IS_D2,10))
  ) %>% 
  mutate(
    dmm_IS_D3 = IS_D3
  ) %>% 
  mutate(
    dmm_IS_D3_lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(dmm_IS_D3,1)),
    dmm_IS_D3_lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(dmm_IS_D3,2)),
    dmm_IS_D3_lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(dmm_IS_D3,3)),
    dmm_IS_D3_lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(dmm_IS_D3,4)),
    dmm_IS_D3_lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(dmm_IS_D3,5)),
    dmm_IS_D3_lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(dmm_IS_D3,6)),
    dmm_IS_D3_lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(dmm_IS_D3,7)),
    dmm_IS_D3_lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(dmm_IS_D3,8)),
    dmm_IS_D3_lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(dmm_IS_D3,9)),
    dmm_IS_D3_lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(dmm_IS_D3,10))
  ) %>% 
  mutate(
    dmm_DISASTER_YEAR_EXT = DISASTER_YEAR_EXT
  ) %>% 
  mutate(
    dmm_DISASTER_YEAR_EXT_lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(dmm_DISASTER_YEAR_EXT,1)),
    dmm_DISASTER_YEAR_EXT_lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(dmm_DISASTER_YEAR_EXT,2)),
    dmm_DISASTER_YEAR_EXT_lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(dmm_DISASTER_YEAR_EXT,3)),
    dmm_DISASTER_YEAR_EXT_lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(dmm_DISASTER_YEAR_EXT,4)),
    dmm_DISASTER_YEAR_EXT_lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(dmm_DISASTER_YEAR_EXT,5)),
    dmm_DISASTER_YEAR_EXT_lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(dmm_DISASTER_YEAR_EXT,6)),
    dmm_DISASTER_YEAR_EXT_lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(dmm_DISASTER_YEAR_EXT,7)),
    dmm_DISASTER_YEAR_EXT_lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(dmm_DISASTER_YEAR_EXT,8)),
    dmm_DISASTER_YEAR_EXT_lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(dmm_DISASTER_YEAR_EXT,9)),
    dmm_DISASTER_YEAR_EXT_lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(dmm_DISASTER_YEAR_EXT,10))
  ) 


#### Lead ####

data %<>% 
  mutate(
    lead1 = ifelse(Year>=2019, as.numeric(NA), dplyr::lead(S4,1)),
    lead2 = ifelse(Year>=2018, as.numeric(NA), dplyr::lead(S4,2)),
    lead3 = ifelse(Year>=2017, as.numeric(NA), dplyr::lead(S4,3)),
    lead4 = ifelse(Year>=2016, as.numeric(NA), dplyr::lead(S4,4))
  ) 

#### Alt mod ###

data %<>% 
  mutate(
    tmp_sq = tmp^2,
    prec_sq = prec^2,
    ST_ACT_POP=ST_AAE_TOT_TOT/POPULATION
  )


#### Alt exposure ####

data %<>% 
  mutate(
    S1_lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(S1,1)),
    S1_lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(S1,2)),
    S1_lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(S1,3)),
    S1_lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(S1,4)),
    S1_lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(S1,5)),
    S1_lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(S1,6)),
    S1_lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(S1,7)),
    S1_lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(S1,8)),
    S1_lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(S1,9)),
    S1_lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(S1,10))
  ) %>% 
  mutate(
    S2_lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(S2,1)),
    S2_lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(S2,2)),
    S2_lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(S2,3)),
    S2_lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(S2,4)),
    S2_lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(S2,5)),
    S2_lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(S2,6)),
    S2_lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(S2,7)),
    S2_lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(S2,8)),
    S2_lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(S2,9)),
    S2_lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(S2,10))
  ) %>% 
  mutate(
    S3_lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(S3,1)),
    S3_lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(S3,2)),
    S3_lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(S3,3)),
    S3_lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(S3,4)),
    S3_lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(S3,5)),
    S3_lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(S3,6)),
    S3_lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(S3,7)),
    S3_lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(S3,8)),
    S3_lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(S3,9)),
    S3_lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(S3,10))
  )

#### AR ####

data %<>% 
  mutate(
    GR_AAP_TOT_TOT_lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(GR_AAP_TOT_TOT,1)),
    GR_AAP_TOT_TOT_lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(GR_AAP_TOT_TOT,2)),
    GR_AAP_TOT_TOT_lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(GR_AAP_TOT_TOT,3)),
    GR_AAP_TOT_TOT_lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(GR_AAP_TOT_TOT,4))
  ) %>% 
  mutate(
    GR_INCOME_PC_lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(GR_INCOME_PC,1)),
    GR_INCOME_PC_lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(GR_INCOME_PC,2)),
    GR_INCOME_PC_lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(GR_INCOME_PC,3)),
    GR_INCOME_PC_lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(GR_INCOME_PC,4))
  )

#### Time subset ####

data %>%
  filter(time>1997) -> d96

data %>% 
  filter(time>2002) -> d03

data %>% 
  filter(time<2008) -> d08

data %>% 
  filter(time<2013) -> d13

min(data$time)
min(d08$time)

d96 %<>% 
  mutate(
    lag1 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(S4,1)),
    lag2 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(S4,2)),
    lag3 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(S4,3)),
    lag4 = ifelse(Year==2001, as.numeric(NA), dplyr::lag(S4,4)),
    lag5 = ifelse(Year==2002, as.numeric(NA), dplyr::lag(S4,5)),
    lag6 = ifelse(Year==2003, as.numeric(NA), dplyr::lag(S4,6)),
    lag7 = ifelse(Year==2004, as.numeric(NA), dplyr::lag(S4,7)),
    lag8 = ifelse(Year==2005, as.numeric(NA), dplyr::lag(S4,8)),
    lag9 = ifelse(Year==2006, as.numeric(NA), dplyr::lag(S4,9)),
    lag10 = ifelse(Year==2007, as.numeric(NA), dplyr::lag(S4,10))
  ) 


d03 %<>% 
  mutate(
    lag1 = ifelse(Year==2003, as.numeric(NA), dplyr::lag(S4,1)),
    lag2 = ifelse(Year==2004, as.numeric(NA), dplyr::lag(S4,2)),
    lag3 = ifelse(Year==2005, as.numeric(NA), dplyr::lag(S4,3)),
    lag4 = ifelse(Year==2006, as.numeric(NA), dplyr::lag(S4,4)),
    lag5 = ifelse(Year==2007, as.numeric(NA), dplyr::lag(S4,5)),
    lag6 = ifelse(Year==2008, as.numeric(NA), dplyr::lag(S4,6)),
    lag7 = ifelse(Year==2009, as.numeric(NA), dplyr::lag(S4,7)),
    lag8 = ifelse(Year==2010, as.numeric(NA), dplyr::lag(S4,8)),
    lag9 = ifelse(Year==2011, as.numeric(NA), dplyr::lag(S4,9)),
    lag10 = ifelse(Year==2012, as.numeric(NA), dplyr::lag(S4,10))
  ) 


d08 %<>% 
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


d13 %<>% 
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


#### Save ###
write.csv(data, "data/panel_rob_csv.csv", row.names = FALSE)
haven::write_dta(data, "stata_rob/panel_rob_dta.dta")

write.csv(d96, "data/panel_d96.csv", row.names = FALSE)
haven::write_dta(d96, "stata_rob/panel_d96.dta")

write.csv(d03, "data/panel_d03.csv", row.names = FALSE)
haven::write_dta(d03, "stata_rob/panel_d03.dta")

write.csv(d08, "data/panel_d08.csv", row.names = FALSE)
haven::write_dta(d08, "stata_rob/panel_d08.dta")

write.csv(d13, "data/panel_d13.csv", row.names = FALSE)
haven::write_dta(d13, "stata_rob/panel_d13.dta")
