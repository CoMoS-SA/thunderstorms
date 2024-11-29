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

bea <- read_csv("data/CAINC4/CAINC4__ALL_AREAS_1969_2021.csv", 
                col_types = cols(`1969` = col_number(), 
                                 `1970` = col_number(),
                                 `1971` = col_number(),
                                 `1972` = col_number(),
                                 `1973` = col_number(),
                                 `1974` = col_number(),
                                 `1975` = col_number(),
                                 `1976` = col_number(),
                                 `1977` = col_number(),
                                 `1978` = col_number(),
                                 `1979` = col_number(),
                                 `1980` = col_number(),
                                 `1981` = col_number(),
                                 `1982` = col_number(),
                                 `1983` = col_number(),
                                 `1984` = col_number(),
                                 `1985` = col_number(),
                                 `1986` = col_number(),
                                 `1987` = col_number(),
                                 `1988` = col_number(),
                                 `1989` = col_number(),
                                 `1990` = col_number(),
                                 `1991` = col_number(),
                                 `1992` = col_number(),
                                 `1993` = col_number(),
                                 `1994` = col_number(),
                                 `1995` = col_number(),
                                 `1996` = col_number(),
                                 `1997` = col_number(),
                                 `1998` = col_number(),
                                 `1999` = col_number(),
                                 `2000` = col_number(),
                                 `2001` = col_number(),
                                 `2002` = col_number(),
                                 `2003` = col_number(),
                                 `2004` = col_number(),
                                 `2005` = col_number(),
                                 `2006` = col_number(),
                                 `2007` = col_number(),
                                 `2008` = col_number(),
                                 `2009` = col_number(),
                                 `2010` = col_number(),
                                 `2011` = col_number(),
                                 `2012` = col_number(),
                                 `2013` = col_number(),
                                 `2014` = col_number(),
                                 `2015` = col_number(),
                                 `2016` = col_number(),
                                 `2017` = col_number(),
                                 `2018` = col_number(),
                                 `2019` = col_number(),
                                 `2020` = col_number(),
                                 `2021` = col_number(),
                ))


bea %<>% 
  select(-c("1969":"1990")) %>% 
  filter(Description%in%c("Wage and salary employment", "Wages and salaries")) %>% 
  select(GeoFIPS, Description, "1991":"2019") %>% 
  pivot_longer(cols ="1991":"2019", names_to = "Year") %>% 
  pivot_wider(names_from="Description", values_from = "value") %>% 
  rename(
    employment_wage_bea = `Wage and salary employment`,
    wage_bea = `Wages and salaries`
  )


PCE_DEFLATOR <- read_csv("utilities/PCE_deflator.csv")

PCE_DEFLATOR %<>% 
  mutate(
    DATE=str_sub(DATE,1,4)
  ) %>% 
  rename(PCE_DEFLATOR = DPCERD3A086NBEA)

bea %<>% 
  mutate(Year=as.character(Year)) %>% 
  left_join(
    .,
    PCE_DEFLATOR,
    by=c("Year"="DATE")
  ) 

bea %<>% 
  mutate(PCE_DEFLATOR = PCE_DEFLATOR/100) %>% 
  mutate(
    wage_bea = wage_bea/PCE_DEFLATOR,
  ) %>% 
  select(-PCE_DEFLATOR)


data <- readRDS(file="data/f_panel_tr.rds")

data %<>% 
  left_join(
    .,
    bea,
    by=c("FIPS"="GeoFIPS", "Year"="Year")
  )


data %<>% 
  mutate(
    wage_pe_bea=wage_bea/employment_wage_bea
  ) %>% 
  arrange(FIPS, Year) %>% 
  group_by(FIPS) %>% 
  mutate(
    across(wage_pe_bea, ~ ((.x/lag(.x))-1)*100, .names = "GR_{.col}"
    )
  ) %>% ungroup()

data %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )


#save to stata 

data_stata <- data %>% 
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

data_stata %<>% 
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

write.csv(data_stata, "data/panel_bea.csv", row.names = FALSE)
haven::write_dta(data_stata, "stata_rob/panel_bea.dta")

#### Estimation ####

source("new_scripts/functions_f.R")

#Baseline model wages
data$x<-data$S4
data$y<-data$GR_AAP_TOT_TOT

mod0_w_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

cum0_w_f <- getcumul10_f(mod0_w_f, data, "stata_rob/vcov_conley50_1lag.xlsx")

#Robustness BEA
data$x<-data$S4
data$y<-data$GR_wage_pe_bea

modb_w_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

cumb_w_f <- getcumul10_f(modb_w_f, data, "stata_rob/vcov_bea.xlsx")


#### Plot ####

rbind(
  cum0_w_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Baseline"),
  cumb_w_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="BEA")
) %>% 
  mutate(
    var = factor(
      var,
      levels = c("Baseline","BEA"),
      labels = c("QCEW (Baseline)","BEA")
    )
  ) %>% 
  ggplot(aes(x=year, y=coeffsums_sd, color=factor(var))) +
  geom_line(linewidth=0.8) +
  geom_line(aes(y=lower, linetype=factor(var))) +
  geom_line(aes(y=upper, linetype=factor(var))) +
  xlab("Years since storm exposure") +
  ylab(expression("% change in Annual Average Pay per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_colour_manual(values = c("QCEW (Baseline)"="#d7301f", "BEA"="#fdae61")) +
  scale_linetype_manual(values = c("QCEW (Baseline)"=2, "BEA"=2)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1, 'cm')
  ) -> g_rob_bea

ggsave("charts/bea_rob.pdf", g_rob_bea, width=5.85, height = 4.4)

#### Tables ####

yy <- NULL
for(i in 0:10){
  new <- c(i, "")
  yy <- c(yy, new)
}

cbind(
  tablize_f(cum0_w_f, mod0_w_f),
  tablize_f(cumb_w_f, modb_w_f) %>% select(-year)
) %>% 
  `colnames<-`(letters[1:3]) %>% 
  mutate(a=yy) %>% 
  add_row(a="County fixed effects", b="\\checkmark", c="\\checkmark") %>%
  add_row(a="Year fixed effects",   b="\\checkmark", c="\\checkmark") %>%
  add_row(a="State-level trends",   b="\\checkmark", c="\\checkmark") %>%
  add_row(a="Climatic controls",    b="\\checkmark", c="\\checkmark") %>%
  add_row(a="Adjusted $R^2$", 
          b=as.character(round(r2(mod0_w_f, type="ar2"),3)), 
          c=as.character(round(r2(modb_w_f, type="ar2"),3))
  ) %>% 
  add_row(a="Observations", 
          b=as.character(nobs(mod0_w_f)), 
          c=as.character(nobs(modb_w_f))
  ) %>% 
  `colnames<-`(c("Years since storm exposure", 
                 "QCEW (Baseline)", 
                 "BEA")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 6, hline_after = T) %>% 
  row_spec(10*2 + 8, hline_after = T) %>% 
  add_header_above(c(" " = 1, 
                     "Annual Average Pay" = 2
  ), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., "tables/bea_rob.tex")
