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


data %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )


data %>%
  filter(time>1997) -> d96

data %>% 
  filter(time>2002) -> d03

data %>% 
  filter(time<2008) -> d08

data %>% 
  filter(time<2013) -> d13


#Wages
data$x<-data$S4
data$y<-data$GR_AAP_TOT_TOT

mod_w0 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_w0 <- getcumul10_f(mod_w0, data, "stata_rob/vcov_conley50_1lag.xlsx")

#
d96$x<-d96$S4
d96$y<-d96$GR_AAP_TOT_TOT

mod_w1 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(d96, panel.id = c("Area", "Year"))
) 
cum_w1 <- getcumul10_f(mod_w1, d96, "stata_rob/vcov_w_alttime_d96.xlsx")

#
d03$x<-d03$S4
d03$y<-d03$GR_AAP_TOT_TOT

mod_w2 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(d03, panel.id = c("Area", "Year"))
) 
cum_w2 <- getcumul10_f(mod_w2, d03, "stata_rob/vcov_w_alttime_d03.xlsx")

#
d08$x<-d08$S4
d08$y<-d08$GR_AAP_TOT_TOT

mod_w3 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(d08, panel.id = c("Area", "Year"))
) 
cum_w3 <- getcumul10_f(mod_w3, d08, "stata_rob/vcov_w_alttime_d08.xlsx")

#
d13$x<-d13$S4
d13$y<-d13$GR_AAP_TOT_TOT

mod_w4 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(d13, panel.id = c("Area", "Year"))
) 
cum_w4 <- getcumul10_f(mod_w4, d13, "stata_rob/vcov_w_alttime_d13.xlsx")




#Income
data$x<-data$S4
data$y<-data$GR_INCOME_PC

mod_i0 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_i0 <- getcumul10_f(mod_i0, data, "stata_rob/income_vcov_conley50_1lag.xlsx")

#
d96$x<-d96$S4
d96$y<-d96$GR_INCOME_PC

mod_i1 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(d96, panel.id = c("Area", "Year"))
) 
cum_i1 <- getcumul10_f(mod_i1, d96, "stata_rob/vcov_i_alttime_d96.xlsx")

#
d03$x<-d03$S4
d03$y<-d03$GR_INCOME_PC

mod_i2 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(d03, panel.id = c("Area", "Year"))
) 
cum_i2 <- getcumul10_f(mod_i2, d03, "stata_rob/vcov_i_alttime_d03.xlsx")

#
d08$x<-d08$S4
d08$y<-d08$GR_INCOME_PC

mod_i3 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(d08, panel.id = c("Area", "Year"))
) 
cum_i3 <- getcumul10_f(mod_i3, d08, "stata_rob/vcov_i_alttime_d08.xlsx")

#
d13$x<-d13$S4
d13$y<-d13$GR_INCOME_PC

mod_i4 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(d13, panel.id = c("Area", "Year"))
) 
cum_i4 <- getcumul10_f(mod_i4, d13, "stata_rob/vcov_i_alttime_d13.xlsx")



#### Tables ####



cbind(
  tablize_f(cum_w0, mod_w0) ,
  tablize_f(cum_w1, mod_w1) %>% select(-year),
  tablize_f(cum_w2, mod_w2) %>% select(-year),
  tablize_f(cum_w3, mod_w3) %>% select(-year),
  tablize_f(cum_w4, mod_w4) %>% select(-year),
  tablize_f(cum_i0, mod_i0) %>% select(-year),
  tablize_f(cum_i1, mod_i1) %>% select(-year),
  tablize_f(cum_i2, mod_i2) %>% select(-year),
  tablize_f(cum_i3, mod_i3) %>% select(-year),
  tablize_f(cum_i4, mod_i4) %>% select(-year)
) %>% 
  `colnames<-`(c("a", "b-Baseline", "c-Wage97", "d-Wage2002", "e-Wage2008", "f-Wage2013", "g-Baseline(Income)", "h-Income97", "i-Income2002", "j-Income2008", "k-Income2013")) %>% 
  filter(a%in%c(0,3,10)) %>% 
  mutate(
    a = case_when(
      a==0 ~ "Contemporeneous",
      a==3 ~ "Short term",
      T ~ "Long term",
    )
  ) -> out 

out %>% 
  pivot_longer(-a) %>% 
  pivot_wider(names_from=a) %>% 
  unnest() %>% 
  group_by(name) %>% 
  summarise(across(everything(), ~ paste0("\\begin{tabular}{@{}c@{}}", paste(.x, collapse = " \\\\ "), "\\end{tabular}" ) )) %>% 
  add_column("") %>% 
  add_column(
    r=c(
      as.character(round(r2(mod_w0, type="ar2"),3)), 
      as.character(round(r2(mod_w1, type="ar2"),3)),
      as.character(round(r2(mod_w2, type="ar2"),3)),
      as.character(round(r2(mod_w3, type="ar2"),3)),
      as.character(round(r2(mod_w4, type="ar2"),3)),
      as.character(round(r2(mod_i0, type="ar2"),3)),
      as.character(round(r2(mod_i1, type="ar2"),3)),
      as.character(round(r2(mod_i2, type="ar2"),3)),
      as.character(round(r2(mod_i3, type="ar2"),3)),
      as.character(round(r2(mod_i4, type="ar2"),3))
    )
  ) %>% 
  add_column(
    obs=c(
      as.character(nobs(mod_w0)), 
      as.character(nobs(mod_w1)),
      as.character(nobs(mod_w2)),
      as.character(nobs(mod_w3)),
      as.character(nobs(mod_w4)),
      as.character(nobs(mod_i0)),
      as.character(nobs(mod_i1)),
      as.character(nobs(mod_i2)),
      as.character(nobs(mod_i3)),
      as.character(nobs(mod_i4))
    )
  ) %>% 
  mutate(name=c("Baseline", ">1997", ">2002", "<2008","<2013","Baseline", ">1997", ">2002", "<2008", "<2013")) %>% 
  `colnames<-`(c(" ", "Contemporaneous", "Short term", "Long term", "", "Adjusted $R^2$", "Observations")) %>% 
  kbl(escape = F, booktabs = T, align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center") %>% 
  pack_rows("Annual Average Pay", 1, 5) %>% 
  pack_rows("Income per capita", 6, 10) %>% 
  add_header_above(c(" " = 1, "Estimated impacts" = 3, " "=3), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., "tables/rob_alttime.tex")


