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

#### Baseline Estimation ####

#Wages
data$x<-data$S4
data$y<-data$GR_AAP_TOT_TOT

mod_w0 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_w0)
cum_w0 <- getcumul10_f(mod_w0, data, "stata_rob/vcov_conley50_1lag.xlsx")


mod_w1 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year, #+ factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_w1)
cum_w1 <- getcumul10_f(mod_w1, data, "stata_rob/vcov_w_altfe_notrend.xlsx")#, "stata/vcov_conley50_1lag.xlsx")


mod_w2 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)^factor(Year),
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_w2)
cum_w2 <- getcumul10_f(mod_w2, data, "stata_rob/vcov_w_altfe_stateyear.xlsx")#, "stata/vcov_conley50_1lag.xlsx")


#Income

data$x<-data$S4
data$y<-data$GR_INCOME_PC

mod_i0 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_i0)
cum_i0 <- getcumul10_f(mod_i0, data, "stata_rob/income_vcov_conley50_1lag.xlsx")


mod_i1 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year, #+ factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_i1)
cum_i1 <- getcumul10_f(mod_i1, data, "stata_rob/vcov_i_altfe_notrend.xlsx")#, "stata/vcov_conley50_1lag.xlsx")


mod_i2 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)^factor(Year),
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_i2)
cum_i2 <- getcumul10_f(mod_i2, data, "stata_rob/vcov_w_altfe_stateyear.xlsx")#, "stata/vcov_conley50_1lag.xlsx")



#### Table ####

yy <- NULL
for(i in 0:10){
  new <- c(i, "")
  yy <- c(yy, new)
}

cbind(
  tablize_f(cum_w0, mod_w0),
  tablize_f(cum_w1, mod_w1) %>% select(-year),
  tablize_f(cum_w2, mod_w2) %>% select(-year),
  tablize_f(cum_i0, mod_i0) %>% select(-year),
  tablize_f(cum_i1, mod_i1) %>% select(-year),
  tablize_f(cum_i2, mod_i2) %>% select(-year)
) %>% 
  `colnames<-`(letters[1:7]) %>% 
  mutate(a=yy) %>% 
  add_row(a="Climatic controls", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark") %>%
  add_row(a="County fixed effects", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark") %>%
  add_row(a="Year fixed effects", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark") %>%
  add_row(a="State-Level trends", b="\\checkmark", c=" ", d=" ", e="\\checkmark", f=" ", g="\\checkmark") %>%
  add_row(a="State-Year dummies", b=" ", c=" ", d="\\checkmark", e=" ", f=" ", g="\\checkmark") %>%
  add_row(a="Adjusted $R^2$", 
          b=as.character(round(r2(mod_w0, type="ar2"),3)), 
          c=as.character(round(r2(mod_w1, type="ar2"),3)),
          d=as.character(round(r2(mod_w2, type="ar2"),3)),
          e=as.character(round(r2(mod_i0, type="ar2"),3)),
          f=as.character(round(r2(mod_i1, type="ar2"),3)),
          g=as.character(round(r2(mod_i2, type="ar2"),3)),
  ) %>% 
  add_row(a="Observations", 
          b=as.character(nobs(mod_w0)), 
          c=as.character(nobs(mod_w1)),
          d=as.character(nobs(mod_w2)),
          e=as.character(nobs(mod_i0)),
          f=as.character(nobs(mod_i1)),
          g=as.character(nobs(mod_i2))
  ) %>% 
  `colnames<-`(c("Years since storm exposure", "Baseline", "(1)","(2)","Baseline","(1)","(2)")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  #pack_rows("", lags*2 + 3, lags*2 + 7) %>% 
  #pack_rows("", lags*2 + 8, lags*2 + 9) %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 7, hline_after = T) %>% 
  add_header_above(c(" " = 1, "Growth rate of Annual Average Pay" = 3, "Growth rate of Income per capita"=3), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., paste0("tables/rob_altfe.tex"))
