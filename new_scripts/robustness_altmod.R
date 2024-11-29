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

data %<>% 
  mutate(
    time = as.numeric(as.character(Year)),
    tmp_sq = tmp^2,
    prec_sq = prec^2,
    ST_ACT_POP=ST_AAE_TOT_TOT/ST_POPULATION,
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
cum_w0 <- getcumul10_f(mod_w0, data, "stata/vcov_conley50_1lag.xlsx")

mod_w1 <- fixest::feols(
  fml = y ~ l(x,0:10) | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_w1 <- getcumul10_f(mod_w1, data, "stata_rob/vcov_w_altmod_1.xlsx")

mod_w2 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec +
    tmp_sq + prec_sq | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_w2 <- getcumul10_f(mod_w2, data, "stata_rob/vcov_w_altmod_2.xlsx")

mod_w3 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec +
    tmp_sq + prec_sq + GR_AAE_TOT_TOT | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_w3 <- getcumul10_f(mod_w3, data, "stata_rob/vcov_w_altmod_3.xlsx")

mod_w4 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec +
    tmp_sq + prec_sq + GR_AAE_TOT_TOT + GR_POPULATION | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_w4 <- getcumul10_f(mod_w4, data, "stata_rob/vcov_w_altmod_4.xlsx")

mod_w5 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec +
    tmp_sq + prec_sq + GR_AAE_TOT_TOT + GR_POPULATION + l(ST_ACT_POP,1) | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_w5 <- getcumul10_f(mod_w5, data, "stata_rob/vcov_w_altmod_5.xlsx")

mod_w6 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec +
    tmp_sq + prec_sq + GR_AAE_TOT_TOT + GR_POPULATION + l(ST_ACT_POP,1) + l(ST_AAP_TOT_TOT,1) | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_w6 <- getcumul10_f(mod_w6, data, "stata_rob/vcov_w_altmod_6.xlsx")


#Income
data$x<-data$S4
data$y<-data$GR_INCOME_PC

mod_i0 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_i0 <- getcumul10_f(mod_i0, data, "stata/income_vcov_conley50_1lag.xlsx")

mod_i1 <- fixest::feols(
  fml = y ~ l(x,0:10) | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_i1 <- getcumul10_f(mod_i1, data, "stata_rob/vcov_i_altmod_1.xlsx")

mod_i2 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec +
    tmp_sq + prec_sq | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_i2 <- getcumul10_f(mod_i2, data, "stata_rob/vcov_i_altmod_2.xlsx")

mod_i3 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec +
    tmp_sq + prec_sq + GR_AAE_TOT_TOT | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_i3 <- getcumul10_f(mod_i3, data, "stata_rob/vcov_i_altmod_3.xlsx")

mod_i4 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec +
    tmp_sq + prec_sq + GR_AAE_TOT_TOT + GR_POPULATION | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_i4 <- getcumul10_f(mod_i4, data, "stata_rob/vcov_i_altmod_4.xlsx")

mod_i5 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec +
    tmp_sq + prec_sq + GR_AAE_TOT_TOT + GR_POPULATION + l(ST_ACT_POP,1) | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_i5 <- getcumul10_f(mod_i5, data, "stata_rob/vcov_i_altmod_5.xlsx")

mod_i6 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec +
    tmp_sq + prec_sq + GR_AAE_TOT_TOT + GR_POPULATION + l(ST_ACT_POP,1) + l(ST_INCOME_PC,1) | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_i6 <- getcumul10_f(mod_i6, data, "stata_rob/vcov_i_altmod_6.xlsx")


#### Tables ####

yy <- NULL
for(i in 0:10){
  new <- c(i, "")
  yy <- c(yy, new)
}

cbind(
  tablize_f(cum_w0, mod_w0),
  tablize_f(cum_w1, mod_w1) %>% select(-year),
  tablize_f(cum_w2, mod_w2) %>% select(-year),
  tablize_f(cum_w3, mod_w3) %>% select(-year),
  tablize_f(cum_w4, mod_w4) %>% select(-year),
  tablize_f(cum_w5, mod_w5) %>% select(-year),
  tablize_f(cum_w6, mod_w6) %>% select(-year)
) %>% 
  `colnames<-`(letters[1:8]) %>% 
  mutate(a=yy) %>% 
  add_row(a="County fixed effects", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Year fixed effects", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="State-Level trends", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Climatic controls", b="\\checkmark", c=" ", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Squared climatic controls", b=" ", c=" ", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Growth rate of Employment", b=" ", c=" ", d=" ", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Growth rate of Population", b=" ", c=" ", d=" ", e=" ", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="State-Level Lagged Active population", b=" ", c=" ", d=" ", e=" ", f=" ", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="State-Level Lagged Annual Average Pay", b=" ", c=" ", d=" ", e=" ", f="", g=" ", h="\\checkmark") %>%
  add_row(a="Adjusted $R^2$", 
          b=as.character(round(r2(mod_w0, type="ar2"),3)), 
          c=as.character(round(r2(mod_w1, type="ar2"),3)),
          d=as.character(round(r2(mod_w2, type="ar2"),3)), 
          e=as.character(round(r2(mod_w3, type="ar2"),3)),
          f=as.character(round(r2(mod_w4, type="ar2"),3)), 
          g=as.character(round(r2(mod_w5, type="ar2"),3)),
          h=as.character(round(r2(mod_w6, type="ar2"),3))
  ) %>% 
  add_row(a="Observations", 
          b=as.character(nobs(mod_w0)), 
          c=as.character(nobs(mod_w1)),
          d=as.character(nobs(mod_w2)), 
          e=as.character(nobs(mod_w3)),
          f=as.character(nobs(mod_w4)), 
          g=as.character(nobs(mod_w5)),
          h=as.character(nobs(mod_w6))
  ) %>% 
  `colnames<-`(c("Years since storm exposure", "Baseline", "(1)","(2)","(3)","(4)","(5)","(6)")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 11, hline_after = T) %>% 
  add_header_above(c(" " = 1, "Growth rate of Annual Average Pay" = 6), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., paste0("tables/rob_altmod_wages.tex"))



cbind(
  tablize_f(cum_i0, mod_i0),
  tablize_f(cum_i1, mod_i1) %>% select(-year),
  tablize_f(cum_i2, mod_i2) %>% select(-year),
  tablize_f(cum_i3, mod_i3) %>% select(-year),
  tablize_f(cum_i4, mod_i4) %>% select(-year),
  tablize_f(cum_i5, mod_i5) %>% select(-year),
  tablize_f(cum_i6, mod_i6) %>% select(-year)
) %>% 
  `colnames<-`(letters[1:8]) %>% 
  mutate(a=yy) %>% 
  add_row(a="County fixed effects", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Year fixed effects", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="State-Level trends", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Climatic controls", b="\\checkmark", c=" ", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Squared climatic controls", b=" ", c=" ", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Growth rate of Employment", b=" ", c=" ", d=" ", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Growth rate of Population", b=" ", c=" ", d=" ", e=" ", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="State-Level Lagged Active population", b=" ", c=" ", d=" ", e=" ", f=" ", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="State-Level Lagged Income per capita", b=" ", c=" ", d=" ", e=" ", f="", g=" ", h="\\checkmark") %>%
  add_row(a="Adjusted $R^2$", 
          b=as.character(round(r2(mod_i0, type="ar2"),3)), 
          c=as.character(round(r2(mod_i1, type="ar2"),3)),
          d=as.character(round(r2(mod_i2, type="ar2"),3)), 
          e=as.character(round(r2(mod_i3, type="ar2"),3)),
          f=as.character(round(r2(mod_i4, type="ar2"),3)), 
          g=as.character(round(r2(mod_i5, type="ar2"),3)),
          h=as.character(round(r2(mod_i6, type="ar2"),3))
  ) %>% 
  add_row(a="Observations", 
          b=as.character(nobs(mod_i0)), 
          c=as.character(nobs(mod_i1)),
          d=as.character(nobs(mod_i2)), 
          e=as.character(nobs(mod_i3)),
          f=as.character(nobs(mod_i4)), 
          g=as.character(nobs(mod_i5)),
          h=as.character(nobs(mod_i6))
  ) %>% 
  `colnames<-`(c("Years since storm exposure", "Baseline", "(1)","(2)","(3)","(4)","(5)","(6)")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 11, hline_after = T) %>% 
  add_header_above(c(" " = 1, "Growth rate of Income per capita" = 6), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., "tables/rob_altmod_income.tex")


