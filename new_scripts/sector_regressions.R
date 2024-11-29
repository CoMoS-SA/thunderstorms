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

# crosswalk https://www.bls.gov/cew/classifications/industry/industry-supersectors.htm
# aggregation levels https://www.bls.gov/cew/classifications/aggregation/agg-level-titles.htm

source("scripts/functions_f.R")

datas <- readRDS(file="data/f_panel_sectors_tr.rds")

datas %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )

#### Employment ####

#### Natmin

datas$x<-datas$S4
datas$y<-datas$GR_AAE_PRIV_NATMIN

mod0_enm_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(datas, panel.id = c("Area", "Year"))
) 

cum0_enm_f <- getcumul10_f(mod0_enm_f, datas, "stata_rob/vcov_e_natmin.xlsx")


#### constr

datas$x<-datas$S4
datas$y<-datas$GR_AAE_PRIV_CONSTR

mod0_eco_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(datas, panel.id = c("Area", "Year"))
) 

cum0_eco_f <- getcumul10_f(mod0_eco_f, datas, "stata_rob/vcov_e_constr.xlsx")


#### manuf

datas$x<-datas$S4
datas$y<-datas$GR_AAE_PRIV_MANUF

mod0_ema_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(datas, panel.id = c("Area", "Year"))
) 

cum0_ema_f <- getcumul10_f(mod0_ema_f, datas, "stata_rob/vcov_e_manuf.xlsx")



#### services

datas$x<-datas$S4
datas$y<-datas$GR_AAE_PRIV_SERV

mod0_ese_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(datas, panel.id = c("Area", "Year"))
) 

cum0_ese_f <- getcumul10_f(mod0_ese_f, datas, "stata_rob/vcov_e_serv.xlsx")



#### Wages ####


#### Natmin

datas$x<-datas$S4
datas$y<-datas$GR_AAP_PRIV_NATMIN

mod0_wnm_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(datas, panel.id = c("Area", "Year"))
) 

cum0_wnm_f <- getcumul10_f(mod0_wnm_f, datas, "stata_rob/vcov_w_natmin.xlsx")


#### constr

datas$x<-datas$S4
datas$y<-datas$GR_AAP_PRIV_CONSTR

mod0_wco_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(datas, panel.id = c("Area", "Year"))
) 

cum0_wco_f <- getcumul10_f(mod0_wco_f, datas, "stata_rob/vcov_w_constr.xlsx")


#### manuf

datas$x<-datas$S4
datas$y<-datas$GR_AAP_PRIV_MANUF

mod0_wma_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(datas, panel.id = c("Area", "Year"))
) 

cum0_wma_f <- getcumul10_f(mod0_wma_f, datas, "stata_rob/vcov_w_manuf.xlsx")



#### services

datas$x<-datas$S4
datas$y<-datas$GR_AAP_PRIV_SERV

mod0_wse_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(datas, panel.id = c("Area", "Year"))
) 

cum0_wse_f <- getcumul10_f(mod0_wse_f, datas, "stata_rob/vcov_w_serv.xlsx")


#### Charts ####

rbind(
  cum0_enm_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(type="emp", var="Agriculture, Forestry, Fishing,\nHunting and Mining"),
  cum0_eco_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(type="emp", var="Construction"),
  cum0_ema_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(type="emp", var="Manufacturing"),
  cum0_ese_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(type="emp", var="Services"),
  cum0_wnm_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(type="wage", var="Agriculture, Forestry, Fishing,\nHunting and Mining"),
  cum0_wco_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(type="wage", var="Construction"),
  cum0_wma_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(type="wage", var="Manufacturing"),
  cum0_wse_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(type="wage", var="Services")
) %>% 
  mutate(
    var = factor(
      var,
      levels = c(
        "Agriculture, Forestry, Fishing,\nHunting and Mining","Construction","Manufacturing","Services"),
      labels = c(
        "Agriculture, Forestry, Fishing,\nHunting and Mining","Construction","Manufacturing","Services"),
    )
  ) %>% 
  mutate(
    type = factor(
      type,
      levels = c("wage", "emp"),
      labels = c("Annnual Average Pay", "Annual Average Employment")
    )
  ) %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(aes(colour=var, linetype=type), size=1) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=var), linetype=5, alpha=0.25) +
  facet_grid(type~var, scales = "free_y") +
  xlab("Years since storm exposure") +
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_fill_manual(values = c("Total" = "black","Agriculture, Forestry, Fishing,\nHunting and Mining"="#74c476", "Construction"="#8c6bb1", "Manufacturing"="#41b6c4", "Services"="#fdae61")) +
  scale_colour_manual(values = c("Total"="black", "Agriculture, Forestry, Fishing,\nHunting and Mining"="#74c476", "Construction"="#8c6bb1", "Manufacturing"="#41b6c4", "Services"="#fdae61")) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = "none",
    legend.key.width = unit(1, 'cm'),
    axis.text=element_text(size=11),
    axis.title.x=element_text(size=14, hjust=0.5),
    axis.title.y = element_blank(),
    plot.margin = margin(0,5,0,40),
    legend.text = element_text(size=12),
    strip.text = element_text(size=12)
  ) -> g_tog

line_3 <- expression("%"~change~per~s.d.~of~S^(4))
cowplot::ggdraw(g_tog) + 
  cowplot::draw_label(line_3, x = 0.035, y = 0.5, angle=90) -> g_tog

ggsave("charts/employment_wage_sectors.pdf", g_tog, width=10.4, height = 5, scale=1.2)
