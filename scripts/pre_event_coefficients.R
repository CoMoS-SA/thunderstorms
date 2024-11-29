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


data$x<-data$S4
data$y<-data$GR_AAP_TOT_TOT

mod_w0 <- fixest::feols(
  fml = y ~ l(x,0:10) + l(x,-4:-2) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 


data$x<-data$S4
data$y<-data$GR_INCOME_PC

mod_i0 <- fixest::feols(
  fml = y ~ l(x,0:10) + l(x,-4:-2) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 


rbind(
  rbind(
    get_leads(mod_w0, data, "stata_rob/vcov_w_pre_event.xlsx")[c(3,2,1),] %>% 
      add_row(coeffsums_sd=0, lower=0, upper=0),
    getcumul10_f(mod_w0, data, "stata_rob/vcov_w_pre_event.xlsx") %>% 
      select(coeffsums_sd, sdsums_sd, lower, upper)
  ) %>% 
    add_column(year=-4:10) %>% 
    add_column(var="Annual Average Pay"),
  
  rbind(
    get_leads(mod_i0, data, "stata_rob/vcov_i_pre_event.xlsx")[c(3,2,1),] %>% 
      add_row(coeffsums_sd=0, lower=0, upper=0),
    getcumul10_f(mod_i0, data, "stata_rob/vcov_i_pre_event.xlsx") %>% 
      select(coeffsums_sd, sdsums_sd, lower, upper)
  ) %>% 
    add_column(year=-4:10) %>% 
    add_column(var="Income per capita")
) -> data_g_lead


data_g_lead %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(data=. %>% filter(year>=-1), aes(colour=factor(var), linetype=factor(var)), linewidth=0.8) +
  geom_ribbon(data=. %>% filter(year>=-1), aes(ymin=lower, ymax=upper, fill=factor(var)), alpha=0.25) +
  geom_pointrange(data=. %>% filter(year< -1), aes(x=year, y=coeffsums_sd, ymin=lower, ymax=upper, colour=factor(var)), linewidth=0.4, size=0.3, position = position_dodge(width = 0.8), show.legend=FALSE) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = -1)+
  scale_x_continuous(breaks=c(-4,-2,0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_fill_manual(values = c("Annual Average Pay"="#d7301f", "Income per capita"="#08519c")) +
  scale_colour_manual(values = c("Annual Average Pay"="#d7301f", "Income per capita"="#08519c")) +
  scale_linetype_manual(values = c("Annual Average Pay"=1, "Income per capita"=2)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1, 'cm')
  ) -> g_pre_ev

ggsave("charts/pre_event.pdf", g_pre_ev, width=6.85, height = 4.1)




