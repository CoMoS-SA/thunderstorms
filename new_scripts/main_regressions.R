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
source("scripts/function_disasters_f.R")
data <- readRDS(file="data/f_panel_tr.rds")


data %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )

# by risk 

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

# by richness

data %<>% select(-quart)

data %>% 
  filter(Year==1991) %>% 
  mutate(
    quart = ntile(INCOME_PC, 4)
  ) %>% select(Area, quart) -> aux

data %<>% 
  left_join(.,aux, by="Area") %>% 
  mutate(tmm_rich=as.factor(case_when(quart==1~1, quart%in%c(2,3)~2, T~3)))


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

data_expl <- left_join(
  data, 
  data %>%
    select(STATE, time, Area, Year) %>% 
    mutate(STATE2=STATE, time2=time) %>% 
    pivot_wider(names_from = STATE2, values_from = time2, names_prefix = "state_trend_", values_fill = list(time2 = 0)) %>% 
    mutate(
      across(starts_with("state_trend_"), function(x){
        ifelse(STATE==gsub("state_trend_", "",cur_column()),time,0)
      })
    ) %>% 
    rename_with(~ ifelse(str_starts(.x, "state_trend_"), str_replace_all(.x, " ", "_"), .x)),
  by=c("Area", "Year")
)

mod_w0_expl <- fixest::feols(
  fml = as.formula(paste("y ~ l(x,0:10) + tmp + prec +", 
                         paste(names(data_expl) %>% grep("^state_trend_", ., value = TRUE), collapse = " + "), 
                         "| Area + Year")),
  data = fixest::panel(data_expl, panel.id = c("Area", "Year"))
) 


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

mod_i0_expl <- fixest::feols(
  fml = as.formula(paste("y ~ l(x,0:10) + tmp + prec +", 
                         paste(names(data_expl) %>% grep("^state_trend_", ., value = TRUE), collapse = " + "), 
                         "| Area + Year")),
  data = fixest::panel(data_expl, panel.id = c("Area", "Year"))
) 


#### Area Riskness Estimation ####

#Wages
data$x<-data$S4
data$y<-data$GR_AAP_TOT_TOT
data$tmm <- data$tmm_risk

mod_w_risk <- fixest::feols(
  fml = y ~ l(x,0:10)*tmm +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_w_risk)
cum_w_risk <- getcumul_t10_f(mod_w_risk, data, "stata_rob/vcov_risk.xlsx")

#Income

data$x<-data$S4
data$y<-data$GR_INCOME_PC
data$tmm <- data$tmm_risk

mod_i_risk <- fixest::feols(
  fml = y ~ l(x,0:10)*tmm +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_i_risk)
cum_i_risk <- getcumul_t10_f(mod_i_risk, data, "stata_rob/vcov_risk_income.xlsx")


#### Area Richness Estimation ####

#Wages
data$x<-data$S4
data$y<-data$GR_AAP_TOT_TOT
data$tmm <- data$tmm_rich

mod_w_rich <- fixest::feols(
  fml = y ~ l(x,0:10)*tmm +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_w_rich)
cum_w_rich <- getcumul_t10_f(mod_w_rich, data, "stata_rob/vcov_rich.xlsx")

#Income

data$x<-data$S4
data$y<-data$GR_INCOME_PC
data$tmm <- data$tmm_rich

mod_i_rich <- fixest::feols(
  fml = y ~ l(x,0:10)*tmm +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_i_rich)
cum_i_rich <- getcumul_t10_f(mod_i_rich, data, "stata_rob/vcov_rich_income.xlsx")


#### Disasters estimation ####
# IS_D
#wages
data$x<-data$S4
data$y<-data$GR_AAP_TOT_TOT
data$dmm <- data$IS_D

mod_w_isd <- fixest::feols(
  fml = y ~ l(x,0:10) + x:dmm + l(x,1):l(dmm,1) + l(x,2):l(dmm,2) + l(x,3):l(dmm,3) +
    l(x,4):l(dmm,4) + l(x,5):l(dmm,5) + l(x,6):l(dmm,6) +
    l(x,7):l(dmm,7) + l(x,8):l(dmm,8) + l(x,9):l(dmm,9) + l(x,10):l(dmm,10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_w_isd)
cum_w_isd <- getcumul_d10_dyn(mod_w_isd, data, "stata_rob/vcov_isd.xlsx")

#income
data$x<-data$S4
data$y<-data$GR_INCOME_PC
data$dmm <- data$IS_D

mod_i_isd <- fixest::feols(
  fml = y ~ l(x,0:10) + x:dmm + l(x,1):l(dmm,1) + l(x,2):l(dmm,2) + l(x,3):l(dmm,3) +
    l(x,4):l(dmm,4) + l(x,5):l(dmm,5) + l(x,6):l(dmm,6) +
    l(x,7):l(dmm,7) + l(x,8):l(dmm,8) + l(x,9):l(dmm,9) + l(x,10):l(dmm,10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_i_isd)
cum_i_isd <- getcumul_d10_dyn(mod_i_isd, data, "stata_rob/vcov_isd_income.xlsx")

# IS_D2
#wages
data$x<-data$S4
data$y<-data$GR_AAP_TOT_TOT
data$dmm <- data$IS_D2

mod_w_isd2 <- fixest::feols(
  fml = y ~ l(x,0:10) + x:dmm + l(x,1):l(dmm,1) + l(x,2):l(dmm,2) + l(x,3):l(dmm,3) +
    l(x,4):l(dmm,4) + l(x,5):l(dmm,5) + l(x,6):l(dmm,6) +
    l(x,7):l(dmm,7) + l(x,8):l(dmm,8) + l(x,9):l(dmm,9) + l(x,10):l(dmm,10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_w_isd2)
cum_w_isd2 <- getcumul_d10_dyn(mod_w_isd2, data, "stata_rob/vcov_isd2.xlsx")

#income
data$x<-data$S4
data$y<-data$GR_INCOME_PC
data$dmm <- data$IS_D2

mod_i_isd2 <- fixest::feols(
  fml = y ~ l(x,0:10) + x:dmm + l(x,1):l(dmm,1) + l(x,2):l(dmm,2) + l(x,3):l(dmm,3) +
    l(x,4):l(dmm,4) + l(x,5):l(dmm,5) + l(x,6):l(dmm,6) +
    l(x,7):l(dmm,7) + l(x,8):l(dmm,8) + l(x,9):l(dmm,9) + l(x,10):l(dmm,10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_i_isd2)
cum_i_isd2 <- getcumul_d10_dyn(mod_i_isd2, data, "stata_rob/vcov_isd2_income.xlsx")


# IS_D3
#wages
data$x<-data$S4
data$y<-data$GR_AAP_TOT_TOT
data$dmm <- data$IS_D3

mod_w_isd3 <- fixest::feols(
  fml = y ~ l(x,0:10) + x:dmm + l(x,1):l(dmm,1) + l(x,2):l(dmm,2) + l(x,3):l(dmm,3) +
    l(x,4):l(dmm,4) + l(x,5):l(dmm,5) + l(x,6):l(dmm,6) +
    l(x,7):l(dmm,7) + l(x,8):l(dmm,8) + l(x,9):l(dmm,9) + l(x,10):l(dmm,10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_w_isd3)
cum_w_isd3 <- getcumul_d10_dyn(mod_w_isd3, data, "stata_rob/vcov_isd3.xlsx")

#income
data$x<-data$S4
data$y<-data$GR_INCOME_PC
data$dmm <- data$IS_D3

mod_i_isd3 <- fixest::feols(
  fml = y ~ l(x,0:10) + x:dmm + l(x,1):l(dmm,1) + l(x,2):l(dmm,2) + l(x,3):l(dmm,3) +
    l(x,4):l(dmm,4) + l(x,5):l(dmm,5) + l(x,6):l(dmm,6) +
    l(x,7):l(dmm,7) + l(x,8):l(dmm,8) + l(x,9):l(dmm,9) + l(x,10):l(dmm,10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_i_isd3)
cum_i_isd3 <- getcumul_d10_dyn(mod_i_isd3, data, "stata_rob/vcov_isd3_income.xlsx")

# IS_D + ext 
#wages
data$x<-data$S4
data$y<-data$GR_AAP_TOT_TOT
data$dmm <- data$IS_D

mod_w_isd_ext <- fixest::feols(
  fml = y ~ l(x,0:10) + x:dmm + l(x,1):l(dmm,1) + l(x,2):l(dmm,2) + l(x,3):l(dmm,3) +
    l(x,4):l(dmm,4) + l(x,5):l(dmm,5) + l(x,6):l(dmm,6) +
    l(x,7):l(dmm,7) + l(x,8):l(dmm,8) + l(x,9):l(dmm,9) + l(x,10):l(dmm,10) +
    l(DISASTER_YEAR_EXT, 0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_w_isd_ext)
cum_w_isd_ext <- getcumul_d10_dyn(mod_w_isd_ext, data, "stata_rob/vcov_isd_ext.xlsx")

#income
data$x<-data$S4
data$y<-data$GR_INCOME_PC
data$dmm <- data$IS_D

mod_i_isd_ext <- fixest::feols(
  fml = y ~ l(x,0:10) + x:dmm + l(x,1):l(dmm,1) + l(x,2):l(dmm,2) + l(x,3):l(dmm,3) +
    l(x,4):l(dmm,4) + l(x,5):l(dmm,5) + l(x,6):l(dmm,6) +
    l(x,7):l(dmm,7) + l(x,8):l(dmm,8) + l(x,9):l(dmm,9) + l(x,10):l(dmm,10) +
    l(DISASTER_YEAR_EXT, 0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_i_isd_ext)
cum_i_isd_ext <- getcumul_d10_dyn(mod_i_isd_ext, data, "stata_rob/vcov_isd_ext_income.xlsx")


#### Charts ####

rbind(
  cum_w0 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Annual Average Pay") , 
  cum_i0 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Income per capita")
) %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(aes(colour=factor(var), linetype=factor(var)), linewidth=0.8) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(var)), alpha=0.25) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
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
  ) -> g1





cum_w_rich %>% 
  add_row(year=-1, coeffsums_sd=0, lower=0, upper=0, tmm=1) %>% 
  add_row(year=-1, coeffsums_sd=0, lower=0, upper=0, tmm=2) %>% 
  add_row(year=-1, coeffsums_sd=0, lower=0, upper=0, tmm=3) %>% 
  mutate(
    tmm = factor(
      tmm,
      levels = c(1, 2, 3),
      label = c("Poor", "Medium", "Rich")
    ),
    title = "Annual Average Pay"
  ) %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(aes(colour=factor(tmm), linetype=factor(tmm)), size=1) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(tmm)), linetype=5, alpha=0.25) +
  facet_wrap(~title) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_fill_manual(values = c("Rich"="#d7301f", "Medium"="#fdae61", "Poor"="#949496")) +
  scale_colour_manual(values = c("Rich"="#d7301f", "Medium"="#fdae61", "Poor"="#949496")) +
  scale_linetype_manual(values = c("Rich"=1, "Medium"=1, "Poor"=1)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    strip.background = element_blank(),
    legend.key.width = unit(1, 'cm'),
    axis.text=element_text(size=11),
    axis.title=element_text(size=14),
    legend.text = element_text(size=12),
    strip.text = element_text(size=12)
  ) -> g_w_rich

cum_i_rich %>% 
  add_row(year=-1, coeffsums_sd=0, lower=0, upper=0, tmm=1) %>% 
  add_row(year=-1, coeffsums_sd=0, lower=0, upper=0, tmm=2) %>% 
  add_row(year=-1, coeffsums_sd=0, lower=0, upper=0, tmm=3) %>% 
  mutate(
    tmm = factor(
      tmm,
      levels = c(1, 2, 3),
      label = c("Poor", "Medium", "Rich")
    ),
    title = "Income per capita"
  ) %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(aes(colour=factor(tmm), linetype=factor(tmm)), size=1) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(tmm)), linetype=5, alpha=0.25) +
  facet_wrap(~title) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_fill_manual(values = c("Rich"="#08519c", "Medium"="#41b6c4", "Poor"="#949496")) +
  scale_colour_manual(values = c("Rich"="#08519c", "Medium"="#41b6c4", "Poor"="#949496")) +
  scale_linetype_manual(values = c("Rich"=1, "Medium"=1, "Poor"=1)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "bottom",
    strip.background = element_blank(),
    legend.key.width = unit(1, 'cm'),
    axis.text=element_text(size=11),
    axis.title=element_text(size=14),
    legend.text = element_text(size=12),
    strip.text = element_text(size=12)
  ) -> g_i_rich

# Add distributions of cutting categories 
data %>% 
  filter(Year==1991) %>% 
  mutate(
    quart = ntile(INCOME_PC, 4)
  ) %>% select(Area, quart, INCOME_PC) -> aux

aux %>% 
  ggplot() + 
  geom_density(aes(x=INCOME_PC)) -> aux2

ggplot_build(aux2)$data[[1]][,c("x","y")] %>% 
  as_tibble() %>% 
  mutate(
    group=case_when(
      x <= quantile(aux$INCOME_PC, 0.25) ~ 1,
      x > quantile(aux$INCOME_PC, 0.75) ~ 3,
      T ~ 2
    )
  ) -> aux3

library("scales")
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

aux3 %>%  
  mutate(title=as.factor("Income per capita\n(2012$)")) %>% 
  ggplot() +
  geom_ribbon(data=aux3[aux3$group==1, ], aes(x=x, ymax=y, ymin=0), fill="#d9d9d9", alpha=0.7) +
  geom_ribbon(data=aux3[aux3$group==2, ], aes(x=x, ymax=y, ymin=0), fill="#737373", alpha=0.7) +
  geom_ribbon(data=aux3[aux3$group==3, ], aes(x=x, ymax=y, ymin=0), fill="#252525", alpha=0.7) +
  geom_line(data=aux3, aes(x=x, y=y), linewidth=0.3) + 
  geom_segment(aes(x=quantile(aux$INCOME_PC, 0.25), xend=quantile(aux$INCOME_PC, 0.25), y=0, yend=max(aux3[aux3$group==1,]$y)), linewidth=0.2) +
  geom_segment(aes(x=quantile(aux$INCOME_PC, 0.75), xend=quantile(aux$INCOME_PC, 0.75), y=0, yend=max(aux3[aux3$group==3,]$y)), linewidth=0.2) +
  geom_segment(data=aux3, aes(x=x[1], xend=x[1], y=0, yend=y[1]), linewidth=0.2) +
  annotate("text", x = 19500, y = 0.000018, label = "Poor", size=4) +
  annotate("text", x = 23500, y = 0.000027, label = "Medium", size=4) +
  annotate("text", x = 28000, y = 0.000018, label = "Rich", size=4) +
  facet_wrap(title~.)+
  scale_x_continuous(trans=log_trans(10), breaks = c(10000, 30000, 50000), labels = c("10k", "30k", "50k")) +
  coord_flip() +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(size=12)
  ) -> dens_inc

ggsave("charts/main_storms.pdf", g1, width=5.85, height = 4.1)

ggsave(paste0("charts/rich_storms.pdf"), 
       cowplot::plot_grid(g_w_rich, g_i_rich, dens_inc, nrow=1, labels = c('A','B', 'C'), rel_widths = c(0.42,0.42,0.16)),  
       width=11.8, height = 4.4)


# Disasters

cum_w_isd %>% 
  add_row(year=-1, coeffsums_sd=0, lower=0, upper=0, dmm=0) %>% 
  add_row(year=-1, coeffsums_sd=0, lower=0, upper=0, dmm=1) %>% 
  mutate(
    dmm = factor(
      dmm,
      levels = c(1, 0),
      label = c("FEMA intervention (\u2713)", "No FEMA intervention (\u2717)")
    ),
    title = "Annual Average Pay"
  ) %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(aes(colour=factor(dmm), linetype=factor(dmm)), size=1) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(dmm)), linetype=5, alpha=0.25) +
  facet_wrap(~title) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_fill_manual(values = c("No FEMA intervention (\u2717)"="#949496", "FEMA intervention (\u2713)"="#d7301f")) +
  scale_colour_manual(values = c("No FEMA intervention (\u2717)"="#949496", "FEMA intervention (\u2713)"="#d7301f")) +
  scale_linetype_manual(values = c("No FEMA intervention (\u2717)"=1, "FEMA intervention (\u2713)"=1)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.text = element_text(size=12, family = "Arial Unicode MS"),
    legend.position = "bottom",
    strip.background = element_blank(),
    legend.key.width = unit(1, 'cm'),
    axis.text=element_text(size=11),
    axis.title=element_text(size=14),
    strip.text = element_text(size=12)
  ) -> g_w_dis

cum_i_isd %>% 
  add_row(year=-1, coeffsums_sd=0, lower=0, upper=0, dmm=0) %>% 
  add_row(year=-1, coeffsums_sd=0, lower=0, upper=0, dmm=1) %>% 
  mutate(
    dmm = factor(
      dmm,
      levels = c(1, 0),
      label = c("FEMA intervention (\u2713)", "No FEMA intervention (\u2717)")
    ),
    title = "Income per capita"
  ) %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(aes(colour=factor(dmm), linetype=factor(dmm)), size=1) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(dmm)), linetype=5, alpha=0.25) +
  facet_wrap(~title) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_fill_manual(values = c("No FEMA intervention (\u2717)"="#949496", "FEMA intervention (\u2713)"="#08519c")) +
  scale_colour_manual(values = c("No FEMA intervention (\u2717)"="#949496", "FEMA intervention (\u2713)"="#08519c")) +
  scale_linetype_manual(values = c("No FEMA intervention (\u2717)"=1, "FEMA intervention (\u2713)"=1)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.text = element_text(size=12, family = "Arial Unicode MS"),
    legend.position = "bottom",
    strip.background = element_blank(),
    legend.key.width = unit(1, 'cm'),
    plot.tag.position = c(0.15, 0.02),
    axis.text=element_text(size=11),
    axis.title=element_text(size=14),
    strip.text = element_text(size=12)
  ) -> g_i_dis


dst <- 0.2

tibble(
  xmin = c(2,5,2,4,3,3,3,3),
  xmax = c(5,8,8,6,7,7,7,7),
  y = c(4-dst,3-dst,2-dst,1-dst, 4,3,2,1),
  type= c("disaster","disaster","disaster","disaster", "storm","storm","storm","storm"),
  gr = c("a","b","c","d","a","b","c","d"),
  text = rep(c("FEMA","NOAA"),each=4)
) %>% 
  mutate(
    up = ifelse(type=="storm", y + 0.2, y - 0.2)
  ) %>% 
  ggplot() + 
  geom_segment(aes(x=xmin, xend=xmax, y=y, yend=y, colour=type, group=gr, linetype=type)) +
  geom_point(aes(x=xmin, y=y, colour=type, group=gr)) +
  geom_point(aes(x=xmax, y=y, colour=type, group=gr)) +
  geom_segment(aes(x = 1.5, y = 0, xend = 8.5, yend = 0),
               arrow = arrow(length = unit(0.1, "cm")), size=0.4) + 
  geom_text(aes(x=(xmin + xmax)/2, y=up, label=text), size=3.7) +
  geom_text(data=tibble(x=rep(10.5,3),y=c(2,3,4)-(dst/2)), aes(x=x,y=y),label = c("\u2717","\u2717","\u2717"),  size=7, family = "Arial Unicode MS") +
  geom_text(data=tibble(x=10.5,y=1-(dst/2)), aes(x=x,y=y),label = c("\u2713"),  size=7, family = "Arial Unicode MS") +
  geom_text(data=tibble(x=11.2, y=4.75), aes(x=x, y=y), label="Is the storm classified\n under \"FEMA intervention\"?", size=3.5) +
  geom_text(data=tibble(x=5, y=4.75), aes(x=x, y=y), label="NOAA storm / FEMA disaster timing", size=3.5) +
  scale_colour_manual(values = c("disaster"="#737373", "storm"="black")) +
  scale_linetype_manual(values = c("disaster"=1, "storm"=2)) +
  geom_tile(data=tibble(x=rep(12,3), y=c(1.9,2.9,3.9)), aes(x=x, y=y), height=0.4, width=1, fill="#737373", alpha=0.25) +
  geom_segment(data=tibble(x=rep(12,3), y=c(1.9,2.9,3.9)), aes(x=x-0.3, y=y, xend=x+0.3, yend=y), color="#737373", size=1.2) +
  geom_tile(data=tibble(x=12, y=1), aes(x=x, y=y), height=0.2, width=1, fill="#d7301f", alpha=0.25) +
  geom_segment(data=tibble(x=12, y=1), aes(x=x-0.3, y=y, xend=x+0.3, yend=y), color="#d7301f", size=1.2) +
  geom_tile(data=tibble(x=12, y=0.8), aes(x=x, y=y), height=0.2, width=1, fill="#08519c", alpha=0.25) +
  geom_segment(data=tibble(x=12, y=0.8), aes(x=x-0.3, y=y, xend=x+0.3, yend=y), color="#08519c", size=1.2) +
  geom_point(aes(x=11, y=2), color="transparent") +
  labs(tag="Time") +
  theme_minimal() + 
  theme(
    legend.position = "none",
    panel.grid=element_blank(),
    axis.title = element_blank(),
    plot.tag = element_text(size=11),
    plot.tag.position = c(0.34, 0.025),
    axis.text = element_blank()
  ) -> g_ts_sep

quartz(type='pdf', file=paste0("charts/fema_imputation.pdf"), width=5.85, height = 4.1)
g_ts_sep
dev.off()

quartz(type='pdf', file=paste0("charts/disasters_storms.pdf"), width=11.8, height = 4.4)
cowplot::plot_grid(
  g_w_dis,
  g_i_dis,
  nrow=1, labels = c('A','B'), label_y = 1)
dev.off()



#### Tables ####

yy <- NULL
for(i in 0:10){
  new <- c(i, "")
  yy <- c(yy, new)
}

cbind(
  tablize_f(cum_w0, mod_w0),
  tablize_f(cum_i0, mod_i0) %>% select(-year)
) %>% 
  `colnames<-`(letters[1:3]) %>% 
  group_by(a) %>% 
  summarise(
    b=paste0(b, collapse = " "),
    c=paste0(c, collapse = " ")
  ) %>% 
  mutate(a=as.character(0:10)) %>% 
  add_row(a="County fixed effects", b="\\checkmark", c="\\checkmark") %>%
  add_row(a="Year fixed effects",   b="\\checkmark", c="\\checkmark") %>%
  add_row(a="State-level trends",   b="\\checkmark", c="\\checkmark") %>%
  add_row(a="Climatic controls",    b="\\checkmark", c="\\checkmark") %>%
  add_row(a="Adjusted $R^2$", 
          b=as.character(round(r2(mod_w0, type="ar2"),3)), 
          c=as.character(round(r2(mod_i0, type="ar2"),3))
  ) %>% 
  add_row(a="Observations", 
          b=as.character(nobs(mod_w0)), 
          c=as.character(nobs(mod_i0))
  ) %>% 
  add_row(a="Chi-test $t_s=0$ (p-value)", 
          b=as.character( ifelse(wald(mod_w0_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod_w0_expl, "^state_trend_", print=F)$p, 3))), 
          c=as.character( ifelse(wald(mod_i0_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod_i0_expl, "^state_trend_", print=F)$p, 3)))
  ) %>%
  `colnames<-`(c("Years since storm exposure", 
                 "Cumulative effect $\\hat{C}_{tau}$", 
                 "Cumulative effect $\\hat{C}_{tau}$"
                 )) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 6, hline_after = T) %>% 
  row_spec(10*2 + 8, hline_after = T) %>% 
  add_header_above(c(" " = 1, 
                     "Annual Average Pay" = 1, 
                     "Income per capita" = 1
  ), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., "tables/main_storm_reduced.tex")


# Risk

cbind(
  tablize_f(cum_w_risk, mod_w_risk, tmm=T),
  tablize_f(cum_i_risk, mod_i_risk, tmm=T) %>% select(-year) 
) %>% 
  `colnames<-`(c("a", "b", "c", "d", "e", "f", "g")) %>% 
  mutate(a=yy) %>% 
  add_row(a="County fixed effects", b=" ", c="\\checkmark", d=" ", e=" ", f="\\checkmark", g=" ") %>%
  add_row(a="Year fixed effects", b=" ", c="\\checkmark", d=" ", e=" ", f="\\checkmark", g=" ") %>%
  add_row(a="State-level trends", b=" ", c="\\checkmark", d=" ", e=" ", f="\\checkmark", g=" ") %>%
  add_row(a="Climatic controls", b=" ", c="\\checkmark", d=" ", e=" ", f="\\checkmark", g=" ") %>%
  add_row(a="Adjusted $R^2$", 
          b= " ",
          c=as.character(round(r2(mod_w_risk, type="ar2"),3)), 
          d= " ",
          e= " ",
          f=as.character(round(r2(mod_i_risk, type="ar2"),3)),
          g= " "
  ) %>% 
  add_row(a="Observations", 
          b= " ",
          c=as.character(nobs(mod_w_risk)), 
          d= " ",
          e= " ",
          f=as.character(nobs(mod_i_risk)),
          g= " "
  ) %>% 
  `colnames<-`(c("Years since storm exposure", "Low risk", "Medium risk", "High risk", "Low risk", "Medium risk", "High risk")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 6, hline_after = T) %>% 
  add_header_above(c(" " = 1, "Growth rate of Annual Average Pay" = 3, "Growth rate of Income per capita" = 3), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., paste0("tables/adaptation.tex"))



# Rich

cbind(
  tablize_f(cum_w_rich, mod_w_risk, tmm=T),
  tablize_f(cum_i_rich, mod_i_risk, tmm=T) %>% select(-year) 
) %>% 
  `colnames<-`(c("a", "b", "c", "d", "e", "f", "g")) %>% 
  mutate(a=yy) %>% 
  add_row(a="County fixed effects", b=" ", c="\\checkmark", d=" ", e=" ", f="\\checkmark", g=" ") %>%
  add_row(a="Year fixed effects", b=" ", c="\\checkmark", d=" ", e=" ", f="\\checkmark", g=" ") %>%
  add_row(a="State-level trends", b=" ", c="\\checkmark", d=" ", e=" ", f="\\checkmark", g=" ") %>%
  add_row(a="Climatic controls", b=" ", c="\\checkmark", d=" ", e=" ", f="\\checkmark", g=" ") %>%
  add_row(a="Adjusted $R^2$", 
          b= " ",
          c=as.character(round(r2(mod_w_rich, type="ar2"),3)), 
          d= " ",
          e= " ",
          f=as.character(round(r2(mod_i_rich, type="ar2"),3)),
          g= " "
  ) %>% 
  add_row(a="Observations", 
          b= " ",
          c=as.character(nobs(mod_w_rich)), 
          d= " ",
          e= " ",
          f=as.character(nobs(mod_i_rich)),
          g= " "
  ) %>% 
  `colnames<-`(c("Years since storm exposure", "Poor", "Medium", "Rich", "Poor", "Medium", "Rich")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 6, hline_after = T) %>% 
  add_header_above(c(" " = 1, "Growth rate of Annual Average Pay" = 3, "Growth rate of Income per capita" = 3), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., paste0("tables/rich.tex"))


#Disasters


cbind(
  tablize_f(cum_w_isd, mod_w_isd, dmm=T),
  tablize_f(cum_w_isd2, mod_w_isd2, dmm=T) %>% select(-year), 
  tablize_f(cum_w_isd3, mod_w_isd3, dmm=T) %>% select(-year), 
  tablize_f(cum_w_isd_ext, mod_w_isd_ext, dmm=T) %>% select(-year)
) %>% 
  `colnames<-`(c("a", "b", "c", "d", "e", "f", "g", "h", "i")) %>% 
  mutate(a=yy) %>% 
  add_row(a="County fixed effects", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark") %>%
  add_row(a="Year fixed effects",   b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark") %>%
  add_row(a="State-level trends",   b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark") %>%
  add_row(a="Climatic controls",    b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark") %>%
  add_row(a="Adjusted $R^2$", 
          b=as.character(round(r2(mod_w_isd, type="ar2"),3)), 
          c=as.character(round(r2(mod_w_isd, type="ar2"),3)),
          d=as.character(round(r2(mod_w_isd2, type="ar2"),3)), 
          e=as.character(round(r2(mod_w_isd2, type="ar2"),3)),
          f=as.character(round(r2(mod_w_isd3, type="ar2"),3)), 
          g=as.character(round(r2(mod_w_isd3, type="ar2"),3)),
          h=as.character(round(r2(mod_w_isd_ext, type="ar2"),3)), 
          i=as.character(round(r2(mod_w_isd_ext, type="ar2"),3))
  ) %>% 
  add_row(a="Observations", 
          b=as.character(nobs(mod_w_isd)), 
          c=as.character(nobs(mod_w_isd)),
          d=as.character(nobs(mod_w_isd2)), 
          e=as.character(nobs(mod_w_isd2)),
          f=as.character(nobs(mod_w_isd3)), 
          g=as.character(nobs(mod_w_isd3)),
          h=as.character(nobs(mod_w_isd_ext)), 
          i=as.character(nobs(mod_w_isd_ext))
  ) %>% 
  `colnames<-`(c("Years since storm exposure", "No FEMA intervention", "FEMA intervention", 
                 "No FEMA intervention", "FEMA intervention",
                 "No FEMA intervention", "FEMA intervention",
                 "No FEMA intervention", "FEMA intervention"
  )) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 6, hline_after = T) %>% 
  add_header_above(c(" " = 1, "Baseline" = 2, "With secondary" = 2, "With large"=2, "Baseline+ ext"=2), bold=T) %>% 
  add_header_above(c(" " = 1, "Annual Average Pay" = 8), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., paste0("tables/disasters_wage.tex")) #columns to be relabeded in manuscript


cbind(
  tablize_f(cum_i_isd, mod_i_isd, dmm=T),
  tablize_f(cum_i_isd2, mod_i_isd2, dmm=T) %>% select(-year), 
  tablize_f(cum_i_isd3, mod_i_isd3, dmm=T) %>% select(-year), 
  tablize_f(cum_i_isd_ext, mod_i_isd_ext, dmm=T) %>% select(-year)
) %>% 
  `colnames<-`(c("a", "b", "c", "d", "e", "f", "g", "h", "i")) %>% 
  mutate(a=yy) %>% 
  add_row(a="County fixed effects", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark") %>%
  add_row(a="Year fixed effects",   b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark") %>%
  add_row(a="State-level trends",   b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark") %>%
  add_row(a="Climatic controls",    b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark") %>%
  add_row(a="Adjusted $R^2$", 
          b=as.character(round(r2(mod_i_isd, type="ar2"),3)), 
          c=as.character(round(r2(mod_i_isd, type="ar2"),3)),
          d=as.character(round(r2(mod_i_isd2, type="ar2"),3)), 
          e=as.character(round(r2(mod_i_isd2, type="ar2"),3)),
          f=as.character(round(r2(mod_i_isd3, type="ar2"),3)), 
          g=as.character(round(r2(mod_i_isd3, type="ar2"),3)),
          h=as.character(round(r2(mod_i_isd_ext, type="ar2"),3)), 
          i=as.character(round(r2(mod_i_isd_ext, type="ar2"),3))
  ) %>% 
  add_row(a="Observations", 
          b=as.character(nobs(mod_i_isd)), 
          c=as.character(nobs(mod_i_isd)),
          d=as.character(nobs(mod_i_isd2)), 
          e=as.character(nobs(mod_i_isd2)),
          f=as.character(nobs(mod_i_isd3)), 
          g=as.character(nobs(mod_i_isd3)),
          h=as.character(nobs(mod_i_isd_ext)), 
          i=as.character(nobs(mod_i_isd_ext))
  ) %>% 
  `colnames<-`(c("Years since storm exposure", "No FEMA intervention", "FEMA intervention", 
                 "No FEMA intervention", "FEMA intervention",
                 "No FEMA intervention", "FEMA intervention",
                 "No FEMA intervention", "FEMA intervention"
  )) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 6, hline_after = T) %>% 
  add_header_above(c(" " = 1, "Baseline" = 2, "With secondary" = 2, "With large"=2, "Baseline+ ext"=2), bold=T) %>% 
  add_header_above(c(" " = 1, "Annual Average Pay" = 8), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., paste0("tables/disasters_income.tex")) #columns to be relabeded in manuscript

data %>% 
  as_tibble() %>% 
  filter(IS_D==1) %>% 
  nrow() -> a

data %>% 
  as_tibble() %>% 
  nrow() -> b

data %>% 
  as_tibble() %>% 
  filter(IS_D2==1) %>% 
  nrow() -> c

data %>% 
  as_tibble() %>% 
  filter(IS_D3==1) %>% 
  nrow() -> d

a
a/b
c
c/b
d
d/b
