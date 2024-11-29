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

body(getcumul10_f)[[20]] <- substitute(lower <- coeffsums_sd - sdsums_sd * 1.645)
body(getcumul10_f)[[21]] <- substitute(upper <- coeffsums_sd + sdsums_sd * 1.645)

data %>% 
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


# Data manipulation #

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


#### Estimation ####


#dt_p
#wages 

dt_p$x<-dt_p$S4
dt_p$y<-dt_p$GR_AAP_TOT_TOT

mod_w1 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_p, panel.id = c("Area", "Year"))
) 
cum_w1 <- getcumul10_f(mod_w1, dt_p, "stata_rob/vcov_w_re_dt_p.xlsx")

#Income

dt_p$x<-dt_p$S4
dt_p$y<-dt_p$GR_INCOME_PC

mod_i1 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_p, panel.id = c("Area", "Year"))
) 
cum_i1 <- getcumul10_f(mod_i1, dt_p, "stata_rob/vcov_i_re_dt_p.xlsx")


#dt_wm
#wages 

dt_wm$x<-dt_wm$S4
dt_wm$y<-dt_wm$GR_AAP_TOT_TOT

mod_w2 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_wm, panel.id = c("Area", "Year"))
) 
cum_w2 <- getcumul10_f(mod_w2, dt_wm, "stata_rob/vcov_w_re_dt_wm.xlsx")

#Income

dt_wm$x<-dt_wm$S4
dt_wm$y<-dt_wm$GR_INCOME_PC

mod_i2 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_wm, panel.id = c("Area", "Year"))
) 
cum_i2 <- getcumul10_f(mod_i2, dt_wm, "stata_rob/vcov_i_re_dt_wm.xlsx")


#dt_wmo
#wages 

dt_wmo$x<-dt_wmo$S4
dt_wmo$y<-dt_wmo$GR_AAP_TOT_TOT

mod_w3 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_wmo, panel.id = c("Area", "Year"))
) 
cum_w3 <- getcumul10_f(mod_w3, dt_wmo, "stata_rob/vcov_w_re_dt_wmo.xlsx")

#Income

dt_wmo$x<-dt_wmo$S4
dt_wmo$y<-dt_wmo$GR_INCOME_PC

mod_i3 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_wmo, panel.id = c("Area", "Year"))
) 
cum_i3 <- getcumul10_f(mod_i3, dt_wmo, "stata_rob/vcov_i_re_dt_wmo.xlsx")


#dt_wt
#wages 

dt_wt$x<-dt_wt$S4
dt_wt$y<-dt_wt$GR_AAP_TOT_TOT

mod_w4 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_wt, panel.id = c("Area", "Year"))
) 
cum_w4 <- getcumul10_f(mod_w4, dt_wt, "stata_rob/vcov_w_re_dt_wt.xlsx")

#Income

dt_wt$x<-dt_wt$S4
dt_wt$y<-dt_wt$GR_INCOME_PC

mod_i4 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_wt, panel.id = c("Area", "Year"))
) 
cum_i4 <- getcumul10_f(mod_i4, dt_wt, "stata_rob/vcov_i_re_dt_wt.xlsx")


#### Plot ####

rbind(
  perc_wm %>% as_tibble %>% add_column(reg = "Threshold: mean\n(within-county)"),
  perc_p %>% as_tibble %>% add_column(reg = "Threshold: mean\n(pooled)"),
  perc_wmo %>% as_tibble %>% add_column(reg = "Threshold: mean\n(within-county, distinct)"),
  perc_wt %>% as_tibble %>% add_column(reg = "Threshold: top 75%\n(within-county)")
) %>% 
  mutate(
    value = paste0("Sample size ", round(value, 2)*100, "%\nof full sample"),
    reg = factor(
      reg,
      levels = c("Threshold: mean\n(within-county)", "Threshold: mean\n(pooled)",  "Threshold: mean\n(within-county, distinct)", "Threshold: top 75%\n(within-county)")
    )
  )-> data_perc


rbind(
  cum_w1 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Annual Average Pay") , 
  cum_i1 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Income per capita")
) -> data_gp

rbind(
  cum_w2 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Annual Average Pay") , 
  cum_i2 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Income per capita")
) -> data_gwm

rbind(
  cum_w3 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Annual Average Pay") , 
  cum_i3 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Income per capita")
) -> data_gwmo

rbind(
  cum_w4 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Annual Average Pay") , 
  cum_i4 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Income per capita")
) -> data_gwt


rbind(
  data_gwm %>% add_column(reg = "Threshold: mean\n(within-county)"),
  data_gp %>% add_column(reg = "Threshold: mean\n(pooled)"),
  data_gwmo %>% add_column(reg = "Threshold: mean\n(within-county, distinct)"),
  data_gwt %>% add_column(reg = "Threshold: top 75%\n(within-county)")
) %>% 
  mutate(
    reg = factor(
      reg,
      levels = c("Threshold: mean\n(within-county)", "Threshold: mean\n(pooled)", "Threshold: mean\n(within-county, distinct)", "Threshold: top 75%\n(within-county)")
    )
  ) %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(aes(colour=factor(var), linetype=factor(var)), linewidth=0.8) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(var)), alpha=0.25) +
  geom_label(data=data_perc, aes(x=2, y=0.33, label=value), size=3) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  facet_wrap(.~reg, nrow=2) +
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
    legend.key.width = unit(1, 'cm'),
    strip.background = element_blank()
  ) -> g_rep

ggsave("charts/rep_events.pdf", g_rep, width=8, height = 6, scale=1.2)

