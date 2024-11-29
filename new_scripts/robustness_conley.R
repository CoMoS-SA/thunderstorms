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

source("new_scripts/functions_f.R")
data <- readRDS(file="data/f_panel_tr.rds")

data %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )

#Baseline model wages
data$x<-data$S4
data$y<-data$GR_AAP_TOT_TOT

mod0_w_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

cum0_w_f <- getcumul10_f(mod0_w_f, data)
cum0_w_f_c50_0 <- getcumul10_f(mod0_w_f, data, "stata_rob/vcov_conley50_nolag.xlsx")
cum0_w_f_c50_1 <- getcumul10_f(mod0_w_f, data, "stata_rob/vcov_conley50_1lag.xlsx")
cum0_w_f_c50_2 <- getcumul10_f(mod0_w_f, data, "stata_rob/vcov_conley50_2lag.xlsx")
cum0_w_f_c50_5 <- getcumul10_f(mod0_w_f, data, "stata_rob/vcov_conley50_5lag.xlsx")
#
cum0_w_f_c100_0 <- getcumul10_f(mod0_w_f, data, "stata_rob/vcov_conley100_nolag.xlsx")
cum0_w_f_c100_1 <- getcumul10_f(mod0_w_f, data, "stata_rob/vcov_conley100_1lag.xlsx")
cum0_w_f_c100_2 <- getcumul10_f(mod0_w_f, data, "stata_rob/vcov_conley100_2lag.xlsx")
cum0_w_f_c100_5 <- getcumul10_f(mod0_w_f, data, "stata_rob/vcov_conley100_5lag.xlsx")
#
cum0_w_f_c150_1 <- getcumul10_f(mod0_w_f, data, "stata_rob/vcov_conley150_1lag.xlsx")

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

mod0_w_f_expl <- fixest::feols(
  fml = as.formula(paste("y ~ l(x,0:10) + tmp + prec +", 
                         paste(names(data_expl) %>% grep("^state_trend_", ., value = TRUE), collapse = " + "), 
                         "| Area + Year")),
  data = fixest::panel(data_expl, panel.id = c("Area", "Year"))
) 

#Baseline model income
data$x<-data$S4
data$y<-data$GR_INCOME_PC

mod0_i_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 


cum0_i_f <- getcumul10_f(mod0_i_f, data)
cum0_i_f_c50_0 <- getcumul10_f(mod0_i_f, data, "stata_rob/income_vcov_conley50_nolag.xlsx")
cum0_i_f_c50_1 <- getcumul10_f(mod0_i_f, data, "stata_rob/income_vcov_conley50_1lag.xlsx")
cum0_i_f_c50_2 <- getcumul10_f(mod0_i_f, data, "stata_rob/income_vcov_conley50_2lag.xlsx")
cum0_i_f_c50_5 <- getcumul10_f(mod0_i_f, data, "stata_rob/income_vcov_conley50_5lag.xlsx")
#
cum0_i_f_c100_0 <- getcumul10_f(mod0_i_f, data, "stata_rob/income_vcov_conley100_nolag.xlsx")
cum0_i_f_c100_1 <- getcumul10_f(mod0_i_f, data, "stata_rob/income_vcov_conley100_1lag.xlsx")
cum0_i_f_c100_2 <- getcumul10_f(mod0_i_f, data, "stata_rob/income_vcov_conley100_2lag.xlsx")
cum0_i_f_c100_5 <- getcumul10_f(mod0_i_f, data, "stata_rob/income_vcov_conley100_5lag.xlsx")
#
cum0_i_f_c150_1 <- getcumul10_f(mod0_i_f, data, "stata_rob/income_vcov_conley150_1lag.xlsx")


mod0_i_f_expl <- fixest::feols(
  fml = as.formula(paste("y ~ l(x,0:10) + tmp + prec +", 
                         paste(names(data_expl) %>% grep("^state_trend_", ., value = TRUE), collapse = " + "), 
                         "| Area + Year")),
  data = fixest::panel(data_expl, panel.id = c("Area", "Year"))
) 


#### Charts #####

# By distance

rbind(
  cum0_w_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Baseline"),
  cum0_w_f_c50_1 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 50 km (1 lag)"),
  cum0_w_f_c100_1 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 100 km (1 lag)"),
  cum0_w_f_c150_1 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 150 km (1 lag)")
) %>% 
  mutate(
    var = factor(
      var,
      levels = c("Baseline","Conley 50 km (1 lag)","Conley 100 km (1 lag)","Conley 150 km (1 lag)"),
      labels = c("Clustered (county)","Conley 50 km (1 lag)","Conley 100 km (1 lag)","Conley 150 km (1 lag)"),
    )
  ) %>% 
  mutate(title="Annual Average Pay") %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(linewidth=0.8) +
  geom_point() +
  geom_line(aes(y=lower, color=factor(var), linetype=factor(var))) +
  geom_line(aes(y=upper, color=factor(var), linetype=factor(var))) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  facet_wrap(.~title) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_colour_manual(values = c("Clustered (county)"="#d7301f", "Conley 50 km (1 lag)"="#feb24c", "Conley 100 km (1 lag)"="#969696", "Conley 150 km (1 lag)"="#df65b0")) +
  scale_linetype_manual(values = c("Clustered (county)"=1, "Conley 50 km (1 lag)"=2, "Conley 100 km (1 lag)"=3, "Conley 150 km (1 lag)"=4)) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1, 'cm'),
    strip.background =  element_rect(fill="white", color = "transparent"),
    axis.text=element_text(size=11),
    axis.title=element_text(size=14),
    strip.text = element_text(size=12),
    legend.text = element_text(size=12)
  ) -> conley_dist_w


rbind(
  cum0_i_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Baseline"),
  cum0_i_f_c50_1 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 50 km (1 lag)"),
  cum0_i_f_c100_1 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 100 km (1 lag)"),
  cum0_i_f_c150_1 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 150 km (1 lag)")
  ) %>% 
  mutate(
    var = factor(
      var,
      levels = c("Baseline","Conley 50 km (1 lag)","Conley 100 km (1 lag)","Conley 150 km (1 lag)"),
      labels = c("Clustered (county)","Conley 50 km (1 lag)","Conley 100 km (1 lag)","Conley 150 km (1 lag)"),
    )
  ) %>% 
  mutate(title="Income per capita") %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(linewidth=0.8) +
  geom_point() +
  geom_line(aes(y=lower, color=factor(var), linetype=factor(var))) +
  geom_line(aes(y=upper, color=factor(var), linetype=factor(var))) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  facet_wrap(.~title) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_colour_manual(values = c("Clustered (county)"="#08519c", "Conley 50 km (1 lag)"="#08919c", "Conley 100 km (1 lag)"="#969696", "Conley 150 km (1 lag)"="#41ab5d")) +
  scale_linetype_manual(values = c("Clustered (county)"=1, "Conley 50 km (1 lag)"=2, "Conley 100 km (1 lag)"=3, "Conley 150 km (1 lag)"=4)) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1, 'cm'),
    strip.background =  element_rect(fill="white", color = "transparent"),
    axis.text=element_text(size=11),
    axis.title=element_text(size=14),
    strip.text = element_text(size=12),
    legend.text = element_text(size=12)
  ) -> conley_dist_i


#By lag, 50

rbind(
  cum0_w_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Baseline"),
  cum0_w_f_c50_0 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 50 km (0 lag)"),
  cum0_w_f_c50_1 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 50 km (1 lag)"),
  cum0_w_f_c50_2 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 50 km (2 lag)"),
  cum0_w_f_c50_5 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 50 km (5 lag)")
) %>% 
  mutate(
    var = factor(
      var,
      levels = c("Baseline","Conley 50 km (0 lag)","Conley 50 km (1 lag)","Conley 50 km (2 lag)","Conley 50 km (5 lag)"),
      labels = c("Clustered (county)","Conley 50 km (0 lag)","Conley 50 km (1 lag)","Conley 50 km (2 lag)","Conley 50 km (5 lag)"),
    )
  ) %>% 
  mutate(title="Annual Average Pay") %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(linewidth=0.8) +
  geom_point() +
  geom_line(aes(y=lower, color=factor(var), linetype=factor(var))) +
  geom_line(aes(y=upper, color=factor(var), linetype=factor(var))) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  facet_wrap(.~title) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_colour_manual(values = c("Clustered (county)"="#d7301f","Conley 50 km (0 lag)"="#feb24c","Conley 50 km (1 lag)"="#969696","Conley 50 km (2 lag)"="#df65b0","Conley 50 km (5 lag)"="#7a0177")) +
  scale_linetype_manual(values = c("Clustered (county)"=1,"Conley 50 km (0 lag)"=2,"Conley 50 km (1 lag)"=3,"Conley 50 km (2 lag)"=4,"Conley 50 km (5 lag)"=5)) +
  guides(color=guide_legend(nrow=3,byrow=TRUE)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1, 'cm'),
    strip.background =  element_rect(fill="white", color = "transparent"),
    axis.text=element_text(size=11),
    axis.title=element_text(size=14),
    strip.text = element_text(size=12),
    legend.text = element_text(size=12)
  ) -> conley_lag_50_w



rbind(
  cum0_i_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Baseline"),
  cum0_i_f_c50_0 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 50 km (0 lag)"),
  cum0_i_f_c50_1 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 50 km (1 lag)"),
  cum0_i_f_c50_2 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 50 km (2 lag)"),
  cum0_i_f_c50_5 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 50 km (5 lag)")
) %>% 
  mutate(
    var = factor(
      var,
      levels = c("Baseline","Conley 50 km (0 lag)","Conley 50 km (1 lag)","Conley 50 km (2 lag)","Conley 50 km (5 lag)"),
      labels = c("Clustered (county)","Conley 50 km (0 lag)","Conley 50 km (1 lag)","Conley 50 km (2 lag)","Conley 50 km (5 lag)"),
    )
  ) %>% 
  mutate(title="Income per capita") %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(linewidth=0.8) +
  geom_point() +
  geom_line(aes(y=lower, color=factor(var), linetype=factor(var))) +
  geom_line(aes(y=upper, color=factor(var), linetype=factor(var))) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  facet_wrap(.~title) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_colour_manual(values = c("Clustered (county)"="#08519c","Conley 50 km (0 lag)"="#08919c","Conley 50 km (1 lag)"="#969696","Conley 50 km (2 lag)"="#41ab5d","Conley 50 km (5 lag)"="#00441b")) +
  scale_linetype_manual(values = c("Clustered (county)"=1,"Conley 50 km (0 lag)"=2,"Conley 50 km (1 lag)"=3,"Conley 50 km (2 lag)"=4,"Conley 50 km (5 lag)"=5)) +
  guides(color=guide_legend(nrow=3,byrow=TRUE)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1, 'cm'),
    strip.background =  element_rect(fill="white", color = "transparent"),
    axis.text=element_text(size=11),
    axis.title=element_text(size=14),
    strip.text = element_text(size=12),
    legend.text = element_text(size=12)
  ) -> conley_lag_50_i

#By lag, 100

rbind(
  cum0_w_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Baseline"),
  cum0_w_f_c100_0 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 100 km (0 lag)"),
  cum0_w_f_c100_1 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 100 km (1 lag)"),
  cum0_w_f_c100_2 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 100 km (2 lag)"),
  cum0_w_f_c100_5 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 100 km (5 lag)")
) %>% 
  mutate(
    var = factor(
      var,
      levels = c("Baseline","Conley 100 km (0 lag)","Conley 100 km (1 lag)","Conley 100 km (2 lag)","Conley 100 km (5 lag)"),
      labels = c("Clustered (county)","Conley 100 km (0 lag)","Conley 100 km (1 lag)","Conley 100 km (2 lag)","Conley 100 km (5 lag)"),
    )
  ) %>% 
  mutate(title="Annual Average Pay") %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(linewidth=0.8) +
  geom_point() +
  geom_line(aes(y=lower, color=factor(var), linetype=factor(var))) +
  geom_line(aes(y=upper, color=factor(var), linetype=factor(var))) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  facet_wrap(.~title) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_colour_manual(values = c("Clustered (county)"="#d7301f","Conley 100 km (0 lag)"="#feb24c","Conley 100 km (1 lag)"="#969696","Conley 100 km (2 lag)"="#df65b0","Conley 100 km (5 lag)"="#7a0177")) +
  scale_linetype_manual(values = c("Clustered (county)"=1,"Conley 100 km (0 lag)"=2,"Conley 100 km (1 lag)"=3,"Conley 100 km (2 lag)"=4,"Conley 100 km (5 lag)"=5)) +
  guides(color=guide_legend(nrow=3,byrow=TRUE)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1, 'cm'),
    strip.background =  element_rect(fill="white", color = "transparent"),
    axis.text=element_text(size=11),
    axis.title=element_text(size=14),
    strip.text = element_text(size=12),
    legend.text = element_text(size=12)
  ) -> conley_lag_100_w



rbind(
  cum0_i_f %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Baseline"),
  cum0_i_f_c50_0 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 100 km (0 lag)"),
  cum0_i_f_c50_1 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 100 km (1 lag)"),
  cum0_i_f_c50_2 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 100 km (2 lag)"),
  cum0_i_f_c50_5 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Conley 100 km (5 lag)")
) %>% 
  mutate(
    var = factor(
      var,
      levels = c("Baseline","Conley 100 km (0 lag)","Conley 100 km (1 lag)","Conley 100 km (2 lag)","Conley 100 km (5 lag)"),
      labels = c("Clustered (county)","Conley 100 km (0 lag)","Conley 100 km (1 lag)","Conley 100 km (2 lag)","Conley 100 km (5 lag)"),
    )
  ) %>% 
  mutate(title="Income per capita") %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(linewidth=0.8) +
  geom_point() +
  geom_line(aes(y=lower, color=factor(var), linetype=factor(var))) +
  geom_line(aes(y=upper, color=factor(var), linetype=factor(var))) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  facet_wrap(.~title) +
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_colour_manual(values = c("Clustered (county)"="#08519c","Conley 100 km (0 lag)"="#08919c","Conley 100 km (1 lag)"="#969696","Conley 100 km (2 lag)"="#41ab5d","Conley 100 km (5 lag)"="#00441b")) +
  scale_linetype_manual(values = c("Clustered (county)"=1,"Conley 100 km (0 lag)"=2,"Conley 100 km (1 lag)"=3,"Conley 100 km (2 lag)"=4,"Conley 100 km (5 lag)"=5)) +
  guides(color=guide_legend(nrow=3,byrow=TRUE)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1, 'cm'),
    strip.background =  element_rect(fill="white", color = "transparent"),
    axis.text=element_text(size=11),
    axis.title=element_text(size=14),
    strip.text = element_text(size=12),
    legend.text = element_text(size=12)
  ) -> conley_lag_100_i




conley_dist <- cowplot::plot_grid(conley_dist_w, conley_dist_i, nrow=1, labels = c('A','B'))
conley_lag_50 <- cowplot::plot_grid(conley_lag_50_w, conley_lag_50_i, nrow=1, labels = c('A','B'))
conley_lag_100 <- cowplot::plot_grid(conley_lag_100_w, conley_lag_100_i, nrow=1, labels = c('A','B'))

ggsave("charts/conley_dist.pdf", conley_dist, width=11, height = 4.2, scale=1)
ggsave("charts/conley_lag_50.pdf", conley_lag_50, width=11, height = 4.2, scale=1)
ggsave("charts/conley_lag_100.pdf", conley_lag_100, width=11, height = 4.2, scale=1)


#### Tables ####

yy <- NULL
for(i in 0:10){
  new <- c(i, "")
  yy <- c(yy, new)
}

cbind(
  tablize_f(cum0_w_f, mod0_w_f),
  tablize_f(cum0_w_f_c50_0, mod0_w_f) %>% select(-year),
  tablize_f(cum0_w_f_c50_1, mod0_w_f) %>% select(-year),
  tablize_f(cum0_w_f_c50_2, mod0_w_f) %>% select(-year), 
  tablize_f(cum0_w_f_c50_5, mod0_w_f) %>% select(-year),
  tablize_f(cum0_w_f_c100_0, mod0_w_f) %>% select(-year),
  tablize_f(cum0_w_f_c100_1, mod0_w_f) %>% select(-year),
  tablize_f(cum0_w_f_c100_2, mod0_w_f) %>% select(-year),
  tablize_f(cum0_w_f_c100_5, mod0_w_f) %>% select(-year),
  tablize_f(cum0_w_f_c150_1, mod0_w_f) %>% select(-year)
) %>% 
  `colnames<-`(letters[1:11]) %>% 
  mutate(a=yy) %>% 
  add_row(a="County fixed effects", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark", j="\\checkmark", k="\\checkmark") %>%
  add_row(a="Year fixed effects",   b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark", j="\\checkmark", k="\\checkmark") %>%
  add_row(a="State-level trends",   b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark", j="\\checkmark", k="\\checkmark") %>%
  add_row(a="Climatic controls",    b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark", j="\\checkmark", k="\\checkmark") %>%
  add_row(a="Adjusted $R^2$", 
          b=as.character(round(r2(mod0_w_f, type="ar2"),3)), 
          c=as.character(round(r2(mod0_w_f, type="ar2"),3)),
          d=as.character(round(r2(mod0_w_f, type="ar2"),3)),
          e=as.character(round(r2(mod0_w_f, type="ar2"),3)),
          f=as.character(round(r2(mod0_w_f, type="ar2"),3)),
          g=as.character(round(r2(mod0_w_f, type="ar2"),3)),
          h=as.character(round(r2(mod0_w_f, type="ar2"),3)),
          i=as.character(round(r2(mod0_w_f, type="ar2"),3)),
          j=as.character(round(r2(mod0_w_f, type="ar2"),3)),
          k=as.character(round(r2(mod0_w_f, type="ar2"),3))
  ) %>% 
  add_row(a="Observations", 
          b=as.character(nobs(mod0_w_f)), 
          c=as.character(nobs(mod0_w_f)),
          d=as.character(nobs(mod0_w_f)),
          e=as.character(nobs(mod0_w_f)),
          f=as.character(nobs(mod0_w_f)),
          g=as.character(nobs(mod0_w_f)),
          h=as.character(nobs(mod0_w_f)),
          i=as.character(nobs(mod0_w_f)),
          j=as.character(nobs(mod0_w_f)),
          k=as.character(nobs(mod0_w_f))
  ) %>% 
  add_row(a="Chi-test $t_s=0$ (p-value)", 
          b=as.character( ifelse(wald(mod0_w_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))), 
          c=as.character( ifelse(wald(mod0_w_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          d=as.character( ifelse(wald(mod0_w_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          e=as.character( ifelse(wald(mod0_w_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          f=as.character( ifelse(wald(mod0_w_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          g=as.character( ifelse(wald(mod0_w_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          h=as.character( ifelse(wald(mod0_w_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          i=as.character( ifelse(wald(mod0_w_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          j=as.character( ifelse(wald(mod0_w_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          k=as.character( ifelse(wald(mod0_w_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3)))
  ) %>%
  `colnames<-`(c("Years since storm exposure", 
                 " ", 
                 "0L","1L","2L","3L",
                 "0L","1L","2L","3L",
                 "1L")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 6, hline_after = T) %>% 
  row_spec(10*2 + 8, hline_after = T) %>% 
  add_header_above(c(" " = 1, 
                     "Clustered (county)" = 1, 
                     "50 km" = 4,
                     "100 km" = 4,
                     "150 km" = 1
  )) %>% 
  add_header_above(c(" " = 1, 
                     "Annual Average Pay" = 10
  ), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., "tables/wages_conley_rob.tex")


cbind(
  tablize_f(cum0_i_f, mod0_i_f),
  tablize_f(cum0_i_f_c50_0, mod0_i_f) %>% select(-year),
  tablize_f(cum0_i_f_c50_1, mod0_i_f) %>% select(-year),
  tablize_f(cum0_i_f_c50_2, mod0_i_f) %>% select(-year), 
  tablize_f(cum0_i_f_c50_5, mod0_i_f) %>% select(-year),
  tablize_f(cum0_i_f_c100_0, mod0_i_f) %>% select(-year),
  tablize_f(cum0_i_f_c100_1, mod0_i_f) %>% select(-year),
  tablize_f(cum0_i_f_c100_2, mod0_i_f) %>% select(-year),
  tablize_f(cum0_i_f_c100_5, mod0_i_f) %>% select(-year),
  tablize_f(cum0_i_f_c150_1, mod0_i_f) %>% select(-year)
) %>% 
  `colnames<-`(letters[1:11]) %>% 
  mutate(a=yy) %>% 
  add_row(a="County fixed effects", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark", j="\\checkmark", k="\\checkmark") %>%
  add_row(a="Year fixed effects",   b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark", j="\\checkmark", k="\\checkmark") %>%
  add_row(a="State-level trends",   b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark", j="\\checkmark", k="\\checkmark") %>%
  add_row(a="Climatic controls",    b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark", i="\\checkmark", j="\\checkmark", k="\\checkmark") %>%
  add_row(a="Adjusted $R^2$", 
          b=as.character(round(r2(mod0_i_f, type="ar2"),3)), 
          c=as.character(round(r2(mod0_i_f, type="ar2"),3)),
          d=as.character(round(r2(mod0_i_f, type="ar2"),3)),
          e=as.character(round(r2(mod0_i_f, type="ar2"),3)),
          f=as.character(round(r2(mod0_i_f, type="ar2"),3)),
          g=as.character(round(r2(mod0_i_f, type="ar2"),3)),
          h=as.character(round(r2(mod0_i_f, type="ar2"),3)),
          i=as.character(round(r2(mod0_i_f, type="ar2"),3)),
          j=as.character(round(r2(mod0_i_f, type="ar2"),3)),
          k=as.character(round(r2(mod0_i_f, type="ar2"),3))
  ) %>% 
  add_row(a="Observations", 
          b=as.character(nobs(mod0_i_f)), 
          c=as.character(nobs(mod0_i_f)),
          d=as.character(nobs(mod0_i_f)),
          e=as.character(nobs(mod0_i_f)),
          f=as.character(nobs(mod0_i_f)),
          g=as.character(nobs(mod0_i_f)),
          h=as.character(nobs(mod0_i_f)),
          i=as.character(nobs(mod0_i_f)),
          j=as.character(nobs(mod0_i_f)),
          k=as.character(nobs(mod0_i_f))
  ) %>% 
  add_row(a="Chi-test $t_s=0$ (p-value)", 
          b=as.character( ifelse(wald(mod0_i_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))), 
          c=as.character( ifelse(wald(mod0_i_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          d=as.character( ifelse(wald(mod0_i_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          e=as.character( ifelse(wald(mod0_i_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          f=as.character( ifelse(wald(mod0_i_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          g=as.character( ifelse(wald(mod0_i_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          h=as.character( ifelse(wald(mod0_i_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          i=as.character( ifelse(wald(mod0_i_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          j=as.character( ifelse(wald(mod0_i_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3))),
          k=as.character( ifelse(wald(mod0_i_f_expl, "^state_trend_", print=F)$p <0.001, "$<0.001$", round(wald(mod0_w_f_expl, "^state_trend_", print=F)$p, 3)))
  ) %>%
  `colnames<-`(c("Years since storm exposure", 
                 " ", 
                 "0L","1L","2L","3L",
                 "0L","1L","2L","3L",
                 "1L")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 6, hline_after = T) %>% 
  row_spec(10*2 + 8, hline_after = T) %>% 
  add_header_above(c(" " = 1, 
                     "Clustered (county)" = 1, 
                     "50 km" = 4,
                     "100 km" = 4,
                     "150 km" = 1
  )) %>% 
  add_header_above(c(" " = 1, 
                     "Income per capita" = 10
  ), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., "tables/income_conley_rob.tex")




  
  
  
