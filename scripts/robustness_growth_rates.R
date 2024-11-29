library(tidyverse)
library(magrittr)
library(plm)
library(stargazer)
library(ggplot2)
library(grDevices)
library(knitr)
library(kableExtra)


rm(list = ls())
source("scripts/functions_f.R")

data <- readRDS(file="data/f_panel_tr.rds")


data %>% 
  as_tibble() %>% 
  mutate(Year=as.numeric(as.character(Year))) %>% 
  arrange(Area, Year) %>% 
  group_by(Area) %>% 
  summarise(
    income_rho_1 = aTSA::pp.test(GR_INCOME_PC, type="Z_rho", output=F)["type 1", "p.value"],
    income_rho_2 = aTSA::pp.test(GR_INCOME_PC, type="Z_rho", output=F)["type 2", "p.value"],
    income_tau_1 = aTSA::pp.test(GR_INCOME_PC, type="Z_tau", output=F)["type 1", "p.value"],
    income_tau_2 = aTSA::pp.test(GR_INCOME_PC, type="Z_tau", output=F)["type 2", "p.value"],
    wage_rho_1 = aTSA::pp.test(GR_AAP_TOT_TOT, type="Z_rho", output=F)["type 1", "p.value"],
    wage_rho_2 = aTSA::pp.test(GR_AAP_TOT_TOT, type="Z_rho", output=F)["type 2", "p.value"],
    wage_tau_1 = aTSA::pp.test(GR_AAP_TOT_TOT, type="Z_tau", output=F)["type 1", "p.value"],
    wage_tau_2 = aTSA::pp.test(GR_AAP_TOT_TOT, type="Z_tau", output=F)["type 2", "p.value"]
  ) -> pp_data


pp_data %>% 
  summarise(
    across(c(income_rho_1:wage_tau_2), ~ (sum(.x<=0.05))/n() )
  ) 



data %>% 
  as_tibble() %>% 
  mutate(Year=as.numeric(as.character(Year))) %>% 
  arrange(Area, Year) %>% 
  group_by(Area) %>% 
  summarise(
    income_mk = Kendall::MannKendall(GR_INCOME_PC)$sl,
    wage_mk =   Kendall::MannKendall(GR_AAP_TOT_TOT)$sl,
    income_mmk_ar = funtimes::notrend_test(GR_INCOME_PC, test = "MK", B=2000)$p.value ,#["P-value"],
    wage_mmk_ar = funtimes::notrend_test(GR_AAP_TOT_TOT, test = "MK", B=2000)$p.value #["P-value"],
  ) -> kendall_data

kendall_data %>% 
  summarise(
    across(c(income_mk:wage_mmk_ar), ~ (sum(.x<0.05))/n() )
  ) 


data %>% 
  as_tibble() %>% 
  mutate(time=as.numeric(as.character(Year))) %>% 
  group_by(Area) %>% 
  summarise(
    beta_inc = lm(GR_INCOME_PC~time)$coefficients["time"],
    p_inc = summary(lm(GR_INCOME_PC~time))$coefficients["time","Pr(>|t|)"],
    beta_wage = lm(GR_AAP_TOT_TOT~time)$coefficients["time"],
    p_wage = summary(lm(GR_AAP_TOT_TOT~time))$coefficients["time","Pr(>|t|)"],
  ) -> data_regs

data_regs %>% 
  summarise(
    across(c(p_inc,p_wage), ~ (sum(.x<0.05))/n() )
  ) 


data %>% 
  summarise(
    GR_INCOME_PC = mean(GR_INCOME_PC),
    GR_AAP_TOT_TOT = mean(GR_AAP_TOT_TOT)
  ) %>% 
  pivot_longer(GR_INCOME_PC:GR_AAP_TOT_TOT) %>% 
  mutate(
    name = factor(name, levels = c("GR_AAP_TOT_TOT", "GR_INCOME_PC"),
                  labels = c("Annual Average Pay", "Income per capita"))
  )-> means



cbind(
  pp_data %>% 
    summarise(
      across(c(income_rho_1, income_rho_2), ~ (sum(.x<=0.05))/n() )
    ),
  kendall_data %>% 
    summarise(
      across(c(income_mk,income_mmk_ar), ~ (sum(.x>0.05))/n() )
    ),
  data_regs %>% 
    summarise(
      across(c(p_inc), ~ (sum(.x>0.05))/n() )
    ) 
) %>% 
  mutate(across(everything(), ~ paste0(round(.x*100,2), "%") )) %>% 
  `colnames<-`(c("Phillips-Perron\n(no constant)", "Phillips-Perron\n(with constant)",
                 "Mann-Kendall", "Mann-Kendall\n(Bootstrapped)", "Linear Model\n(T-test)")) -> tab_income

ggpubr::ggtexttable(
  tab_income,
  rows = NULL,
  theme = ggpubr::ttheme(
    base_size = 11,
    rownames.style = ggpubr::rownames_style(
      fill = c("white", "grey80"),
      hjust=1.3,
      parse = T
    ),
  )
) -> t_inc

cbind(
  pp_data %>% 
    summarise(
      across(c( wage_rho_1, wage_rho_2), ~ (sum(.x<=0.05))/n() )
    ),
  kendall_data %>% 
    summarise(
      across(c(wage_mk,wage_mmk_ar), ~ (sum(.x>0.05))/n() )
    ),
  data_regs %>% 
    summarise(
      across(c(p_wage), ~ (sum(.x>0.05))/n() )
    ) 
) %>% 
  mutate(across(everything(), ~ paste0(round(.x*100,2), "%") )) %>% 
  `colnames<-`(c("Phillips-Perron\n(no constant)", "Phillips-Perron\n(with constant)",
                 "Mann-Kendall", "Mann-Kendall\n(Bootstrapped)", "Linear Model\n(T-test)")) -> tab_wage

ggpubr::ggtexttable(
  tab_wage,
  rows = NULL,
  theme = ggpubr::ttheme(
    base_size = 11,
    rownames.style = ggpubr::rownames_style(
      fill = c("white", "grey80"),
      hjust=1.3,
      parse = T
    ),
  )
) -> t_wage


data %>% 
  as_tibble() %>% 
  mutate(time=as.numeric(as.character(Year))) %>% 
  arrange(Area, time) %>% 
  select(Area, time, GR_INCOME_PC, GR_AAP_TOT_TOT) %>% 
  pivot_longer(GR_INCOME_PC:GR_AAP_TOT_TOT) %>% 
  mutate(
    name = factor(name, levels = c("GR_AAP_TOT_TOT", "GR_INCOME_PC"),
                  labels = c("Annual Average Pay", "Income per capita"))
  ) %>% 
  ggplot() + 
  geom_line(aes(x=time, y=value, group=Area), linewidth=0.0001, color="grey40") +
  geom_hline(data=means, aes(yintercept=value), color="indianred", linetype=2, linewidth=0.5) +
  scale_x_continuous(breaks = c(1991, 2000, 2010, 2019), expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  facet_grid(.~name, scales = "free_y") +
  theme_bw() + 
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(2.4, "lines"),
    axis.title = element_blank(),
    text = element_text(size=18),
    plot.margin = unit(c(2,10,2,2), "mm")
  ) -> g_rates


cowplot::plot_grid(
  g_rates,
  cowplot::plot_grid(
    t_wage + theme(plot.margin=grid::unit(c(0,0,0,13), "mm")),
    t_inc + theme(plot.margin=grid::unit(c(0,0,0,0), "mm")),
    nrow=1
  ),
  nrow=2,
  rel_heights = c(0.8,0.2)
) -> final

#Phillips-Perron indicates the percentage of county for which we reject the null hypothesis of unit root.
#Mann Kendall indicates the percentage of counties for which we cannot reject the null hypothesis of no-trend.
#Linear model indicates the percentage of counties for which we cannot reject the null hypothesis of no-trend
#t-test on time regressor. All significance levels are at 5%. 


data %>% 
  as_tibble() %>% 
  mutate(time=as.numeric(as.character(Year))) %>% 
  arrange(Area, time) %>% 
  select(Area, time, GR_INCOME_PC, GR_AAP_TOT_TOT) %>% 
  pivot_longer(GR_INCOME_PC:GR_AAP_TOT_TOT) %>% 
  mutate(
    name = factor(name, levels = c("GR_AAP_TOT_TOT", "GR_INCOME_PC"),
                  labels = c("Annual Average Pay", "Income per capita"))
  ) %>% 
  group_by(name) %>% 
  summarise(
    'Sample Mean' = round(mean(value),2),
    'Sample Standard\nDeviation' = round(sd(value),2)
  ) -> tab_mv

tab_mv %>% 
  slice(1) %>% 
  select(-name) %>% 
  ggpubr::ggtexttable(
    .,
    rows = NULL,
    theme = ggpubr::ttheme(
      base_size = 11,
      tbody.style = ggpubr::tbody_style(fill="grey95"),
      rownames.style = ggpubr::rownames_style(
        fill = c("white", "white"),
        hjust=1.3,
        parse = T
      ),
    )
  ) -> tab_wage

tab_mv %>% 
  slice(2) %>% 
  select(-name) %>% 
  ggpubr::ggtexttable(
    .,
    rows = NULL,
    theme = ggpubr::ttheme(
      base_size = 11,
      tbody.style = ggpubr::tbody_style(fill="grey95"),
      rownames.style = ggpubr::rownames_style(
        fill = c("white", "white"),
        hjust=1.3,
        parse = T
      ),
    )
  ) -> tab_inc


cowplot::plot_grid(
  g_rates,
  cowplot::plot_grid(
    tab_wage + theme(plot.margin=grid::unit(c(0,0,0,13), "mm")),
    tab_inc + theme(plot.margin=grid::unit(c(0,0,0,0), "mm")),
    nrow=1
  ),
  cowplot::plot_grid(
    t_wage + theme(plot.margin=grid::unit(c(0,0,0,13), "mm")),
    t_inc + theme(plot.margin=grid::unit(c(0,0,0,0), "mm")),
    nrow=1
  ),
  nrow=3,
  rel_heights = c(0.68,0.16,0.16)
) -> final2

ggsave(paste0("charts/growth_rates_base.pdf"), final2, width=11, height = 5.2, scale = 1.2)

