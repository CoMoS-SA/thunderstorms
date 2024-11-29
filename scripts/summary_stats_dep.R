library(tidyverse)
library(magrittr)
library(plm)
library(stargazer)
library(ggplot2)
library(grDevices)
library(knitr)
library(kableExtra)
library(fixest)


rm(list = ls())


data <- readRDS(file="data/f_panel_tr.rds")

emp_data <- read_csv("data/CAINC4/CAINC4__ALL_AREAS_1969_2021.csv", 
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


emp_data %<>% 
  select(-c("1969":"1990")) %>% 
  filter(Description=="Total employment") %>% 
  select(GeoFIPS, Description, "1991":"2019") %>% 
  pivot_longer(cols ="1991":"2019", names_to = "Year") %>% 
  select(-Description) %>% 
  rename(emp=value) 


left_join(
  data,
  emp_data,
  by=c("FIPS"="GeoFIPS", "Year"="Year")
) -> data


data %<>% 
  mutate(
    income_emp = (INCOME*1000)/emp,
    time = as.numeric(Year)
  )


##### Together ####

data %>% 
  group_by(time) %>% 
  summarise(
    mean_aap= mean(AAP_TOT_TOT),
    mean_inpc= mean(income_emp),
  ) %>%
  mutate(
    index_aap = mean_aap/mean_aap[time==1991],
    index_inpc = mean_inpc/mean_inpc[time==1991],
    var = "Average rate of change (1991=1)"
  ) %>% 
  select(-mean_aap, -mean_inpc) %>% 
  pivot_longer(cols = c(index_aap, index_inpc)) %>% 
  mutate(name=factor(name, levels = c("index_aap", "index_inpc"), labels = c("Annual Average Pay", "Income per Employee"))) %>% 
  ggplot() + 
  geom_line(aes(x=time, y=value, color=name)) + 
  facet_wrap(var~.) +
  xlab("Year") +
  scale_x_continuous(breaks=c(1991, 2000, 2010, 2019)) +
  #ylab(expression("% change per"~s.d.~of~S^(4))) + 
  scale_colour_manual(values = c("Annual Average Pay"="#d7301f", "Income per Employee"="#08519c")) +
  scale_linetype_manual(values = c("Annual Average Pay"=1, "Income per Employee"=2)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.2,0.85),
    legend.key.width = unit(1, 'cm'),
    axis.title.y = element_blank(),
    strip.background = element_rect(fill="white")
  ) -> g_growth


data %>% 
  filter(time==2019) %>% 
  select(FIPS, AAP_TOT_TOT, income_emp) %>% 
  pivot_longer(cols = c(AAP_TOT_TOT, income_emp)) %>% 
  mutate(name=factor(name, levels = c("AAP_TOT_TOT", "income_emp"), labels = c("Annual Average Pay", "Income per Employee"))) %>%
  mutate(var = "County-level distribution (2019)") %>% 
  ggplot() + 
  geom_density(aes(x=value, fill=name, color=name), alpha=0.3) +
  facet_wrap(var~.) +
  scale_x_log10(breaks=c(30000, 50000, 100000), labels = c("30k", "50k", "100k"), limits=c(17000, NA)) +
  scale_fill_manual(values = c("Annual Average Pay"="#d7301f", "Income per Employee"="#08519c")) +
  scale_color_manual(values = c("Annual Average Pay"="#d7301f", "Income per Employee"="#08519c")) +
  xlab("Dollars (2012)") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.8,0.85),
    legend.key.width = unit(1, 'cm'),
    axis.title.y = element_blank(),
    strip.background = element_rect(fill="white")
  ) -> g_dist


#2019 SURBEY OF CONSUMER FINANCES
#https://www.federalreserve.gov/econres/scf_2019.htm


scf <- read_excel("data/SCF/scf2022_tables_public_real_historical.xlsx", 
               sheet = "Table 2", range = "A108:G114", 
               col_names = FALSE)

colnames(scf) <- c("incs", "p25", "p25_50", "p50_75", "p75_90", "p90_100", "total")

scf[2,2] <- "< 0.05"
scf[4,2] <- "< 0.05"
scf[1,2] <- as.character(round(as.numeric(scf[1,2]),2))
scf <- scf[-nrow(scf),]
scf[,4:7]<- round(scf[,4:7],2)

scf %<>% 
  mutate(across(everything(), ~ as.character(.x))) %>% 
  mutate(across(p25:total, ~ paste0(.x, "%"))) %>% 
  add_row(incs=" ", p25="    < 25th    ", p25_50="25th-50th", p50_75="50th-75th", p75_90="75th-90th", p90_100="    > 90th    ", total="", .before=1)

colnames(scf) <- c("Income source", " ", " ", "Percentiles\nof wealth", " ", " ", "    Total    ")

library(ggpubr)

ggtexttable(
  scf,
  rows = NULL,
  theme = ttheme(
    base_style="light",
    base_size = 10,
    padding = unit(c(6, 6), "mm")
  )
) %>% 
  tab_add_hline(at.row = 3, row.side = "top", from.column = 2, to.column = 6, linewidth = 1.5) -> tab

final <- cowplot::ggdraw() +
  cowplot::draw_plot(g_growth, 0, 0.473, 0.5, 0.5) +
  cowplot::draw_plot(g_dist, 0.5, 0.473, 0.5, 0.5) +
  cowplot::draw_plot(tab, 0.25, 0, 0.5, 0.51) +
  cowplot::draw_plot_label(c("A", "B", "C"), c(0, 0.5, 0.11), c(1, 1, 0.47), size = 15)

ggsave("charts/summary_dependent.pdf", final, width=12, height = 8, scale=0.9)
knitr::plot_crop("charts/summary_dependent.pdf")



