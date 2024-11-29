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

hpi <- read_excel("data/hpi/hpi_at_bdl_county.xlsx", 
                  skip = 6)
# https://www.fhfa.gov/data/hpi/datasets?tab=additional-data download page housing county level data

#hpi <- read_csv("data/hpi/hpi_master.csv")
#https://www.fhfa.gov/data/hpi/datasets?tab=master-hpi-data download page housing data


hpi %<>% 
  mutate(
    time=as.numeric(Year),
  ) %>% 
  filter(time>=1990) %>% 
  filter(time<=2019) %>% 
  rename(
    FIPS = `FIPS code`,
    change_original=`Annual Change (%)`,
    HPI_original=HPI,
    HPI_1990=`HPI with 1990 base`,
    HPI_2000=`HPI with 2000 base`
  ) %>% 
  mutate(
    HPI_original=ifelse(HPI_original==".", as.numeric(NA), as.numeric(HPI_original)),
    HPI_1990=ifelse(HPI_1990==".", as.numeric(NA), as.numeric(HPI_1990)),
    HPI_2000=ifelse(HPI_2000==".", as.numeric(NA), as.numeric(HPI_2000))
  ) %>% 
  arrange(FIPS, Year) %>% 
  group_by(FIPS) %>% #we need to have a balanced panel. Remove those who do not have all observations from 1990 to 2019
  mutate(
    to_rem = sum(!is.na(HPI_1990))!=30, #sum of NOT missing values different from 30 (total of years)
  ) %>% 
  ungroup() %>% 
  filter(!to_rem) %>% 
  select(-to_rem, -time) %>% 
  arrange(FIPS, Year) %>%  #compute growth rates
  group_by(FIPS) %>% 
  mutate(
    across(c(HPI_original, HPI_1990, HPI_2000), ~ ((.x/dplyr::lag(.x))-1)*100, .names = "GR_{.col}"
    )
  ) %>% 
  ungroup() %>% #maintain only needed columns
  select(FIPS, Year, GR_HPI_1990) %>% 
  rename(GR_HPI=GR_HPI_1990)


data %>% 
  inner_join(
    ., 
    hpi, 
    by=c("FIPS", "Year")
  ) -> data_hpi


data_hpi %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )

# Save for stata 

data_stata <- data_hpi %>% 
  mutate(
    state_trend_ALABAMA = ifelse(STATE=="ALABAMA", time, 0),
    state_trend_ARIZONA = ifelse(STATE=="ARIZONA", time, 0),
    state_trend_ARKANSAS = ifelse(STATE=="ARKANSAS", time, 0),
    state_trend_CALIFORNIA = ifelse(STATE=="CALIFORNIA", time, 0),
    state_trend_COLORADO = ifelse(STATE=="COLORADO", time, 0),
    state_trend_CONNECTICUT = ifelse(STATE=="CONNECTICUT", time, 0),
    state_trend_DELAWARE = ifelse(STATE=="DELAWARE", time, 0),
    state_trend_FLORIDA = ifelse(STATE=="FLORIDA", time, 0),
    state_trend_GEORGIA = ifelse(STATE=="GEORGIA", time, 0),
    state_trend_IDAHO = ifelse(STATE=="IDAHO", time, 0),
    state_trend_ILLINOIS = ifelse(STATE=="ILLINOIS", time, 0),
    state_trend_INDIANA = ifelse(STATE=="INDIANA", time, 0),
    state_trend_IOWA = ifelse(STATE=="IOWA", time, 0),
    state_trend_KANSAS = ifelse(STATE=="KANSAS", time, 0),
    state_trend_KENTUCKY = ifelse(STATE=="KENTUCKY", time, 0),
    state_trend_LOUISIANA = ifelse(STATE=="LOUISIANA", time, 0),
    state_trend_MAINE = ifelse(STATE=="MAINE", time, 0),
    state_trend_MARYLAND = ifelse(STATE=="MARYLAND", time, 0),
    state_trend_MASSACHUSETTS = ifelse(STATE=="MASSACHUSETTS", time, 0),
    state_trend_MICHIGAN = ifelse(STATE=="MICHIGAN", time, 0),
    state_trend_MINNESOTA = ifelse(STATE=="MINNESOTA", time, 0),
    state_trend_MISSISSIPPI = ifelse(STATE=="MISSISSIPPI", time, 0),
    state_trend_MISSOURI = ifelse(STATE=="MISSOURI", time, 0),
    state_trend_MONTANA = ifelse(STATE=="MONTANA", time, 0),
    state_trend_NEBRASKA = ifelse(STATE=="NEBRASKA", time, 0),
    state_trend_NEVADA = ifelse(STATE=="NEVADA", time, 0),
    state_trend_NEW_HAMPSHIRE = ifelse(STATE=="NEW HAMPSHIRE", time, 0),
    state_trend_NEW_JERSEY = ifelse(STATE=="NEW JERSEY", time, 0),
    state_trend_NEW_MEXICO = ifelse(STATE=="NEW MEXICO", time, 0),
    state_trend_NEW_YORK = ifelse(STATE=="NEW YORK", time, 0),
    state_trend_NORTH_CAROLINA = ifelse(STATE=="NORTH CAROLINA", time, 0),
    state_trend_NORTH_DAKOTA = ifelse(STATE=="NORTH DAKOTA", time, 0),
    state_trend_OHIO = ifelse(STATE=="OHIO", time, 0),
    state_trend_OKLAHOMA = ifelse(STATE=="OKLAHOMA", time, 0),
    state_trend_OREGON = ifelse(STATE=="OREGON", time, 0),
    state_trend_PENNSYLVANIA = ifelse(STATE=="PENNSYLVANIA", time, 0),
    state_trend_RHODE_ISLAND = ifelse(STATE=="RHODE ISLAND", time, 0),
    state_trend_SOUTH_CAROLINA = ifelse(STATE=="SOUTH CAROLINA", time, 0),
    state_trend_SOUTH_DAKOTA = ifelse(STATE=="SOUTH DAKOTA", time, 0),
    state_trend_TENNESSEE = ifelse(STATE=="TENNESSEE", time, 0),
    state_trend_TEXAS = ifelse(STATE=="TEXAS", time, 0),
    state_trend_UTAH = ifelse(STATE=="UTAH", time, 0),
    state_trend_VERMONT = ifelse(STATE=="VERMONT", time, 0),
    state_trend_VIRGINIA = ifelse(STATE=="VIRGINIA", time, 0),
    state_trend_WASHINGTON = ifelse(STATE=="WASHINGTON", time, 0),
    state_trend_WEST_VIRGINIA = ifelse(STATE=="WEST VIRGINIA", time, 0),
    state_trend_WISCONSIN = ifelse(STATE=="WISCONSIN", time, 0),
    state_trend_WYOMING = ifelse(STATE=="WYOMING", time, 0)
  ) 

data_stata %<>% 
  mutate(
    lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(S4,1)),
    lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(S4,2)),
    lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(S4,3)),
    lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(S4,4)),
    lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(S4,5)),
    lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(S4,6)),
    lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(S4,7)),
    lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(S4,8)),
    lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(S4,9)),
    lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(S4,10))
  ) 

write.csv(data_stata, "data/panel_housing.csv", row.names = FALSE)
haven::write_dta(data_stata, "stata_rob/panel_housing.dta")

data_hpi$x<-data_hpi$S4
data_hpi$y<-data_hpi$GR_HPI

mod_hpi <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data_hpi, panel.id = c("Area", "Year"))
) 

cum_hpi <- getcumul10_f(mod_hpi, data_hpi, "stata_rob/vcov_housing_trend.xlsx")

mod_hpi_2 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)^factor(Year),
  data = fixest::panel(data_hpi, panel.id = c("Area", "Year"))
) 

cum_hpi_2 <- getcumul10_f(mod_hpi_2, data_hpi, "stata_rob/vcov_housing_stateyear.xlsx")


#### Tables ####

yy <- NULL
for(i in 0:10){
  new <- c(i, "")
  yy <- c(yy, new)
}


cbind(
  tablize_f(cum_hpi, mod_hpi),
  tablize_f(cum_hpi_2, mod_hpi_2) %>% select(-year)
) %>% 
  `colnames<-`(c("a", "b", "c")) %>% 
  mutate(a=yy) %>% 
  add_row(a="County fixed effects", b="\\checkmark", c="\\checkmark") %>%
  add_row(a="Year fixed effects",   b="\\checkmark", c="\\checkmark") %>%
  add_row(a="State-level trends",   b="\\checkmark", c="") %>%
  add_row(a="State-Year dummies",   b="", c="\\checkmark") %>%
  add_row(a="Climatic controls",    b="\\checkmark", c="\\checkmark") %>%
  add_row(a="Adjusted $R^2$", 
          b=as.character(round(r2(mod_hpi, type="ar2"),3)), 
          c=as.character(round(r2(mod_hpi_2, type="ar2"),3)),
  ) %>% 
  add_row(a="Observations", 
          b=as.character(nobs(mod_hpi)), 
          c=as.character(nobs(mod_hpi_2))
  ) %>% 
  `colnames<-`(c("Years since storm exposure", 
                 "(1)", 
                 "(2)"
  )) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 7, hline_after = T) %>% 
  add_header_above(c(" " = 1, 
                     "House Price Index" = 2
  ), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., "tables/housing_rob.tex")



#### Tests for non-stationarity ####

data_hpi %>% 
  arrange(Area, time) %>% 
  group_by(Area) %>% 
  summarise(
    hpi_rho_1 = aTSA::pp.test(GR_HPI, type="Z_rho", output=F)["type 1", "p.value"],
    hpi_rho_2 = aTSA::pp.test(GR_HPI, type="Z_rho", output=F)["type 2", "p.value"],
    hpi_tau_1 = aTSA::pp.test(GR_HPI, type="Z_tau", output=F)["type 1", "p.value"],
    hpi_tau_2 = aTSA::pp.test(GR_HPI, type="Z_tau", output=F)["type 2", "p.value"]
  ) -> pp_data


data_hpi %>% 
  filter(FIPS!="36063") %>%  #throws an error
  arrange(Area, time) %>% 
  group_by(Area) %>% 
  summarise(
    hpi_mk = Kendall::MannKendall(GR_HPI)$sl,
    hpi_mmk_ar = funtimes::notrend_test(GR_HPI, test = "MK", B=2000)$p.value ,
  ) -> kendall_data


data_hpi %>% 
  group_by(Area) %>% 
  summarise(
    beta_hpi = lm(GR_HPI~time)$coefficients["time"],
    p_hpi = summary(lm(GR_HPI~time))$coefficients["time","Pr(>|t|)"],
  ) -> data_regs



cbind(
  pp_data %>% 
    summarise(
      across(c(hpi_rho_1, hpi_rho_2), ~ (sum(.x<=0.05))/n() )
    ),
  kendall_data %>% 
    summarise(
      across(c(hpi_mk,hpi_mmk_ar), ~ (sum(.x>0.05))/n() )
    ),
  data_regs %>% 
    summarise(
      across(c(p_hpi), ~ (sum(.x>0.05))/n() )
    ) 
) %>% 
  mutate(across(everything(), ~ paste0(round(.x*100,2), "%") )) %>% 
  `colnames<-`(c("Phillips-Perron\n(no constant)", "Phillips-Perron\n(with constant)",
                 "Mann-Kendall", "Mann-Kendall\n(Bootstrapped)", "Linear Model\n(T-test)")) -> tab_hpi


#### Charts ####

data_hpi %>% 
  ggplot() +
  geom_line(aes(x=time, y=GR_HPI, group=FIPS), linewidth=0.025, color="grey40") +
  geom_hline(aes(yintercept=mean(GR_HPI)), color="indianred", linetype=2, linewidth=0.5) +
  scale_x_continuous(breaks = c(1991, 2000, 2010, 2019), expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  theme_bw() + 
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(2.4, "lines"),
    axis.title = element_blank(),
    text = element_text(size=18),
    plot.margin = unit(c(2,10,2,2), "mm")
  ) -> g_hpi_rates


ggpubr::ggtexttable(
  tab_hpi,
  rows = NULL,
  theme = ggpubr::ttheme(
    base_size = 11,
    rownames.style = ggpubr::rownames_style(
      fill = c("white", "grey80"),
      hjust=1.3,
      parse = T
    ),
  )
) -> t_hpi


cowplot::plot_grid(
  g_hpi_rates,
  t_hpi,
  nrow=2,
  rel_heights = c(0.8,0.2)
) -> combo_county


ggsave("charts/g_hpi_rates_tab.pdf", combo_county, width=5.85, height = 4.1, scale=1.3)







