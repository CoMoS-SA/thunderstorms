library(readxl)
library(tidyverse)
library(sf)
library(magrittr)
library(tictoc)
library(tmaptools, warn.conflicts =FALSE)
library(fixest)
library(knitr)
library(kableExtra)


rm(list = ls())
filtered_storms <- readRDS(file="data/clean_filtered_storms.rds")

filtered_storms %>% 
  group_by(FIPS_AG, YEAR) %>% 
  summarise(
    M1 = n(),
    M2 = max(MAGNITUDE),
    M3 = sum(MAGNITUDE^3),
    M4 = sum(MAGNITUDE^2),
    COUNTY_AREA  = COUNTY_AREA[1]
  ) %>% ungroup() -> filtered_storms_aggr


#Create normalized measures 
filtered_storms_aggr %<>% 
  mutate(
    S1 = ((M1/COUNTY_AREA)*947.4579),
    S2 = ((M2/COUNTY_AREA)*947.4579)*(0.44704), 
    S3 = ((M3/COUNTY_AREA)*947.4579)*(0.44704^3),
    S4 = ((M4/COUNTY_AREA)*947.4579)*(0.44704^2)
  )

data <- readRDS(file="data/f_panel_tr.rds")

data %>% 
  left_join(
    .,
    filtered_storms_aggr %>% 
      mutate(Year = as.character(YEAR)) %>% 
      select(FIPS_AG, Year, S4) %>% 
      rename(S4_filtered=S4),
    by = c("FIPS"="FIPS_AG", "Year"="Year"),
  ) %>% 
  mutate(
    S4_filtered = ifelse(is.na(S4_filtered), 0, S4_filtered)
  ) -> data_filter

data_filter %<>% 
  mutate(S4f = S4 + S4_filtered)


#Save for stata
data %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )

data_filter %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )


data_stata <- data_filter %>% 
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
    lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(S4f,1)),
    lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(S4f,2)),
    lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(S4f,3)),
    lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(S4f,4)),
    lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(S4f,5)),
    lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(S4f,6)),
    lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(S4f,7)),
    lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(S4f,8)),
    lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(S4f,9)),
    lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(S4f,10))
  ) 

write.csv(data_stata, "data/panel_filt_csv.csv", row.names = FALSE)
haven::write_dta(data_stata, "stata_rob/panel_filt_dta.dta")


#### Estimation ####
source("new_scripts/functions_f.R")


getcumul10_ffilt <- getcumul10_f
body(getcumul10_ffilt)[[2]] <- substitute(
  if (is.null(stata_vcov)) {
    if (ssc) {
      vcovHC <- fixest::vcov_cluster(mod, cluster = clust)
    }
    else {
      vcovHC <- fixest::vcov_cluster(mod, cluster = clust, 
                                     ssc = fixest::ssc(fixef.K = "none", adj = FALSE, 
                                                       cluster.adj = FALSE))
    }
  } else {
    vcovHC <- read_excel(stata_vcov)
    vcovHC %<>% rename(var = ...1) %>% filter(var %in% c("S4f", 
                                                         "lag1", "lag2", "lag3", "lag4", "lag5", "lag6", "lag7", 
                                                         "lag8", "lag9", "lag10")) %>% select(c("S4f", "lag1", 
                                                                                                "lag2", "lag3", "lag4", "lag5", "lag6", "lag7", "lag8", 
                                                                                                "lag9", "lag10")) %>% as.matrix()
    colnames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", 
                          "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", 
                          "l(x, 9)", "l(x, 10)")
    rownames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", 
                          "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", 
                          "l(x, 9)", "l(x, 10)")
  }
)

#Baseline model wages
data$x<-data$S4
data$y<-data$GR_AAP_TOT_TOT

mod0_w_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

cum0_w_f <- getcumul10_f(mod0_w_f, data, "stata_rob/vcov_conley50_1lag.xlsx")

#Baseline model income
data$x<-data$S4
data$y<-data$GR_INCOME_PC

mod0_i_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

cum0_i_f <- getcumul10_f(mod0_i_f, data, "stata_rob/income_vcov_conley50_1lag.xlsx")


#Baseline model wages
data_filter$x<-data_filter$S4f
data_filter$y<-data_filter$GR_AAP_TOT_TOT

modf_w_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data_filter, panel.id = c("Area", "Year"))
) 

cumf_w_f <- getcumul10_ffilt(modf_w_f, data, "stata_rob/vcov_w_filt.xlsx")

#Baseline model income
data_filter$x<-data_filter$S4f
data_filter$y<-data_filter$GR_INCOME_PC

modf_i_f <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data_filter, panel.id = c("Area", "Year"))
) 

cumf_i_f <- getcumul10_ffilt(modf_i_f, data, "stata_rob/vcov_i_filt.xlsx")

#### Charts ####


rbind(
  cum0_w_f %>% add_row(coeffsums_sd=0, lower=0, upper=0, year=-1) %>% add_column(type="Winds > 75 km/h (Baseline)") %>% add_column(var="Annual Average Pay"),
  cumf_w_f %>% add_row(coeffsums_sd=0, lower=0, upper=0, year=-1) %>% add_column(type="All winds") %>% add_column(var="Annual Average Pay")
) %>% 
  mutate(type=factor(type, levels=c("Winds > 75 km/h (Baseline)", "All winds"))) %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(linewidth=0.8, aes(color=factor(type))) +
  geom_line(aes(x=year, y=lower, color=factor(type)), linetype=2) + 
  geom_line(aes(x=year, y=upper, color=factor(type)), linetype=2) + 
  xlab("Years since storm exposure") +
  facet_wrap(var~., scales = "free_y", nrow=1) +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_colour_manual(values = c("Winds > 75 km/h (Baseline)"="#d7301f", "All winds"="#fdae61")) +
  scale_fill_manual(values = c("Winds > 75 km/h (Baseline)"="#d7301f", "All winds"="#fdae61")) +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
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
  ) -> gw

rbind(
  cum0_i_f %>% add_row(coeffsums_sd=0, lower=0, upper=0, year=-1) %>% add_column(type="Winds > 75 km/h (Baseline)") %>% add_column(var="Income per capita"),
  cumf_i_f %>% add_row(coeffsums_sd=0, lower=0, upper=0, year=-1) %>% add_column(type="All winds") %>% add_column(var="Income per capita")
) %>% 
  mutate(type=factor(type, levels=c("Winds > 75 km/h (Baseline)", "All winds"))) %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(linewidth=0.8, aes(color=factor(type))) +
  geom_line(aes(x=year, y=lower, color=factor(type)), linetype=2) + 
  geom_line(aes(x=year, y=upper, color=factor(type)), linetype=2) + 
  xlab("Years since storm exposure") +
  facet_wrap(var~., scales = "free_y", nrow=1) +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_colour_manual(values = c("Winds > 75 km/h (Baseline)"="#08519c", "All winds"="#41b6c4")) +
  scale_fill_manual(values = c("Winds > 75 km/h (Baseline)"="#08519c", "All winds"="#41b6c4")) +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
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
  ) -> gi


g_filt <- cowplot::plot_grid(gw, gi, nrow=1)

ggsave("charts/filtered_storms.pdf", 
       cowplot::plot_grid(gw, gi, nrow=1, labels = c('A','B')),  
       width=11.8, height = 4.4)



#### Table ####


yy <- NULL
for(i in 0:10){
  new <- c(i, "")
  yy <- c(yy, new)
}

cbind(
  tablize_f(cum0_w_f, mod0_w_f),
  tablize_f(cumf_w_f, modf_w_f) %>% select(-year),
  tablize_f(cum0_i_f, mod0_i_f) %>% select(-year),
  tablize_f(cumf_i_f, modf_i_f) %>% select(-year)
) %>% 
  `colnames<-`(letters[1:5]) %>% 
  mutate(a=yy) %>% 
  add_row(a="County fixed effects", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark") %>%
  add_row(a="Year fixed effects",   b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark") %>%
  add_row(a="State-level trends",   b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark") %>%
  add_row(a="Climatic controls",    b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark") %>%
  add_row(a="Adjusted $R^2$", 
          b=as.character(round(r2(mod0_w_f, type="ar2"),3)), 
          c=as.character(round(r2(modf_w_f, type="ar2"),3)),
          d=as.character(round(r2(mod0_i_f, type="ar2"),3)),
          e=as.character(round(r2(modf_i_f, type="ar2"),3))
  ) %>% 
  add_row(a="Observations", 
          b=as.character(nobs(mod0_w_f)), 
          c=as.character(nobs(modf_w_f)),
          d=as.character(nobs(mod0_i_f)),
          e=as.character(nobs(modf_i_f))
  ) %>% 
  `colnames<-`(c("Years since storm exposure", "Winds > 75 km/h (Baseline)", "All winds", "Winds > 75 km/h (Baseline)", "All winds")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 6, hline_after = T) %>% 
  row_spec(10*2 + 8, hline_after = T) %>% 
  add_header_above(c(" " = 1, 
                     "Annual Average Pay" = 2,
                     "Income per capita" = 2
  ), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., "tables/filtered_storms.tex")
