library(tidyverse)
library(magrittr)
library(stargazer)
library(ggplot2)
library(grDevices)
library(knitr)
library(kableExtra)
library(fixest)
library(readxl)
library(tictoc)
library(DIDmultiplegtDYN)

rm(list = ls())

source("new_scripts/functions_f.R")
data <- readRDS(file="data/f_panel_tr.rds")

data %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )

data %<>% 
  group_by(FIPS) %>% 
  mutate( #compute within tresholds
    tresh_wt = quantile(S4, 0.5)
  ) %>% 
  ungroup() %>% 
  mutate( #compute global tresholds
    tresh_gl = quantile(S4, 0.5)
  ) %>% 
  group_by(Area) %>% #keep track of never treated
  mutate(nt = sum(S4==0)==n()) %>% 
  ungroup() %>% 
  mutate(
    treat = ifelse(S4>=tresh_wt & S4>=tresh_gl, 1, 0)
  ) %>% 
  mutate(
    treat = ifelse(nt, 0, treat),
  )

data %<>% 
  mutate(
    lag_ST_AAP_TOT_TOT = ifelse(Year==1991, as.numeric(NA), dplyr::lag(ST_AAP_TOT_TOT)),
    lag2_ST_AAP_TOT_TOT = ifelse(Year%in%c(1991, 1992), as.numeric(NA), dplyr::lag(ST_AAP_TOT_TOT, 2))
  )


# Estimation 


modi <- did_multiplegt_dyn(data, 
                            "GR_INCOME_PC", # outcome variable
                            'Area', # group
                            'Year', # time
                            "treat", # treatment
                            effects = 10,
                            placebo = 4,
                            trends_lin = F,
                            normalized = T,
                            controls = c("tmp", "prec"),
                            cluster = 'Area',
                            graph_off=TRUE
)


modw <- did_multiplegt_dyn(data,
                           "GR_AAP_TOT_TOT", # outcome variable
                           'Area', # group
                           'Year', # time
                           "treat", # treatment
                           effects = 10,
                           placebo = 4,
                           trends_lin = F,
                           normalized = T,
                           controls = c("tmp", "prec", "lag_ST_AAP_TOT_TOT", "lag2_ST_AAP_TOT_TOT"),
                           cluster = 'Area',
                           graph_off=TRUE
)

# Plots 

p_plac <-tibble(
  pval = c(round(modw$results$p_jointplacebo,2), round(modi$results$p_jointplacebo,2)),
  var = c("Annual Average Pay", "Income per capita")
)

p_eff <-tibble(
  pval = c(round(modw$results$p_jointeffects,2), round(modi$results$p_jointeffects,2)),
  var = c("Annual Average Pay", "Income per capita")
)


  rbind(
    modw$results$Placebos %>% 
      as_tibble() %>% 
      select(Estimate, `LB CI`, `UB CI`) %>% 
      add_column(year=-4:-1),
    modw$results$Effects %>% 
      as_tibble() %>% 
      select(Estimate, `LB CI`, `UB CI`) %>% 
      add_row(Estimate= 0, `LB CI`= as.numeric(NA), `UB CI`=as.numeric(NA), .before=1) %>% 
      add_column(year=0:10)
  ) %>% 
    add_column(var="Annual Average Pay") %>% 
  ggplot() + 
  geom_point(aes(x=year, y=Estimate), color="#d7301f") +
  geom_line(aes(x=year, y=Estimate), color="#d7301f") +
  geom_errorbar(aes(x=year, ymin=`LB CI`, ymax=`UB CI`), color="#d7301f", alpha=0.4) +
  geom_text(data=p_plac %>% filter(var=="Annual Average Pay"), aes(x= 8, y= 0.2, label=paste0("Joint nullity\nof pre-event coefficients:\np-value=", pval)), size=3.5) +
  facet_wrap(.~var) +
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  xlab("Years since last period before treatment changes") +
  ylab("% change") + 
  scale_x_continuous(breaks=c(-4,-2,0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1, 'cm'),
    strip.background = element_blank(),
    axis.text=element_text(size=11),
    axis.title=element_text(size=14),
    strip.text = element_text(size=12),
    legend.text = element_text(size=12)
  ) -> g_did_w
  
  rbind(
    modi$results$Placebos %>% 
      as_tibble() %>% 
      select(Estimate, `LB CI`, `UB CI`) %>% 
      add_column(year=-4:-1),
    modi$results$Effects %>% 
      as_tibble() %>% 
      select(Estimate, `LB CI`, `UB CI`) %>% 
      add_row(Estimate= 0, `LB CI`= as.numeric(NA), `UB CI`=as.numeric(NA), .before=1) %>% 
      add_column(year=0:10)
  ) %>% 
    add_column(var="Income per capita") %>% 
    ggplot() + 
    geom_point(aes(x=year, y=Estimate), color="#08519c") +
    geom_line(aes(x=year, y=Estimate), color="#08519c") +
    geom_errorbar(aes(x=year, ymin=`LB CI`, ymax=`UB CI`), color="#08519c", alpha=0.4) +
    geom_text(data=p_plac %>% filter(var=="Income per capita"), aes(x= 8, y= -0.34, label=paste0("Joint nullity\nof pre-event coefficients:\np-value=", pval)), size=3.5) +
    facet_wrap(.~var) +
    geom_hline(yintercept=0, linetype=3)+ 
    geom_vline(xintercept = 0)+
    xlab("Years since last period before treatment changes") +
    ylab("% change") + 
    scale_x_continuous(breaks=c(-4,-2,0,2,4,6,8,10)) +
    scale_y_continuous(labels = function(x) paste0(x, '%')) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.key.width = unit(1, 'cm'),
      strip.background = element_blank(),
      axis.text=element_text(size=11),
      axis.title=element_text(size=14),
      strip.text = element_text(size=12),
      legend.text = element_text(size=12)
    ) -> g_did_i

ggsave("charts/robustness_did.pdf", 
       cowplot::plot_grid(g_did_w, g_did_i, nrow=1, labels = c('A','B')),  
       width=11.8, height = 4)


#### Tables ####

yy <- NULL
for(i in -4:10){
  new <- c(i, "")
  yy <- c(yy, new)
}
yy <- yy[-c(9,10)]

cbind(
  rbind(
    modw$results$Placebos %>% 
      as_tibble() %>% 
      mutate(
        Estimate = round(Estimate, 3),
        `LB CI` = round(`LB CI`, 3),
        `UB CI` = round(`UB CI`, 3)
      ) %>% 
      mutate(ci = paste0("[", `LB CI`, ",", `UB CI`, "]")) %>% 
      select(Estimate, ci) %>% 
      add_column(year=-4:-1, .before=1) %>% 
      mutate(Estimate=as.character(Estimate)) %>% 
      pivot_longer(cols = c(Estimate, ci), names_to = "name", values_to="val") %>% 
      select(-name),
    modw$results$Effects %>% 
      as_tibble() %>% 
      mutate(
        Estimate = round(Estimate, 3),
        `LB CI` = round(`LB CI`, 3),
        `UB CI` = round(`UB CI`, 3)
      ) %>% 
      mutate(ci = paste0("[", `LB CI`, ",", `UB CI`, "]")) %>% 
      select(Estimate, ci) %>% 
      add_column(year=1:10, .before=1) %>% 
      mutate(Estimate=as.character(Estimate)) %>% 
      pivot_longer(cols = c(Estimate, ci), names_to = "name", values_to="val") %>% 
      select(-name)
  ),
  rbind(
    modi$results$Placebos %>% 
      as_tibble() %>% 
      mutate(
        Estimate = round(Estimate, 3),
        `LB CI` = round(`LB CI`, 3),
        `UB CI` = round(`UB CI`, 3)
      ) %>% 
      mutate(ci = paste0("[", `LB CI`, ",", `UB CI`, "]")) %>% 
      select(Estimate, ci) %>% 
      add_column(year=-4:-1, .before=1) %>% 
      mutate(Estimate=as.character(Estimate)) %>% 
      pivot_longer(cols = c(Estimate, ci), names_to = "name", values_to="val") %>% 
      select(-name),
    modi$results$Effects %>% 
      as_tibble() %>% 
      mutate(
        Estimate = round(Estimate, 3),
        `LB CI` = round(`LB CI`, 3),
        `UB CI` = round(`UB CI`, 3)
      ) %>% 
      mutate(ci = paste0("[", `LB CI`, ",", `UB CI`, "]")) %>% 
      select(Estimate, ci) %>% 
      add_column(year=1:10, .before=1) %>% 
      mutate(Estimate=as.character(Estimate)) %>% 
      pivot_longer(cols = c(Estimate, ci), names_to = "name", values_to="val") %>% 
      select(-name)
  ) %>% select(-year)
) %>% 
  `colnames<-`(letters[1:3]) %>% 
  mutate(a=yy) %>% 
  add_row(a="", b="", c="", .before=8) %>% 
  add_row(a="Climatic controls",    b="\\checkmark", c="\\checkmark") %>%
  add_row(a="State-Level Lagged Annual Average Pay",    b="\\checkmark", c="") %>%
  add_row(a="Joint nullity of placebos: p-value", 
          b=as.character(round(modw$results$p_jointplacebo,3)), 
          c=as.character(round(modi$results$p_jointplacebo,3))
          ) %>% 
  add_row(a="Joint nullity of effects: p-value", 
          b=ifelse(modw$results$p_jointeffects<0.001, "$<0.001$", as.character(round(modw$results$p_jointeffects,3))), 
          c=ifelse(modi$results$p_jointeffects<0.001, "$<0.001$", as.character(round(modi$results$p_jointeffects,3)))
  ) %>% 
  `colnames<-`(c("Years since last period before treatment changes", 
                 " ", 
                 " ")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  pack_rows("Placebos", 1, 8, bold=F) %>%
  pack_rows("Effects", 9, 28, bold=F) %>%
  row_spec(8, hline_after = T) %>% 
  row_spec(29, hline_after = T) %>% 
  row_spec(31, hline_after = T) %>% 
  add_header_above(c(" " = 1, 
                     "Annual Average Pay" = 1,
                     "Income per capita" = 1
  ), bold=T, line_sep=0) %>% 
  writeLines(., "tables/did.tex")


