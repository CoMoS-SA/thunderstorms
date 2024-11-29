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
data <- readRDS(file="data/f_panel_hail_tr.rds")


body(getcumul10_f)[[20]] <- substitute(lower <- coeffsums_sd - sdsums_sd * 1.645)
body(getcumul10_f)[[21]] <- substitute(upper <- coeffsums_sd + sdsums_sd * 1.645)

body(getcumul10_f)[[2]] <- substitute(
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
    vcovHC %<>% rename(var = ...1) %>% filter(var %in% c("S3", 
                                                         "lag1", "lag2", "lag3", "lag4", "lag5", "lag6", "lag7", 
                                                         "lag8", "lag9", "lag10")) %>% select(c("S3", "lag1", 
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



data %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )


#Wages
data$x<-data$S3
data$y<-data$GR_AAP_TOT_TOT

mod_w0 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_w0)
cum_w0 <- getcumul10_f(mod_w0, data, "stata_rob/vcov_hail.xlsx")

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

data$x<-data$S3
data$y<-data$GR_INCOME_PC

mod_i0 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 

summary(mod_i0)
cum_i0 <- getcumul10_f(mod_i0, data, "stata_rob/vcov_hail_income.xlsx")

mod_i0_expl <- fixest::feols(
  fml = as.formula(paste("y ~ l(x,0:10) + tmp + prec +", 
                         paste(names(data_expl) %>% grep("^state_trend_", ., value = TRUE), collapse = " + "), 
                         "| Area + Year")),
  data = fixest::panel(data_expl, panel.id = c("Area", "Year"))
) 


rbind(
  cum_w0 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Annual Average Pay") , 
  cum_i0 %>% add_row(year=-1, coeffsums_sd=0, lower=0, upper=0) %>% add_column(var="Income per capita")
) %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(aes(colour=factor(var), linetype=factor(var)), linewidth=0.8) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(var)), alpha=0.25) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~H^(3))) + 
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

ggsave("charts/main_hail.pdf", g1, width=5.85, height = 4.1)


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
  mutate(a=yy) %>% 
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
  writeLines(., "new_tables/main_hail.tex")


