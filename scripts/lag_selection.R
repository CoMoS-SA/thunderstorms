library(tidyverse)
library(magrittr)
library(stargazer)
library(ggplot2)
library(grDevices)
library(knitr)
library(kableExtra)
library(fixest)
library(readxl)
library(fixest)

rm(list = ls())

source("scripts/functions_f.R")
data <- readRDS(file="data/f_panel_tr.rds")

data %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )

data$x<-data$S4

max_lag <- 10
models_w <- list()
models_i <- list()
for (lag in 0:max_lag) {
  formula <- as.formula(paste("y ~ l(x,0:", lag, ") + tmp + prec | Area + Year + factor(STATE)[time]", sep = ""))
  data$y<-data$GR_AAP_TOT_TOT
  models_w[[lag+1]] <- fixest::feols(formula, data = fixest::panel(data %>% filter(time>=(1990 + max_lag +1 -lag)), panel.id = c("Area", "Year")))
  data$y<-data$GR_INCOME_PC
  models_i[[lag+1]] <- fixest::feols(formula, data = fixest::panel(data %>% filter(time>=(1990 + max_lag +1 -lag)), panel.id = c("Area", "Year")))
}


wald_w <- tibble(lag=c(paste0("$\beta_", 1:9, "$ to $\beta_{10}$"), "$\beta_{10}$"), wald=as.character(rep(0, max_lag)))
wald_i <- tibble(lag=c(paste0("$\beta_", 1:9, "$ to $\beta_{10}$"), "$\beta_{10}$"), wald=as.character(rep(0, max_lag)))

for(j in 1:10){
  #wald_w$lag[j] <- j
  aux <- wald(models_w[[11]],paste0(paste0(paste0("l\\(x, ", j:10, "\\)|"), collapse = ""), "\n"))$p
  wald_w$wald[j] <- ifelse(aux<0.001, "<0.001", round(aux, 3))
}

for(j in 1:10){
  aux <- wald(models_i[[11]],paste0(paste0(paste0("l\\(x, ", j:10, "\\)|"), collapse = ""), "\n"))$p
  wald_i$wald[j] <- ifelse(aux<0.001, "<0.001", round(aux, 3))
}


cbind(
  wald_w, 
  wald_i %>% select(-lag)
) %>% 
  `colnames<-`(letters[1:3]) %>% 
  `colnames<-`(c("Joint nullity of", "p-value", "p-value")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  add_header_above(c(" " = 1, 
                     "Annual Average Pay" = 1,
                     "Income per capita" = 1
  ), bold=T) %>% 
  writeLines(., "tables/lag_wald.tex")


cbind(
  map_dbl(models_w, AIC),
  map_dbl(models_w, BIC),
  map_dbl(models_w, function(x){
    -2 * logLik(x) + 2 * length(coef(x)) * log(log(nobs(x)))
  }),
  map_dbl(models_w, function(x){r2(x, type="ar2")})
) %>% 
  as_tibble() %>% 
  `colnames<-`(c("AIC", "BIC", "HQ", "Adjusted R2")) %>%
  mutate(
    AIC = ifelse(AIC==min(AIC), paste0(round(AIC,0), "*"), as.character(round(AIC,0))),
    BIC = ifelse(BIC==min(BIC), paste0(round(BIC,0), "*"), as.character(round(BIC,0))),
    HQ = ifelse(HQ==min(HQ), paste0(round(HQ,0), "*"), as.character(round(HQ,0))),
    `Adjusted R2` = ifelse(`Adjusted R2`==max(`Adjusted R2`), paste0(round(`Adjusted R2`,5), "*"), as.character(round(`Adjusted R2`,5)))
  ) -> ic_w

cbind(
  map_dbl(models_i, AIC),
  map_dbl(models_i, BIC),
  map_dbl(models_i, function(x){
    -2 * logLik(x) + 2 * length(coef(x)) * log(log(nobs(x)))
  }),
  map_dbl(models_i, function(x){r2(x, type="ar2")})
) %>% 
  as_tibble() %>% 
  `colnames<-`(c("AIC", "BIC", "HQ", "Adjusted R2")) %>%
  mutate(
    AIC = ifelse(AIC==min(AIC), paste0(round(AIC,0), "*"), as.character(round(AIC,0))),
    BIC = ifelse(BIC==min(BIC), paste0(round(BIC,0), "*"), as.character(round(BIC,0))),
    HQ = ifelse(HQ==min(HQ), paste0(round(HQ,0), "*"), as.character(round(HQ,0))),
    `Adjusted R2` = ifelse(`Adjusted R2`==max(`Adjusted R2`), paste0(round(`Adjusted R2`,5), "*"), as.character(round(`Adjusted R2`,5)))
  ) -> ic_i

cbind(
  `Lag order` = c(0:10),
  ic_w, 
  ic_i
) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  add_header_above(c(" " = 1, 
                     "Annual Average Pay" = 4,
                     "Income per capita" = 4
  ), bold=T) %>% 
  writeLines(., "tables/info_crit.tex")
