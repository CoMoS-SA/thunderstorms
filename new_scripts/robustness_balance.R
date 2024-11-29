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

##### S1 ####
data$x<-data$S1
data$y<-data$S1

mod_null1 <- fixest::feols(
  fml = y ~ 1
  | Area + Year + factor(STATE)[time], 
  cluster = c("Area", "Year"),
  data = fixest::panel(data %>% filter(time> 2000), panel.id = c("Area", "Year"))
) 

for(i in 1:10){
  assign(
    paste0("mod1_",i),
    fixest::feols(
      fml = y ~ l(x,1:i)
      | Area + Year + factor(STATE)[time], 
      cluster = c("Area", "Year"),
      data = fixest::panel(data %>% filter(time> 2000-i), panel.id = c("Area", "Year"))
    ) 
  )
}

fit_table1 <- tibble(
  mod = 0:10,
  adjr2 = as.numeric(NA),
  RMSE = as.numeric(NA)
)

for(i in 0:10){
  if(i==0){
    mm <- mod_null1
  }else{
    mm <- get(paste0("mod1_",i))
  }
  
  fit_table1$RMSE[i+1] <- sqrt(mean((mm$residuals)^2))
  fit_table1$adjr2[i+1] <- r2(mm, "ar2")
}

fit_table1 %>% 
  mutate(
    adjr2_ch = (adjr2 - adjr2[mod==0]),
    RMSE_ch = ((RMSE - RMSE[mod==0])/RMSE[mod==0])*100,
  ) %>% 
  `colnames<-`(letters[1:5]) %>% 
  mutate(
    a=ifelse(a=="0", "", as.character(round(a,2))),
    b=as.character(round(b,3)),
    c=as.character(round(c,2)),
    d=ifelse(a=="", "", as.character(round(d,4))),
    e=ifelse(a=="", "", paste(as.character(round(e,2)),"\\%"))
  ) %>% 
  relocate(d, .before = c) %>% 
  add_row(a="", 
          b="Adjusted $R^2$", 
          d="",
          c ="RMSE",
          e ="",
          .after = 0
  ) %>% 
  add_row(a="Lag storm exposure ($p$)", 
          b="Adjusted $R^2$", 
          d="Adjusted $R^2$ (change w.r.t. \"Only FE\")",
          c ="RMSE",
          e ="RMSE (\\% change w.r.t. \"Only FE\")",
          .after = 2,
  ) %>% 
  kbl(escape = F, booktabs = T, linesep = "", col.names = NULL, align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  pack_rows("Only FE: $S_{i,t} =alpha_i + phi_t +lambda_{\\mathcal{S}_i}t$", 1, 1, escape=F,extra_latex_after = "\\addlinespace[0.5em]") %>%
  pack_rows("AR($p$) + FE: $S_{i,t} =\\sum_{\\ell=1}^{p} S_{i,t-\\ell} + alpha_i + phi_t +lambda_{\\mathcal{S}_i}t$", 3, 11, escape=F,extra_latex_after = "\\addlinespace[0.5em]") %>%
  row_spec(2, hline_after = T) %>% 
  row_spec(3, hline_after = T)  



##### S4 ####
data$x<-data$S4
data$y<-data$S4

mod_null <- fixest::feols(
  fml = y ~ 1
  | Area + Year + factor(STATE)[time], 
  cluster = c("Area", "Year"),
  data = fixest::panel(data %>% filter(time> 2000), panel.id = c("Area", "Year"))
) 

for(i in 1:10){
  assign(
    paste0("mod_",i),
    fixest::feols(
      fml = y ~ l(x,1:i)
      | Area + Year + factor(STATE)[time], 
      cluster = c("Area", "Year"),
      data = fixest::panel(data %>% filter(time> 2000-i), panel.id = c("Area", "Year"))
    ) 
  )
}

fit_table <- tibble(
  mod = 0:10,
  adjr2 = as.numeric(NA),
  RMSE = as.numeric(NA)
)

for(i in 0:10){
  if(i==0){
    mm <- mod_null
  }else{
    mm <- get(paste0("mod_",i))
  }
  
  fit_table$RMSE[i+1] <- sqrt(mean((mm$residuals)^2))
  fit_table$adjr2[i+1] <- r2(mm, "ar2")
}

fit_table %>% 
  mutate(
    adjr2_ch = (adjr2 - adjr2[mod==0]),
    RMSE_ch = ((RMSE - RMSE[mod==0])/RMSE[mod==0])*100,
  ) %>% 
  `colnames<-`(letters[1:5]) %>% 
  mutate(
    a=ifelse(a=="0", "", as.character(round(a,2))),
    b=as.character(round(b,3)),
    c=as.character(round(c,1)),
    d=ifelse(a=="", "", as.character(round(d,4))),
    e=ifelse(a=="", "", paste(as.character(round(e,2)),"\\%"))
  ) %>% 
  relocate(d, .before = c) %>% 
  add_row(a="", 
          b="Adjusted $R^2$", 
          d="",
          c ="RMSE",
          e ="",
          .after = 0
  ) %>% 
  add_row(a="Lag storm exposure ($p$)", 
          b="Adjusted $R^2$", 
          d="Adjusted $R^2$ (change w.r.t. \"Only FE\")",
          c ="RMSE",
          e ="RMSE (\\% change w.r.t. \"Only FE\")",
          .after = 2,
  ) %>% 
  kbl(escape = F, booktabs = T, linesep = "", col.names = NULL, align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  pack_rows("Only FE: $S_{i,t} =alpha_i + phi_t +lambda_{\\mathcal{S}_i}t$", 1, 1, escape=F,extra_latex_after = "\\addlinespace[0.5em]") %>%
  pack_rows("AR($p$) + FE: $S_{i,t} =\\sum_{\\ell=1}^{p} S_{i,t-\\ell} + alpha_i + phi_t +lambda_{\\mathcal{S}_i}t$", 3, 11, escape=F,extra_latex_after = "\\addlinespace[0.5em]") %>%
  row_spec(2, hline_after = T) %>% 
  row_spec(3, hline_after = T)  


##### Orthogonality ######

data %<>% 
  mutate(
    ACTIVE_POP = AAE_TOT_TOT/POPULATION
  ) %>% 
  mutate(
    GR_ACTIVE_POP = ifelse(time==1991, as.numeric(NA), ((ACTIVE_POP - dplyr::lag(ACTIVE_POP))/dplyr::lag(ACTIVE_POP))*100)
  )

data %<>% 
  mutate(
    GR_tmp = ifelse(Year=="1991", as.numeric(NA), ((tmp - dplyr::lag(tmp))/dplyr::lag(tmp))*100 ),
    GR_prec = ifelse(Year=="1991", as.numeric(NA), ((prec - dplyr::lag(prec))/dplyr::lag(prec))*100 )
  ) 

var <- c("AAE_TOT_TOT", "POPULATION", "ACTIVE_POP")

#### S4 ####

mods <- list()
for(i in 1:length(var)){
  formula <- as.formula(
    paste0(
      "S4 ~ l(", var[i], ",1) + l(", paste0("GR_",var[i]),
      ",2:12)| Area + Year + factor(STATE)[time]"
    )
  )
  
  mod <- fixest::feols(
    fml =formula,
    cluster = c("Area", "Year"),
    data = fixest::panel(data, panel.id = c("Area", "Year"))
  ) 
  
  mod$coeftable %>% 
    as_tibble() %>% 
    `colnames<-`(c("est", "se", "t", "p")) %>% 
    rowwise() %>% 
    mutate(st = stars_f(p)) %>% 
    ungroup() %>% 
    mutate(
      est=ifelse(abs(est)>=0.001, paste0(round(est,3),st), ifelse(est>=0, paste0("$<$0.001",st), paste0("$<$-0.001",st))),
      se=ifelse(se>=0.001, paste0("(",round(se,3),")"), "($<$0.001)")
    ) %>% 
    select(est,se) %>% 
    pivot_longer(cols = c(est, se), names_to = NULL, values_to = "value") %>% 
    #add_row(value=as.character(round(wald(mod, "^l", print=F)$p,3))) %>% 
    add_row(value=as.character(round(r2(mod, "ar2"),3))) %>% 
    add_row(value=as.character(round(r2(mod, "ar2") - r2(mod_null, "ar2"),3)  )) -> mods[[i]]
}


mod_all <- fixest::feols(
  fml =  S4 ~ l(AAE_TOT_TOT,1) + l(GR_AAE_TOT_TOT,2:12) +
    l(POPULATION,1) + l(GR_POPULATION,2:12) +
    l(ACTIVE_POP,1) + l(GR_ACTIVE_POP,2:12) 
  | Area + Year + factor(STATE)[time],
  cluster = c("Area", "Year"),
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 


#### S1 ####

mods1 <- list()
for(i in 1:length(var)){
  formula <- as.formula(
    paste0(
      "S1 ~ l(", var[i], ",1) + l(", paste0("GR_",var[i]),
      ",2:12)| Area + Year + factor(STATE)[time]"
    )
  )
  
  mod <- fixest::feols(
    fml =formula,
    cluster = c("Area", "Year"),
    data = fixest::panel(data, panel.id = c("Area", "Year"))
  ) 
  
  
  mod$coeftable %>% 
    as_tibble() %>% 
    `colnames<-`(c("est", "se", "t", "p")) %>% 
    rowwise() %>% 
    mutate(st = stars_f(p)) %>% 
    ungroup() %>% 
    mutate(
      est=ifelse(abs(est)>=0.001, paste0(round(est,3),st), ifelse(est>=0, paste0("$<$0.001",st), paste0("$<$-0.001",st))),
      se=ifelse(se>=0.001, paste0("(",round(se,3),")"), "($<$0.001)")
    ) %>% 
    select(est,se) %>% 
    pivot_longer(cols = c(est, se), names_to = NULL, values_to = "value") %>% 
    #add_row(value=as.character(round(wald(mod, "^l", print=F)$p,3))) %>% 
    add_row(value=as.character(round(r2(mod, "ar2"),3))) %>% 
    add_row(value=as.character(round(r2(mod, "ar2") - r2(mod_null1, "ar2"),3)  )) -> mods1[[i]]
}

mod_all1 <- fixest::feols(
  fml =  S1 ~ l(AAE_TOT_TOT,1) + l(GR_AAE_TOT_TOT,2:12) +
    l(POPULATION,1) + l(GR_POPULATION,2:12) +
    l(ACTIVE_POP,1) + l(GR_ACTIVE_POP,2:12) 
  | Area + Year + factor(STATE)[time],
  cluster = c("Area", "Year"),
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 


yy <- NULL
aux <- paste0("$X^{gr}_{t-",2:12,"}$")
for(i in 1:length(aux)){
  new <- c(aux[i], "")
  yy <- c(yy, new)
}
yy <- c("$X_{t-1}$", "", yy)

cbind(
  mods1 %>% 
    bind_cols() %>% 
    add_column(lag=c(yy, "Adjusted $R^2$", "Adjusted $R^2$ (change w.r.t. model with Fixed Effects only, $\\beta_1 \\dots \\beta_p=0$)"), .before=0) %>% 
    `colnames<-`( c(" ", "Employment", "Population", "Employment-Population ratio")),
  mods %>% 
    bind_cols() %>% 
    `colnames<-`( c("Employment", "Population", "Employment-Population ratio"))
) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  #pack_rows("Only FE: $S_{i,t} =alpha_i + phi_t +lambda_{\\mathcal{S}_i}t$", 1, 1, escape=F,extra_latex_after = "\\addlinespace[0.5em]") %>%
  #pack_rows("AR($p$) + FE: $S_{i,t} =\\sum_{\\ell=1}^{p} S_{i,t-\\ell} + alpha_i + phi_t +lambda_{\\mathcal{S}_i}t$", 3, 11, escape=F,extra_latex_after = "\\addlinespace[0.5em]") %>%
  row_spec(24, hline_after = T) %>% 
  add_header_above(c(" " = 1, 
                     "Storm count ($S^{(1)})$" = 3, 
                     "Squared wind speeds ($S^{(4)})$" = 3
  ), bold=T, escape = F)






