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

data <- readRDS(file="data/f_panel_tr.rds")

th95 = qnorm((1 + 0.95)/2)/sqrt(29)
th99 = qnorm((1 + 0.99)/2)/sqrt(29)

data %>% 
  group_by(Area) %>% 
  summarise(
    acf_w=list(as.numeric(acf(GR_AAP_TOT_TOT, plot=F)$acf)),
    pacf_w=list(as.numeric(pacf(GR_AAP_TOT_TOT, plot=F)$acf)),
    #
    acf_i=list(as.numeric(acf(GR_INCOME_PC, plot=F)$acf)),
    pacf_i=list(as.numeric(pacf(GR_INCOME_PC, plot=F)$acf)),
    #
    acf_s=list(as.numeric(acf(S4, plot=F)$acf)),
    pacf_s=list(as.numeric(pacf(S4, plot=F)$acf))
    ) %>% 
  mutate(
    ws_a_w_95 = map(acf_w, function(x){
      setdiff(which(abs(x)>th95), 1) -1
    }),
    ws_a_w_99 = map(acf_w, function(x){
      setdiff(which(abs(x)>th99), 1) -1
    }),
    ws_p_w_95 = map(pacf_w, function(x){
      which(abs(x)>th95)
    }),
    ws_p_w_99 = map(pacf_w, function(x){
      which(abs(x)>th99)
    }),
    #
    ws_a_i_95 = map(acf_i, function(x){
      setdiff(which(abs(x)>th95), 1) -1
    }),
    ws_a_i_99 = map(acf_i, function(x){
      setdiff(which(abs(x)>th99), 1) -1
    }),
    ws_p_i_95 = map(pacf_i, function(x){
      which(abs(x)>th95)
    }),
    ws_p_i_99 = map(pacf_i, function(x){
      which(abs(x)>th99)
    }),
    #
    ws_a_s_95 = map(acf_s, function(x){
      setdiff(which(abs(x)>th95), 1) -1
    }),
    ws_a_s_99 = map(acf_s, function(x){
      setdiff(which(abs(x)>th99), 1) -1
    }),
    ws_p_s_95 = map(pacf_s, function(x){
      which(abs(x)>th95)
    }),
    ws_p_s_99 = map(pacf_s, function(x){
      which(abs(x)>th99)
    })
  ) -> res

left_join(
  table(unlist(res$ws_a_w_95)) %>% data.frame() %>% as_tibble() %>% rename(ws_a_w_95=Freq),
  table(unlist(res$ws_a_w_99)) %>% data.frame() %>% as_tibble() %>% rename(ws_a_w_95=Freq),
  by = "Var1"
) %>% 
  left_join(.,
    table(unlist(res$ws_p_w_95)) %>% data.frame() %>% as_tibble() %>% rename(ws_p_w_95=Freq)
  ) %>% 
  left_join(.,
    table(unlist(res$ws_p_w_99)) %>% data.frame() %>% as_tibble() %>% rename(ws_p_w_99=Freq)
  ) %>% 
  #
  left_join(.,
    table(unlist(res$ws_a_i_95)) %>% data.frame() %>% as_tibble() %>% rename(ws_a_i_95=Freq)
  ) %>% 
  left_join(.,
    table(unlist(res$ws_a_i_99)) %>% data.frame() %>% as_tibble() %>% rename(ws_a_i_99=Freq)
  ) %>% 
  left_join(.,
    table(unlist(res$ws_p_i_95)) %>% data.frame() %>% as_tibble() %>% rename(ws_p_i_95=Freq)
  ) %>% 
  left_join(.,
    table(unlist(res$ws_p_i_99)) %>% data.frame() %>% as_tibble() %>% rename(ws_p_i_99=Freq)
  )%>% 
  #
  left_join(.,
            table(unlist(res$ws_a_s_95)) %>% data.frame() %>% as_tibble() %>% rename(ws_a_s_95=Freq)
  ) %>% 
  left_join(.,
            table(unlist(res$ws_a_s_99)) %>% data.frame() %>% as_tibble() %>% rename(ws_a_s_99=Freq)
  ) %>% 
  left_join(.,
            table(unlist(res$ws_p_s_95)) %>% data.frame() %>% as_tibble() %>% rename(ws_p_s_95=Freq)
  ) %>% 
  left_join(.,
            table(unlist(res$ws_p_s_99)) %>% data.frame() %>% as_tibble() %>% rename(ws_p_s_99=Freq)
  ) -> tb
  
tb %>% 
  mutate(across(starts_with("ws_"), ~ ifelse( is.na(.x),0 , round((.x/nrow(res))*100,2) ))) %>% 
  `colnames<-`(c("Lag", "95\\%", "99\\%", "95\\%", "99\\%",
                 "95\\%", "99\\%", "95\\%", "99\\%",
                 "95\\%", "99\\%", "95\\%", "99\\%")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  add_header_above(c(" " = 1, 
                     "ACF" = 2,
                     "PACF" = 2,
                     "ACF" = 2,
                     "PACF" = 2,
                     "ACF" = 2,
                     "PACF" = 2
  )) %>% 
  add_header_above(c(" " = 1, 
                     "Annual Average Pay" = 4,
                     "Income per Capita" = 4,
                     "Exposure (S4)" = 4
  ), bold=T) %>% 
  writeLines(., "tables/autocorrelation_table.tex")

