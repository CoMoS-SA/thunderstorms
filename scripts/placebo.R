library(tidyverse)
library(magrittr)
library(plm)
library(stargazer)
library(ggplot2)
library(grDevices)
library(knitr)
library(kableExtra)
library(tictoc)


rm(list = ls())

source("scripts/functions_f.R")

data <- readRDS(file="data/f_panel_tr.rds")


#### Baseline models ####
tic()
data %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )

rep <- 1000
x <- "S4"
yc <- c("GR_AAP_TOT_TOT", "GR_INCOME_PC")
ll <- "l(x, 10)"

ln <- "l(x, 10)"
st <- "l(x, 3)"

### Random permutations of data 

for(j in 1:length(yc)){
  y <- yc[j]
  
  set.seed(1)
  for(i in 1:rep){
    
    #randomize
    data %>% 
      select(Area, Year, prec, tmp, time, STATE, !!y, !!x) %>%  
      mutate(
        x_s := sample(get(x)),
        y_s = sample(get(y))
      ) %>% 
      group_by(Area) %>% 
      mutate(
        x_s_c = sample(get(x)),
        y_s_c = sample(get(y))
      ) %>% 
      ungroup() -> zz
    
    #Take randomized x full sample
    zz %<>% 
      mutate(
        x = x_s,
        y:= get(y)
      )
    
    mod <- fixest::feols(
      fml = y ~ l(x,0:10) +
        tmp + prec | Area + Year + factor(STATE)[time],
      data = fixest::panel(zz, panel.id = c("Area", "Year"))
    ) 
    
    b_x_ln <- getcumul10_f(mod, zz)[ln, "coeffsums_sd"]
    b_x_st <- getcumul10_f(mod, zz)[st, "coeffsums_sd"]
    
    #Take randomized x county_level
    zz %<>% 
      select(-"x",-"y") %>% 
      mutate(
        x = x_s_c,
        y:= get(y)
      )
    
    mod <- fixest::feols(
      fml = y ~ l(x,0:10) +
        tmp + prec | Area + Year + factor(STATE)[time],
      data = fixest::panel(zz, panel.id = c("Area", "Year"))
    ) 
    
    b_x_c_ln <- getcumul10_f(mod, zz)[ln, "coeffsums_sd"]
    b_x_c_st <- getcumul10_f(mod, zz)[st, "coeffsums_sd"]
    
    #Take randomized y full sample
    zz %<>% 
      select(-"x",-"y") %>% 
      mutate(
        x:= get(x),
        y = y_s
      )
    
    mod <- fixest::feols(
      fml = y ~ l(x,0:10) +
        tmp + prec | Area + Year + factor(STATE)[time],
      data = fixest::panel(zz, panel.id = c("Area", "Year"))
    ) 
    
    b_y_ln <- getcumul10_f(mod, zz)[ln, "coeffsums_sd"]
    b_y_st <- getcumul10_f(mod, zz)[st, "coeffsums_sd"]
    
    #Take randomized x county_level
    zz %<>% 
      select(-"x",-"y") %>% 
      mutate(
        x:= get(x),
        y= y_s_c
      )
    
    mod <- fixest::feols(
      fml = y ~ l(x,0:10) +
        tmp + prec | Area + Year + factor(STATE)[time],
      data = fixest::panel(zz, panel.id = c("Area", "Year"))
    ) 
    
    b_y_c_ln <- getcumul10_f(mod, zz)[ln, "coeffsums_sd"]
    b_y_c_st <- getcumul10_f(mod, zz)[st, "coeffsums_sd"]
    
    if(i==1){
      out_ln <- tibble(b_x_ln,b_x_c_ln,b_y_ln,b_y_c_ln)
      out_st <- tibble(b_x_st,b_x_c_st,b_y_st,b_y_c_st)
    }else{
      out_ln <- rbind(out_ln, tibble(b_x_ln,b_x_c_ln,b_y_ln,b_y_c_ln))
      out_st <- rbind(out_st, tibble(b_x_st,b_x_c_st,b_y_st,b_y_c_st))
    }
    print(i)
  }
  assign(paste0("out_ln_",yc[j]),out_ln)
  assign(paste0("out_st_",yc[j]),out_st)
}



dt <- data %>% 
  mutate(
    x=S4,
    y=GR_AAP_TOT_TOT
  )

mod <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt, panel.id = c("Area", "Year"))
) 

b_w_ln <- getcumul10_f(mod, dt)[ln, "coeffsums_sd"]
b_w_st <- getcumul10_f(mod, dt)[st, "coeffsums_sd"]

dt <- data %>% 
  mutate(
    x=S4,
    y=GR_INCOME_PC
  ) 

mod <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt, panel.id = c("Area", "Year"))
) 

b_i_ln <- getcumul10_f(mod, dt)[ln, "coeffsums_sd"]
b_i_st <- getcumul10_f(mod, dt)[st, "coeffsums_sd"]

rbind(
  out_ln_GR_AAP_TOT_TOT %>% 
    pivot_longer(everything()) %>% 
    add_column(var="wage") %>% 
    add_column(int = b_w_ln),
  out_ln_GR_INCOME_PC %>% 
    pivot_longer(everything()) %>% 
    add_column(var="income") %>% 
    add_column(int = b_i_ln)
) -> data_g_ln

rbind(
  out_st_GR_AAP_TOT_TOT %>% 
    pivot_longer(everything()) %>% 
    add_column(var="wage") %>% 
    add_column(int = b_w_st),
  out_st_GR_INCOME_PC %>% 
    pivot_longer(everything()) %>% 
    add_column(var="income") %>% 
    add_column(int = b_i_st)
) -> data_g_st

frac_c <- c(0.6,0.7,0.8,0.9)

for(j in 1:length(frac_c)){
  frac <- frac_c[j]
  
  set.seed(1)
  for(i in 1:rep){
    data %>% 
      filter(Area%in% sample(unique(data$Area), round(length(unique(data$Area))*frac)) ) -> zz
    
    zz %>% 
      mutate(
        x=S4,
        y=GR_AAP_TOT_TOT
      ) -> zz_w
    
    mod <- fixest::feols(
      fml = y ~ l(x,0:10) +
        tmp + prec | Area + Year + factor(STATE)[time],
      data = fixest::panel(zz_w, panel.id = c("Area", "Year"))
    ) 
    
    b_f_w_ln <- getcumul10_f(mod, zz_w)[ln, "coeffsums_sd"]
    b_f_w_st <- getcumul10_f(mod, zz_w)[st, "coeffsums_sd"]
    
    zz %>% 
      mutate(
        x=S4,
        y=GR_INCOME_PC
      ) -> zz_i
    
    mod <- fixest::feols(
      fml = y ~ l(x,0:10) +
        tmp + prec | Area + Year + factor(STATE)[time],
      data = fixest::panel(zz_i, panel.id = c("Area", "Year"))
    ) 
    
    b_f_i_ln <- getcumul10_f(mod, zz_i)[ln, "coeffsums_sd"]
    b_f_i_st <- getcumul10_f(mod, zz_i)[st, "coeffsums_sd"]
    
    if(i==1){
      out_ln <- tibble(b_f_w_ln,b_f_i_ln)
      out_st <- tibble(b_f_w_st,b_f_i_st)
    }else{
      out_ln <- rbind(out_ln, tibble(b_f_w_ln,b_f_i_ln))
      out_st <- rbind(out_st, tibble(b_f_w_st,b_f_i_st))
    }
    print(i)
  }
  assign(paste0("out_ln_",frac_c[j]),out_ln)
  assign(paste0("out_st_",frac_c[j]),out_st)
}

rbind(
  out_ln_0.6 %>% 
    add_column(frac="0.6"),
  out_ln_0.7 %>% 
    add_column(frac="0.7"),
  out_ln_0.8 %>% 
    add_column(frac="0.8"),
  out_ln_0.9 %>% 
    add_column(frac="0.9")
) %>% 
  pivot_longer(b_f_w_ln:b_f_i_ln) %>% 
  mutate(
    var=case_when(
      name=="b_f_w_ln" ~ b_w_ln,
      T ~ b_i_ln
    )
  ) -> data_r_ln

rbind(
  out_st_0.6 %>% 
    add_column(frac="0.6"),
  out_st_0.7 %>% 
    add_column(frac="0.7"),
  out_st_0.8 %>% 
    add_column(frac="0.8"),
  out_st_0.9 %>% 
    add_column(frac="0.9")
) %>% 
  pivot_longer(b_f_w_st:b_f_i_st) %>% 
  mutate(
    var=case_when(
      name=="b_f_w_st" ~ b_w_st,
      T ~ b_i_st
    )
  ) -> data_r_st
toc()

#### Manipulation ####

rbind(
  data_g_ln %>% add_column(horiz="long"),
  data_g_st %>% add_column(horiz="short")
) -> data_g

rbind(
  data_r_ln %>% add_column(horiz="long"),
  data_r_st %>% add_column(horiz="short")
) -> data_r

data_g %<>% 
  mutate(
    type = case_when(
      name %in% c("b_x_c_ln", "b_x_c_st") ~ "rewc",
      name %in% c("b_x_ln", "b_x_st") ~ "refs",
      name %in% c("b_y_c_ln", "b_y_c_st") ~ "rowc",
      T ~ "rofs"
    ),
    type=factor(
      type,
      levels = c("rewc", "refs", "rowc", "rofs"),
      labels = c("Random Exposure\n(Within-county)", "Random Exposure\n(Full Sample)", 
                 "Random\nEconomic Outcome\n(Within-county)", "Random\nEconomic Outcome\n(Full Sample)")  
    ),
    variable = factor(
      var,
      levels = c("wage", "income"),
      labels = c("Annual Average Pay", "Income per capita")
    ),
    horizon=factor(
      name,
      levels = c("b_x_c_ln", "b_x_ln", "b_y_c_ln", "b_y_ln",
                 "b_x_c_st", "b_x_st", "b_y_c_st", "b_y_st"),
      labels = c("Long run", "Long run", "Long run", "Long run",
                 "Short run", "Short run", "Short run", "Short run")  
    ),
    group = factor(
      paste(variable, horizon),
      levels = c("Annual Average Pay Short run", "Annual Average Pay Long run",
                 "Income per capita Short run", "Income per capita Long run"),
      labels = c("Annual Average Pay\n(Short Run)", "Annual Average Pay\n(Long Run)",
                 "Income per capita\n(Short Run)", "Income per capita\n(Long Run)")
    ),
    group2 = paste(variable, type)
  )

data_g %>% 
  group_by(group, type) %>% 
  summarize(
    p = t.test(x=value, mu=int[1], conf.level=0.95)$p.value
  ) %>% 
  mutate(
    p = ifelse(p<0.001, "<0.001", as.character(round(p,3)))
  ) %>% 
  mutate(
    x = case_when(
      group=="Annual Average Pay\n(Short Run)" ~ 0.076,
      group=="Annual Average Pay\n(Long Run)" ~ 0.19,
      group=="Income per capita\n(Short Run)" ~ 0.15,
      group=="Income per capita\n(Long Run)" ~ 0.23,
    ),
    y = case_when(
      type=="Random Exposure\n(Within-county)" ~ 12,
      type=="Random Exposure\n(Full Sample)" ~ 13.5,
      type=="Random\nEconomic Outcome\n(Within-county)" ~ 11,
      type=="Random\nEconomic Outcome\n(Full Sample)" ~ 10.5,
    )
  ) -> data_g_p

data_r %<>% 
  mutate(
    variable = ifelse(name%in%c("b_f_w_ln", "b_f_w_st"), "wage", "income"),
    variable = factor(
      variable,
      levels = c("wage", "income"),
      labels = c("Annual Average Pay", "Income per capita")
    ),
    int = var, 
    horizon=factor(
      name,
      levels = c("b_f_w_ln", "b_f_i_ln", "b_f_w_st", "b_f_i_st"),
      labels = c("Long run", "Long run", "Short run", "Short run")  
    ),
    fraction = factor(
      frac,
      levels = c("0.6", "0.7", "0.8", "0.9"),
      labels = c("60% Random\nCounties", "70% Random\nCounties", "80% Random\nCounties", "90% Random\nCounties")
    ), 
    group = factor(
      paste(variable, horizon),
      levels = c("Annual Average Pay Short run", "Annual Average Pay Long run",
                 "Income per capita Short run", "Income per capita Long run"),
      labels = c("Annual Average Pay\n(Short Run)", "Annual Average Pay\n(Long Run)",
                 "Income per capita\n(Short Run)", "Income per capita\n(Long Run)")
    )
  )

data_r %>% 
  group_by(group, fraction) %>% 
  summarize(
    p = t.test(x=value, mu=0, conf.level=0.95)$p.value
  ) %>% 
  mutate(
    p = ifelse(p<0.001, "<0.001", as.character(round(p,3)))
  ) %>% 
  mutate(
    x = case_when(
      group=="Annual Average Pay\n(Short Run)" ~ -0.034,
      group=="Annual Average Pay\n(Long Run)" ~ -0.05,
      group=="Income per capita\n(Short Run)" ~ -0.05,
      group=="Income per capita\n(Long Run)" ~ 0.12,
    ),
    y = case_when(
      fraction=="60% Random\nCounties" ~ 15,
      fraction=="70% Random\nCounties" ~ 18.5,
      fraction=="80% Random\nCounties" ~ 22,
      fraction=="90% Random\nCounties" ~ 36,
    )
  ) -> data_r_p

#### Charts ####
breaks_fun <- function(x) {
  if (max(x) <0.15) {
    c(-0.15, 0, 0.1)
  } else {
    if(min(x) < -0.15 & max(x)>0.22 & max(x)<0.29){
      c(-0.2, 0, 0.2)
    }else{
      if(min(x) < -0.15 & max(x)>0.22 & max(x)>0.29){
        c(-0.25, 0, 0.25)
      }else{
        c(-0.15, 0, 0.15)
      }
    }
  }
}

data_g %>% 
  ggplot() + 
  geom_density(aes(x=value, fill=group2, alpha=type)) + 
  geom_vline(aes(xintercept=int), color="indianred", linetype=2, linewidth=1) +
  geom_vline(aes(xintercept=0), linetype=3, linewidth=0.6) +
  scale_alpha_manual(values = rep(c(0.4,0.7),4)) +
  facet_grid(type~group, scales = "free",switch="y") +
  scale_x_continuous(breaks = breaks_fun, labels = function(x) paste0(x, '%')) +
  scale_fill_manual(values = c("#d7301f", "#d7301f", "#fdae61","#fdae61",  "#08519c","#08519c", "#41b6c4","#41b6c4")) +
  guides(alpha="none") +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    panel.spacing.x = unit(c(0.8,2,0.8), "line")
  ) -> g_g      


data_r %>% 
  ggplot() + 
  geom_density(aes(x=value, fill=variable, alpha=fraction)) + 
  geom_vline(aes(xintercept=0), color="indianred", linetype=2, linewidth=1) +
  geom_vline(aes(xintercept=int), linetype=3, linewidth=0.6) +
  scale_alpha_manual(values = rep(c(0.3,0.45, 0.6, 0.75),2)) +
  facet_grid(fraction~group, scales = "free",switch="y") +
  scale_x_continuous(labels = function(x) paste0(x, '%')) +
  scale_fill_manual(values = c("#d7301f","#08519c")) +
  guides(alpha="none") +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    panel.spacing.x = unit(c(0.8,2,0.8), "line")
  ) -> g_r

ggsave("charts/placebo_storm_10_base_trend.pdf", g_g, width=6.5, height = 4, scale=1.42)
ggsave("charts/radncounty_storm_10_base_trend.pdf", g_r, width=6.5, height = 4, scale=1.42)

