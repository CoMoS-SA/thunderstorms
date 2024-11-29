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

getcumul10_fs1 <- getcumul10_f

body(getcumul10_fs1)[[2]] <- substitute(
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
    vcovHC %<>% rename(var = ...1) %>% filter(var %in% c("S1", 
                                                         "S1_lag1", "S1_lag2", "S1_lag3", "S1_lag4", "S1_lag5", "S1_lag6", "S1_lag7", 
                                                         "S1_lag8", "S1_lag9", "S1_lag10")) %>% select(c("S1", "S1_lag1", 
                                                                                                         "S1_lag2", "S1_lag3", "S1_lag4", "S1_lag5", "S1_lag6", "S1_lag7", "S1_lag8", 
                                                                                                         "S1_lag9", "S1_lag10")) %>% as.matrix()
    colnames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", 
                          "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", 
                          "l(x, 9)", "l(x, 10)")
    rownames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", 
                          "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", 
                          "l(x, 9)", "l(x, 10)")
  }
)


getcumul10_fs2 <- getcumul10_f

body(getcumul10_fs2)[[2]] <- substitute(
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
    vcovHC %<>% rename(var = ...1) %>% filter(var %in% c("S2", 
                                                         "S2_lag1", "S2_lag2", "S2_lag3", "S2_lag4", "S2_lag5", "S2_lag6", "S2_lag7", 
                                                         "S2_lag8", "S2_lag9", "S2_lag10")) %>% select(c("S2", "S2_lag1", 
                                                                                                         "S2_lag2", "S2_lag3", "S2_lag4", "S2_lag5", "S2_lag6", "S2_lag7", "S2_lag8", 
                                                                                                         "S2_lag9", "S2_lag10")) %>% as.matrix()
    colnames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", 
                          "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", 
                          "l(x, 9)", "l(x, 10)")
    rownames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", 
                          "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", 
                          "l(x, 9)", "l(x, 10)")
  }
)


getcumul10_fs3 <- getcumul10_f

body(getcumul10_fs3)[[2]] <- substitute(
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
                                                         "S3_lag1", "S3_lag2", "S3_lag3", "S3_lag4", "S3_lag5", "S3_lag6", "S3_lag7", 
                                                         "S3_lag8", "S3_lag9", "S3_lag10")) %>% select(c("S3", "S3_lag1", 
                                                                                                         "S3_lag2", "S3_lag3", "S3_lag4", "S3_lag5", "S3_lag6", "S3_lag7", "S3_lag8", 
                                                                                                         "S3_lag9", "S3_lag10")) %>% as.matrix()
    colnames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", 
                          "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", 
                          "l(x, 9)", "l(x, 10)")
    rownames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", 
                          "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", 
                          "l(x, 9)", "l(x, 10)")
  }
)


data <- readRDS(file="data/f_panel_tr.rds")


data %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )


data$x<-data$S4
data$y<-data$GR_AAP_TOT_TOT

mod_w0 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_w0 <- getcumul10_f(mod_w0, data, "stata_rob/vcov_conley50_1lag.xlsx")

data$x<-data$S1
data$y<-data$GR_AAP_TOT_TOT

mod_w1 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 


cum_w1 <- getcumul10_fs1(mod_w1, data, "stata_rob/vcov_w_altexp_s1.xlsx")

data$x<-data$S2
data$y<-data$GR_AAP_TOT_TOT

mod_w2 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_w2 <- getcumul10_fs2(mod_w2, data, "stata_rob/vcov_w_altexp_s2.xlsx")

data$x<-data$S3
data$y<-data$GR_AAP_TOT_TOT

mod_w3 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_w3 <- getcumul10_fs3(mod_w3, data, "stata_rob/vcov_w_altexp_s3.xlsx")

#Income

data$x<-data$S4
data$y<-data$GR_INCOME_PC

mod_i0 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_i0 <- getcumul10_f(mod_i0, data, "stata_rob/income_vcov_conley50_1lag.xlsx")

data$x<-data$S1
data$y<-data$GR_INCOME_PC

mod_i1 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_i1 <- getcumul10_fs1(mod_i1, data, "stata_rob/vcov_i_altexp_s1.xlsx")

data$x<-data$S2
data$y<-data$GR_INCOME_PC

mod_i2 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_i2 <- getcumul10_fs2(mod_i2, data, "stata_rob/vcov_i_altexp_s2.xlsx")

data$x<-data$S3
data$y<-data$GR_INCOME_PC

mod_i3 <- fixest::feols(
  fml = y ~ l(x,0:10) +
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(data, panel.id = c("Area", "Year"))
) 
cum_i3 <- getcumul10_fs3(mod_i3, data, "stata_rob/vcov_i_altexp_s3.xlsx")


#### Chart ####


rbind(
  cum_w1 %>% add_column(var="S1") %>% add_row(.before=1, coeffsums_sd=0, lower=0, upper=0, year=-1, var="S1"),
  cum_w2 %>% add_column(var="S2") %>% add_row(.before=1, coeffsums_sd=0, lower=0, upper=0, year=-1, var="S2"),
  cum_w3 %>% add_column(var="S3") %>% add_row(.before=1, coeffsums_sd=0, lower=0, upper=0, year=-1, var="S3"),
  cum_w0 %>% add_column(var="S4") %>% add_row(.before=1, coeffsums_sd=0, lower=0, upper=0, year=-1, var="S4")
) %>% 
  mutate(title="Annual Average Pay") %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(aes(colour=factor(var)), linewidth=0.8) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(var)), linetype=5, alpha=0.25) +
  facet_wrap(title~.) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~exposure)) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_fill_manual(labels=expression(S^(1),S^(2),S^(3),S^(4)), values=c("#d7301f","#404040","#bababa","#f4a582")) +
  scale_color_manual(labels=expression(S^(1),S^(2),S^(3),S^(4)), values=c("#d7301f","#404040","#bababa","#f4a582")) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    strip.background = element_blank(),
    legend.key.width = unit(1, 'cm'),
    axis.text=element_text(size=11),
    axis.title=element_text(size=14),
    legend.text = element_text(size=12),
    strip.text = element_text(size=12)
  ) -> r_w


rbind(
  cum_i1 %>% add_column(var="S1") %>% add_row(.before=1, coeffsums_sd=0, lower=0, upper=0, year=-1, var="S1"),
  cum_i2 %>% add_column(var="S2") %>% add_row(.before=1, coeffsums_sd=0, lower=0, upper=0, year=-1, var="S2"),
  cum_i3 %>% add_column(var="S3") %>% add_row(.before=1, coeffsums_sd=0, lower=0, upper=0, year=-1, var="S3"),
  cum_i0 %>% add_column(var="S4") %>% add_row(.before=1, coeffsums_sd=0, lower=0, upper=0, year=-1, var="S4")
) %>% 
  mutate(title="Income per capita") %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(aes(colour=factor(var)), linewidth=0.8) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=factor(var)), linetype=5, alpha=0.25) +
  facet_wrap(title~.) +
  xlab("Years since storm exposure") +
  ylab(expression("% change per"~s.d.~of~exposure)) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_fill_manual(labels=expression(S^(1),S^(2),S^(3),S^(4)), values=c("#08519c","#404040","#bababa","#41b6c4")) +
  scale_color_manual(labels=expression(S^(1),S^(2),S^(3),S^(4)), values=c("#08519c","#404040","#bababa","#41b6c4")) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    strip.background = element_blank(),
    legend.key.width = unit(1, 'cm'),
    axis.text=element_text(size=11),
    axis.title=element_text(size=14),
    legend.text = element_text(size=12),
    strip.text = element_text(size=12)
  ) -> r_i

ggsave(paste0("charts/rob_s.pdf"), 
       cowplot::plot_grid(r_w, r_i, nrow=1, labels = c('A','B')),  
       width=11.8, height = 4.4)
