library(tidyverse)
library(magrittr)
library(stargazer)
library(ggplot2)
library(grDevices)
library(knitr)
library(kableExtra)
library(fixest)
library(readxl)
library(sf)
library(tictoc)


rm(list = ls())

source("new_scripts/functions_f.R")

getcumul10_f_x1 <-function(mod, dat, stata_vcov=NULL, stata_beta=NULL, var="x", clust="Area", ssc=T){
  
  if(is.null(stata_vcov)){
    if(ssc){
      vcovHC<-fixest::vcov_cluster(mod, cluster=clust)
    }else{
      vcovHC<-fixest::vcov_cluster(mod, cluster=clust,
                                   ssc = fixest::ssc(fixef.K = "none", adj = FALSE, cluster.adj = FALSE)
      )
    }
  }else{
    #read vcov matrix
    vcovHC <- read_excel(stata_vcov)
    
    vcovHC %<>% 
      rename(var=...1) %>% 
      filter(var%in%c("x_1","x1_lag1","x1_lag2","x1_lag3","x1_lag4","x1_lag5","x1_lag6","x1_lag7","x1_lag8","x1_lag9","x1_lag10")) %>% 
      select(c("x_1","x1_lag1","x1_lag2","x1_lag3","x1_lag4","x1_lag5","x1_lag6","x1_lag7","x1_lag8","x1_lag9","x1_lag10")) %>% 
      as.matrix()
    
    colnames(vcovHC) <- c("x_1", "l(x_1, 1)", "l(x_1, 2)", "l(x_1, 3)", "l(x_1, 4)", "l(x_1, 5)", "l(x_1, 6)", "l(x_1, 7)", "l(x_1, 8)", "l(x_1, 9)", "l(x_1, 10)")
    rownames(vcovHC) <- c("x_1", "l(x_1, 1)", "l(x_1, 2)", "l(x_1, 3)", "l(x_1, 4)", "l(x_1, 5)", "l(x_1, 6)", "l(x_1, 7)", "l(x_1, 8)", "l(x_1, 9)", "l(x_1, 10)") 
    
  }
  
  
  var1<-vcovHC["x_1","x_1"]
  var2<-vcovHC["x_1","x_1"]+vcovHC["l(x_1, 1)","l(x_1, 1)"]+
    2*vcovHC["x_1","l(x_1, 1)"]
  var3<-vcovHC["x_1","x_1"]+vcovHC["l(x_1, 1)","l(x_1, 1)"]+vcovHC["l(x_1, 2)","l(x_1, 2)"]+
    2*vcovHC["x_1","l(x_1, 1)"]+
    2*vcovHC["x_1","l(x_1, 2)"]+2*vcovHC["l(x_1, 1)","l(x_1, 2)"]
  var4<-vcovHC["x_1","x_1"]+vcovHC["l(x_1, 1)","l(x_1, 1)"]+vcovHC["l(x_1, 2)","l(x_1, 2)"]+vcovHC["l(x_1, 3)","l(x_1, 3)"]+
    2*vcovHC["x_1","l(x_1, 1)"]+
    2*vcovHC["x_1","l(x_1, 2)"]+2*vcovHC["l(x_1, 1)","l(x_1, 2)"]+
    2*vcovHC["x_1","l(x_1, 3)"]+2*vcovHC["l(x_1, 1)","l(x_1, 3)"]+2*vcovHC["l(x_1, 2)","l(x_1, 3)"]
  var5<-vcovHC["x_1","x_1"]+vcovHC["l(x_1, 1)","l(x_1, 1)"]+vcovHC["l(x_1, 2)","l(x_1, 2)"]+vcovHC["l(x_1, 3)","l(x_1, 3)"]+vcovHC["l(x_1, 4)","l(x_1, 4)"]+
    2*vcovHC["x_1","l(x_1, 1)"]+
    2*vcovHC["x_1","l(x_1, 2)"]+2*vcovHC["l(x_1, 1)","l(x_1, 2)"]+
    2*vcovHC["x_1","l(x_1, 3)"]+2*vcovHC["l(x_1, 1)","l(x_1, 3)"]+2*vcovHC["l(x_1, 2)","l(x_1, 3)"]+
    2*vcovHC["x_1","l(x_1, 4)"]+2*vcovHC["l(x_1, 1)","l(x_1, 4)"]+2*vcovHC["l(x_1, 2)","l(x_1, 4)"]+2*vcovHC["l(x_1, 3)","l(x_1, 4)"]
  var6<-vcovHC["x_1","x_1"]+vcovHC["l(x_1, 1)","l(x_1, 1)"]+vcovHC["l(x_1, 2)","l(x_1, 2)"]+vcovHC["l(x_1, 3)","l(x_1, 3)"]+vcovHC["l(x_1, 4)","l(x_1, 4)"]+vcovHC["l(x_1, 5)","l(x_1, 5)"]+
    2*vcovHC["x_1","l(x_1, 1)"]+
    2*vcovHC["x_1","l(x_1, 2)"]+2*vcovHC["l(x_1, 1)","l(x_1, 2)"]+
    2*vcovHC["x_1","l(x_1, 3)"]+2*vcovHC["l(x_1, 1)","l(x_1, 3)"]+2*vcovHC["l(x_1, 2)","l(x_1, 3)"]+
    2*vcovHC["x_1","l(x_1, 4)"]+2*vcovHC["l(x_1, 1)","l(x_1, 4)"]+2*vcovHC["l(x_1, 2)","l(x_1, 4)"]+2*vcovHC["l(x_1, 3)","l(x_1, 4)"]+
    2*vcovHC["x_1","l(x_1, 5)"]+2*vcovHC["l(x_1, 1)","l(x_1, 5)"]+2*vcovHC["l(x_1, 2)","l(x_1, 5)"]+2*vcovHC["l(x_1, 3)","l(x_1, 5)"]+2*vcovHC["l(x_1, 4)","l(x_1, 5)"]
  var7<-vcovHC["x_1","x_1"]+vcovHC["l(x_1, 1)","l(x_1, 1)"]+vcovHC["l(x_1, 2)","l(x_1, 2)"]+vcovHC["l(x_1, 3)","l(x_1, 3)"]+vcovHC["l(x_1, 4)","l(x_1, 4)"]+vcovHC["l(x_1, 5)","l(x_1, 5)"]+vcovHC["l(x_1, 6)","l(x_1, 6)"]+
    2*vcovHC["x_1","l(x_1, 1)"]+
    2*vcovHC["x_1","l(x_1, 2)"]+2*vcovHC["l(x_1, 1)","l(x_1, 2)"]+
    2*vcovHC["x_1","l(x_1, 3)"]+2*vcovHC["l(x_1, 1)","l(x_1, 3)"]+2*vcovHC["l(x_1, 2)","l(x_1, 3)"]+
    2*vcovHC["x_1","l(x_1, 4)"]+2*vcovHC["l(x_1, 1)","l(x_1, 4)"]+2*vcovHC["l(x_1, 2)","l(x_1, 4)"]+2*vcovHC["l(x_1, 3)","l(x_1, 4)"]+
    2*vcovHC["x_1","l(x_1, 5)"]+2*vcovHC["l(x_1, 1)","l(x_1, 5)"]+2*vcovHC["l(x_1, 2)","l(x_1, 5)"]+2*vcovHC["l(x_1, 3)","l(x_1, 5)"]+2*vcovHC["l(x_1, 4)","l(x_1, 5)"]+
    2*vcovHC["x_1","l(x_1, 6)"]+2*vcovHC["l(x_1, 1)","l(x_1, 6)"]+2*vcovHC["l(x_1, 2)","l(x_1, 6)"]+2*vcovHC["l(x_1, 3)","l(x_1, 6)"]+2*vcovHC["l(x_1, 4)","l(x_1, 6)"]+2*vcovHC["l(x_1, 5)","l(x_1, 6)"]
  var8<-vcovHC["x_1","x_1"]+vcovHC["l(x_1, 1)","l(x_1, 1)"]+vcovHC["l(x_1, 2)","l(x_1, 2)"]+vcovHC["l(x_1, 3)","l(x_1, 3)"]+vcovHC["l(x_1, 4)","l(x_1, 4)"]+vcovHC["l(x_1, 5)","l(x_1, 5)"]+vcovHC["l(x_1, 6)","l(x_1, 6)"]+vcovHC["l(x_1, 7)","l(x_1, 7)"]+
    2*vcovHC["x_1","l(x_1, 1)"]+
    2*vcovHC["x_1","l(x_1, 2)"]+2*vcovHC["l(x_1, 1)","l(x_1, 2)"]+
    2*vcovHC["x_1","l(x_1, 3)"]+2*vcovHC["l(x_1, 1)","l(x_1, 3)"]+2*vcovHC["l(x_1, 2)","l(x_1, 3)"]+
    2*vcovHC["x_1","l(x_1, 4)"]+2*vcovHC["l(x_1, 1)","l(x_1, 4)"]+2*vcovHC["l(x_1, 2)","l(x_1, 4)"]+2*vcovHC["l(x_1, 3)","l(x_1, 4)"]+
    2*vcovHC["x_1","l(x_1, 5)"]+2*vcovHC["l(x_1, 1)","l(x_1, 5)"]+2*vcovHC["l(x_1, 2)","l(x_1, 5)"]+2*vcovHC["l(x_1, 3)","l(x_1, 5)"]+2*vcovHC["l(x_1, 4)","l(x_1, 5)"]+
    2*vcovHC["x_1","l(x_1, 6)"]+2*vcovHC["l(x_1, 1)","l(x_1, 6)"]+2*vcovHC["l(x_1, 2)","l(x_1, 6)"]+2*vcovHC["l(x_1, 3)","l(x_1, 6)"]+2*vcovHC["l(x_1, 4)","l(x_1, 6)"]+2*vcovHC["l(x_1, 5)","l(x_1, 6)"]+
    2*vcovHC["x_1","l(x_1, 7)"]+2*vcovHC["l(x_1, 1)","l(x_1, 7)"]+2*vcovHC["l(x_1, 2)","l(x_1, 7)"]+2*vcovHC["l(x_1, 3)","l(x_1, 7)"]+2*vcovHC["l(x_1, 4)","l(x_1, 7)"]+2*vcovHC["l(x_1, 5)","l(x_1, 7)"]+2*vcovHC["l(x_1, 6)","l(x_1, 7)"]
  var9<-vcovHC["x_1","x_1"]+vcovHC["l(x_1, 1)","l(x_1, 1)"]+vcovHC["l(x_1, 2)","l(x_1, 2)"]+vcovHC["l(x_1, 3)","l(x_1, 3)"]+vcovHC["l(x_1, 4)","l(x_1, 4)"]+vcovHC["l(x_1, 5)","l(x_1, 5)"]+vcovHC["l(x_1, 6)","l(x_1, 6)"]+vcovHC["l(x_1, 7)","l(x_1, 7)"]+vcovHC["l(x_1, 8)","l(x_1, 8)"]+
    2*vcovHC["x_1","l(x_1, 1)"]+
    2*vcovHC["x_1","l(x_1, 2)"]+2*vcovHC["l(x_1, 1)","l(x_1, 2)"]+
    2*vcovHC["x_1","l(x_1, 3)"]+2*vcovHC["l(x_1, 1)","l(x_1, 3)"]+2*vcovHC["l(x_1, 2)","l(x_1, 3)"]+
    2*vcovHC["x_1","l(x_1, 4)"]+2*vcovHC["l(x_1, 1)","l(x_1, 4)"]+2*vcovHC["l(x_1, 2)","l(x_1, 4)"]+2*vcovHC["l(x_1, 3)","l(x_1, 4)"]+
    2*vcovHC["x_1","l(x_1, 5)"]+2*vcovHC["l(x_1, 1)","l(x_1, 5)"]+2*vcovHC["l(x_1, 2)","l(x_1, 5)"]+2*vcovHC["l(x_1, 3)","l(x_1, 5)"]+2*vcovHC["l(x_1, 4)","l(x_1, 5)"]+
    2*vcovHC["x_1","l(x_1, 6)"]+2*vcovHC["l(x_1, 1)","l(x_1, 6)"]+2*vcovHC["l(x_1, 2)","l(x_1, 6)"]+2*vcovHC["l(x_1, 3)","l(x_1, 6)"]+2*vcovHC["l(x_1, 4)","l(x_1, 6)"]+2*vcovHC["l(x_1, 5)","l(x_1, 6)"]+
    2*vcovHC["x_1","l(x_1, 7)"]+2*vcovHC["l(x_1, 1)","l(x_1, 7)"]+2*vcovHC["l(x_1, 2)","l(x_1, 7)"]+2*vcovHC["l(x_1, 3)","l(x_1, 7)"]+2*vcovHC["l(x_1, 4)","l(x_1, 7)"]+2*vcovHC["l(x_1, 5)","l(x_1, 7)"]+2*vcovHC["l(x_1, 6)","l(x_1, 7)"]+
    2*vcovHC["x_1","l(x_1, 8)"]+2*vcovHC["l(x_1, 1)","l(x_1, 8)"]+2*vcovHC["l(x_1, 2)","l(x_1, 8)"]+2*vcovHC["l(x_1, 3)","l(x_1, 8)"]+2*vcovHC["l(x_1, 4)","l(x_1, 8)"]+2*vcovHC["l(x_1, 5)","l(x_1, 8)"]+2*vcovHC["l(x_1, 6)","l(x_1, 8)"]+2*vcovHC["l(x_1, 7)","l(x_1, 8)"]
  var10<-vcovHC["x_1","x_1"]+vcovHC["l(x_1, 1)","l(x_1, 1)"]+vcovHC["l(x_1, 2)","l(x_1, 2)"]+vcovHC["l(x_1, 3)","l(x_1, 3)"]+vcovHC["l(x_1, 4)","l(x_1, 4)"]+vcovHC["l(x_1, 5)","l(x_1, 5)"]+vcovHC["l(x_1, 6)","l(x_1, 6)"]+vcovHC["l(x_1, 7)","l(x_1, 7)"]+vcovHC["l(x_1, 8)","l(x_1, 8)"]+vcovHC["l(x_1, 9)","l(x_1, 9)"]+
    2*vcovHC["x_1","l(x_1, 1)"]+
    2*vcovHC["x_1","l(x_1, 2)"]+2*vcovHC["l(x_1, 1)","l(x_1, 2)"]+
    2*vcovHC["x_1","l(x_1, 3)"]+2*vcovHC["l(x_1, 1)","l(x_1, 3)"]+2*vcovHC["l(x_1, 2)","l(x_1, 3)"]+
    2*vcovHC["x_1","l(x_1, 4)"]+2*vcovHC["l(x_1, 1)","l(x_1, 4)"]+2*vcovHC["l(x_1, 2)","l(x_1, 4)"]+2*vcovHC["l(x_1, 3)","l(x_1, 4)"]+
    2*vcovHC["x_1","l(x_1, 5)"]+2*vcovHC["l(x_1, 1)","l(x_1, 5)"]+2*vcovHC["l(x_1, 2)","l(x_1, 5)"]+2*vcovHC["l(x_1, 3)","l(x_1, 5)"]+2*vcovHC["l(x_1, 4)","l(x_1, 5)"]+
    2*vcovHC["x_1","l(x_1, 6)"]+2*vcovHC["l(x_1, 1)","l(x_1, 6)"]+2*vcovHC["l(x_1, 2)","l(x_1, 6)"]+2*vcovHC["l(x_1, 3)","l(x_1, 6)"]+2*vcovHC["l(x_1, 4)","l(x_1, 6)"]+2*vcovHC["l(x_1, 5)","l(x_1, 6)"]+
    2*vcovHC["x_1","l(x_1, 7)"]+2*vcovHC["l(x_1, 1)","l(x_1, 7)"]+2*vcovHC["l(x_1, 2)","l(x_1, 7)"]+2*vcovHC["l(x_1, 3)","l(x_1, 7)"]+2*vcovHC["l(x_1, 4)","l(x_1, 7)"]+2*vcovHC["l(x_1, 5)","l(x_1, 7)"]+2*vcovHC["l(x_1, 6)","l(x_1, 7)"]+
    2*vcovHC["x_1","l(x_1, 8)"]+2*vcovHC["l(x_1, 1)","l(x_1, 8)"]+2*vcovHC["l(x_1, 2)","l(x_1, 8)"]+2*vcovHC["l(x_1, 3)","l(x_1, 8)"]+2*vcovHC["l(x_1, 4)","l(x_1, 8)"]+2*vcovHC["l(x_1, 5)","l(x_1, 8)"]+2*vcovHC["l(x_1, 6)","l(x_1, 8)"]+2*vcovHC["l(x_1, 7)","l(x_1, 8)"]+
    2*vcovHC["x_1","l(x_1, 9)"]+2*vcovHC["l(x_1, 1)","l(x_1, 9)"]+2*vcovHC["l(x_1, 2)","l(x_1, 9)"]+2*vcovHC["l(x_1, 3)","l(x_1, 9)"]+2*vcovHC["l(x_1, 4)","l(x_1, 9)"]+2*vcovHC["l(x_1, 5)","l(x_1, 9)"]+2*vcovHC["l(x_1, 6)","l(x_1, 9)"]+2*vcovHC["l(x_1, 7)","l(x_1, 9)"]+2*vcovHC["l(x_1, 8)","l(x_1, 9)"]
  var11<-vcovHC["x_1","x_1"]+vcovHC["l(x_1, 1)","l(x_1, 1)"]+vcovHC["l(x_1, 2)","l(x_1, 2)"]+vcovHC["l(x_1, 3)","l(x_1, 3)"]+vcovHC["l(x_1, 4)","l(x_1, 4)"]+vcovHC["l(x_1, 5)","l(x_1, 5)"]+vcovHC["l(x_1, 6)","l(x_1, 6)"]+vcovHC["l(x_1, 7)","l(x_1, 7)"]+vcovHC["l(x_1, 8)","l(x_1, 8)"]+vcovHC["l(x_1, 9)","l(x_1, 9)"]+vcovHC["l(x_1, 10)","l(x_1, 10)"]+
    2*vcovHC["x_1","l(x_1, 1)"]+
    2*vcovHC["x_1","l(x_1, 2)"]+2*vcovHC["l(x_1, 1)","l(x_1, 2)"]+
    2*vcovHC["x_1","l(x_1, 3)"]+2*vcovHC["l(x_1, 1)","l(x_1, 3)"]+2*vcovHC["l(x_1, 2)","l(x_1, 3)"]+
    2*vcovHC["x_1","l(x_1, 4)"]+2*vcovHC["l(x_1, 1)","l(x_1, 4)"]+2*vcovHC["l(x_1, 2)","l(x_1, 4)"]+2*vcovHC["l(x_1, 3)","l(x_1, 4)"]+
    2*vcovHC["x_1","l(x_1, 5)"]+2*vcovHC["l(x_1, 1)","l(x_1, 5)"]+2*vcovHC["l(x_1, 2)","l(x_1, 5)"]+2*vcovHC["l(x_1, 3)","l(x_1, 5)"]+2*vcovHC["l(x_1, 4)","l(x_1, 5)"]+
    2*vcovHC["x_1","l(x_1, 6)"]+2*vcovHC["l(x_1, 1)","l(x_1, 6)"]+2*vcovHC["l(x_1, 2)","l(x_1, 6)"]+2*vcovHC["l(x_1, 3)","l(x_1, 6)"]+2*vcovHC["l(x_1, 4)","l(x_1, 6)"]+2*vcovHC["l(x_1, 5)","l(x_1, 6)"]+
    2*vcovHC["x_1","l(x_1, 7)"]+2*vcovHC["l(x_1, 1)","l(x_1, 7)"]+2*vcovHC["l(x_1, 2)","l(x_1, 7)"]+2*vcovHC["l(x_1, 3)","l(x_1, 7)"]+2*vcovHC["l(x_1, 4)","l(x_1, 7)"]+2*vcovHC["l(x_1, 5)","l(x_1, 7)"]+2*vcovHC["l(x_1, 6)","l(x_1, 7)"]+
    2*vcovHC["x_1","l(x_1, 8)"]+2*vcovHC["l(x_1, 1)","l(x_1, 8)"]+2*vcovHC["l(x_1, 2)","l(x_1, 8)"]+2*vcovHC["l(x_1, 3)","l(x_1, 8)"]+2*vcovHC["l(x_1, 4)","l(x_1, 8)"]+2*vcovHC["l(x_1, 5)","l(x_1, 8)"]+2*vcovHC["l(x_1, 6)","l(x_1, 8)"]+2*vcovHC["l(x_1, 7)","l(x_1, 8)"]+
    2*vcovHC["x_1","l(x_1, 9)"]+2*vcovHC["l(x_1, 1)","l(x_1, 9)"]+2*vcovHC["l(x_1, 2)","l(x_1, 9)"]+2*vcovHC["l(x_1, 3)","l(x_1, 9)"]+2*vcovHC["l(x_1, 4)","l(x_1, 9)"]+2*vcovHC["l(x_1, 5)","l(x_1, 9)"]+2*vcovHC["l(x_1, 6)","l(x_1, 9)"]+2*vcovHC["l(x_1, 7)","l(x_1, 9)"]+2*vcovHC["l(x_1, 8)","l(x_1, 9)"]+
    2*vcovHC["x_1","l(x_1, 10)"]+2*vcovHC["l(x_1, 1)","l(x_1, 10)"]+2*vcovHC["l(x_1, 2)","l(x_1, 10)"]+2*vcovHC["l(x_1, 3)","l(x_1, 10)"]+2*vcovHC["l(x_1, 4)","l(x_1, 10)"]+2*vcovHC["l(x_1, 5)","l(x_1, 10)"]+2*vcovHC["l(x_1, 6)","l(x_1, 10)"]+2*vcovHC["l(x_1, 7)","l(x_1, 10)"]+2*vcovHC["l(x_1, 8)","l(x_1, 10)"]+2*vcovHC["l(x_1, 9)","l(x_1, 10)"]
  varsums<-c(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11)
  sdsums<-varsums^0.5
  
  #estimates from feols or stata
  if(is.null(stata_vcov) | is.null(stata_beta)){
    singlecoeff<-mod$coefficients[c("x_1", "l(x_1, 1)", "l(x_1, 2)", "l(x_1, 3)", "l(x_1, 4)", "l(x_1, 5)", "l(x_1, 6)", "l(x_1, 7)", "l(x_1, 8)", "l(x_1, 9)", "l(x_1, 10)")]
  }else{ #read beta saved from stata
    beta <- read_csv(stata_beta, 
                     col_names = FALSE)
    
    singlecoeff<- beta %>% 
      slice(-(1:2)) %>% 
      mutate(
        X1 = gsub("[=|\"]", "", X1),
        X2 = gsub("[=|\"]", "", X2),
      ) %>% 
      mutate(X2 = gsub("\\*", "", X2)) %>% 
      filter(X1!="") %>% 
      filter(X1%in%c("x_1","x1_lag1","x1_lag2","x1_lag3","x1_lag4","x1_lag5","x1_lag6","x1_lag7","x1_lag8","x1_lag9","x1_lag10")) %>% 
      mutate(X2 = as.numeric(X2)) %>% 
      rename(var=X1, coefficients=X2) %>% filter(
        var %in% c("x_1","x1_lag1","x1_lag2","x1_lag3","x1_lag4","x1_lag5","x1_lag6","x1_lag7","x1_lag8","x1_lag9","x1_lag10")
      ) %>% pull(coefficients)
  }
  
  coeffsums<-cumsum(singlecoeff)
  
  coeffsums_sd<-coeffsums*sd(dat[,var] %>% pull())
  sdsums_sd<-sdsums*sd(dat[,var]%>% pull())
  
  lower<-coeffsums_sd-sdsums_sd*1.96
  upper<-coeffsums_sd+sdsums_sd*1.96
  datar<-cbind(coeffsums_sd,sdsums_sd,lower,upper, coeffsums, sdsums)
  datar<-as.data.frame(datar)
  datar$year <- 0:(nrow(datar)-1)
  return(datar)
}


data <- readRDS(file="data/f_panel_tr.rds")

data %<>% 
  mutate(
    time = as.numeric(as.character(Year))
  )

#load county shape

county_shape <- st_read("utilities/c_10nv20/") 

#fix non matching counties 
county_shape %<>% 
  mutate(
    FIPS=case_when(
      STATE=="VA" & COUNTYNAME %in% c("Albemarle", "City of Charlottesville") ~ "51901",
      STATE=="VA" & COUNTYNAME %in% c("Alleghany", "City of Covington") ~ "51903",
      STATE=="VA" & COUNTYNAME %in% c("Augusta", "City of Staunton", "City of Waynesboro") ~ "51907",
      STATE=="VA" & COUNTYNAME %in% c("Campbell", "City of Lynchburg") ~ "51911",
      STATE=="VA" & COUNTYNAME %in% c("Carroll", "City of Galax") ~ "51913",
      STATE=="VA" & COUNTYNAME %in% c("Dinwiddie", "City of Colonial Heights", "City of Petersburg") ~ "51918",
      STATE=="VA" & COUNTYNAME %in% c("Fairfax", "City of Fairfax", "City of Falls Church") ~ "51919",
      STATE=="VA" & COUNTYNAME %in% c("Frederick", "City of Winchester") ~ "51921",
      STATE=="VA" & COUNTYNAME %in% c("Greensville", "City of Emporia") ~ "51923",
      STATE=="VA" & COUNTYNAME %in% c("Henry", "City of Martinsville") ~ "51929",
      STATE=="VA" & COUNTYNAME %in% c("James City", "City of Williamsburg") ~ "51931",
      STATE=="VA" & COUNTYNAME %in% c("Montgomery", "City of Radford") ~ "51933",
      STATE=="VA" & COUNTYNAME %in% c("Pittsylvania", "City of Danville") ~ "51939",
      STATE=="VA" & COUNTYNAME %in% c("Prince George", "City of Hopewell") ~ "51941",
      STATE=="VA" & COUNTYNAME %in% c("Prince William", "City of Manassas", "City of Manassas Park") ~ "51942",
      STATE=="VA" & COUNTYNAME %in% c("Roanoke", "City of Salem") ~ "51944",
      STATE=="VA" & COUNTYNAME %in% c("Rockbridge", "City of Buena Vista", "City of Lexington") ~ "51945",
      STATE=="VA" & COUNTYNAME %in% c("Rockingham", "City of Harrisonburg") ~ "51947",
      STATE=="VA" & COUNTYNAME %in% c("Southampton", "City of Franklin") ~ "51949",
      STATE=="VA" & COUNTYNAME %in% c("Spotsylvania", "City of Fredericksburg") ~ "51951",
      STATE=="VA" & COUNTYNAME %in% c("Washington", "City of Bristol") ~ "51953",
      STATE=="VA" & COUNTYNAME %in% c("Wise", "City of Norton") ~ "51955",
      STATE=="VA" & COUNTYNAME %in% c("York", "City of Poquoson") ~ "51958",
      T ~ FIPS
    )
  )

#merge multiple geometries
county_shape %<>% 
  group_by(FIPS) %>% 
  summarise(
    geometry = st_union(geometry)
  ) %>% 
  filter(FIPS%in%unique(data$FIPS))


#remove zero neighbors
to_rem <- county_shape[c(181, 897, 937, 948, 954, 989, 1497, 1523, 1540, 1570, 1879, 2212, 2231, 2243, 2265, 2333),]$FIPS

data <- data %>% 
  filter(!(FIPS%in%to_rem))

county_shape <- county_shape %>% 
  filter(!(FIPS%in%to_rem))

data_shape <- county_shape %>% 
  select(FIPS, geometry) %>% 
  right_join(
    .,
    data,
    by="FIPS"
  )

county_centroid <- st_centroid(county_shape) 

neighb_d50 <- spdep::dnearneigh(x = st_transform(county_centroid, crs = 3857),d1 = 0, d2 = 50000)
neighb_d100 <- spdep::dnearneigh(x = st_transform(county_centroid, crs = 3857), d1 = 0, d2 = 100000)

l_d50 <- spdep::nb2listwdist(neighb_d50, st_transform(county_centroid, crs = 3857), style = "C", alpha=1, zero.policy = TRUE)
l_d100 <- spdep::nb2listwdist(neighb_d100, st_transform(county_centroid, crs = 3857), style = "C", alpha=1, zero.policy = TRUE)
l_d50_sq <- spdep::nb2listwdist(neighb_d50, st_transform(county_centroid, crs = 3857), style = "C", alpha=2, zero.policy = TRUE)


data_d50 <- plm::pdata.frame(data, index = c("FIPS", "Year"))
data_d50$x<-data_d50$S4
data_d50$x_1 <- splm::slag(data_d50$x, l_d50)
dt_d50 <- data_d50 %>% 
  as_tibble()

data_d100 <- plm::pdata.frame(data, index = c("FIPS", "Year"))
data_d100$x<-data_d100$S4
data_d100$x_1 <- splm::slag(data_d100$x, l_d100)
dt_d100 <- data_d100 %>% 
  as_tibble()

data_d50_sq <- plm::pdata.frame(data, index = c("FIPS", "Year"))
data_d50_sq$x<-data_d50_sq$S4
data_d50_sq$x_1 <- splm::slag(data_d50_sq$x, l_d50_sq)
dt_d50_sq <- data_d50_sq %>% 
  as_tibble()


# Save for stata 

data_stata <- dt_d50 %>% 
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
  ) %>% 
  mutate(
    x1_lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(x_1,1)),
    x1_lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(x_1,2)),
    x1_lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(x_1,3)),
    x1_lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(x_1,4)),
    x1_lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(x_1,5)),
    x1_lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(x_1,6)),
    x1_lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(x_1,7)),
    x1_lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(x_1,8)),
    x1_lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(x_1,9)),
    x1_lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(x_1,10))
  ) 

write.csv(data_stata, "data/panel_slx_d50.csv", row.names = FALSE)
haven::write_dta(data_stata, "stata_rob/panel_slx_d50.dta")


data_stata <- dt_d100 %>% 
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
  ) %>% 
  mutate(
    x1_lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(x_1,1)),
    x1_lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(x_1,2)),
    x1_lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(x_1,3)),
    x1_lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(x_1,4)),
    x1_lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(x_1,5)),
    x1_lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(x_1,6)),
    x1_lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(x_1,7)),
    x1_lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(x_1,8)),
    x1_lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(x_1,9)),
    x1_lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(x_1,10))
  ) 

write.csv(data_stata, "data/panel_slx_d100.csv", row.names = FALSE)
haven::write_dta(data_stata, "stata_rob/panel_slx_d100.dta")


data_stata <- dt_d50_sq %>% 
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
  ) %>% 
  mutate(
    x1_lag1 = ifelse(Year==1991, as.numeric(NA), dplyr::lag(x_1,1)),
    x1_lag2 = ifelse(Year==1992, as.numeric(NA), dplyr::lag(x_1,2)),
    x1_lag3 = ifelse(Year==1993, as.numeric(NA), dplyr::lag(x_1,3)),
    x1_lag4 = ifelse(Year==1994, as.numeric(NA), dplyr::lag(x_1,4)),
    x1_lag5 = ifelse(Year==1995, as.numeric(NA), dplyr::lag(x_1,5)),
    x1_lag6 = ifelse(Year==1996, as.numeric(NA), dplyr::lag(x_1,6)),
    x1_lag7 = ifelse(Year==1997, as.numeric(NA), dplyr::lag(x_1,7)),
    x1_lag8 = ifelse(Year==1998, as.numeric(NA), dplyr::lag(x_1,8)),
    x1_lag9 = ifelse(Year==1999, as.numeric(NA), dplyr::lag(x_1,9)),
    x1_lag10 = ifelse(Year==2000, as.numeric(NA), dplyr::lag(x_1,10))
  ) 

write.csv(data_stata, "data/panel_slx_d50_sq.csv", row.names = FALSE)
haven::write_dta(data_stata, "stata_rob/panel_slx_d50_sq.dta")


#### Estimation ####

#baseline

dt_d50$y<-dt_d50$GR_AAP_TOT_TOT

modw0 <- fixest::feols(
  fml = y ~ l(x,0:10) + 
    #l(x_1,0:10) + 
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_d50, panel.id = c("Area", "Year"))
) 

cumw0 <- getcumul10_f(modw0, dt_d50, "stata_rob/vcov_conley50_1lag.xlsx")

dt_d50$y<-dt_d50$GR_INCOME_PC

modi0 <- fixest::feols(
  fml = y ~ l(x,0:10) + 
    #l(x_1,0:10) + 
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_d50, panel.id = c("Area", "Year"))
) 

cumi0 <- getcumul10_f(modi0, dt_d50, "stata_rob/income_vcov_conley50_1lag.xlsx")

#d50

dt_d50$y<-dt_d50$GR_AAP_TOT_TOT

modw_d50 <- fixest::feols(
  fml = y ~ l(x,0:10) + 
    l(x_1,0:10) + 
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_d50, panel.id = c("Area", "Year"))
) 

cumw_d50 <- getcumul10_f(modw_d50, dt_d50, "stata_rob/vcov_w_slx_d50.xlsx")
cumw_d50_x1 <- getcumul10_f_x1(modw_d50, dt_d50, "stata_rob/vcov_w_slx_d50.xlsx")

dt_d50$y<-dt_d50$GR_INCOME_PC

modi_d50 <- fixest::feols(
  fml = y ~ l(x,0:10) + 
    l(x_1,0:10) + 
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_d50, panel.id = c("Area", "Year"))
) 

cumi_d50 <- getcumul10_f(modi_d50, dt_d50, "stata_rob/vcov_i_slx_d50.xlsx")
cumi_d50_x1 <- getcumul10_f_x1(modi_d50, dt_d50, "stata_rob/vcov_i_slx_d50.xlsx")


#d100

dt_d100$y<-dt_d100$GR_AAP_TOT_TOT

modw_d100 <- fixest::feols(
  fml = y ~ l(x,0:10) + 
    l(x_1,0:10) + 
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_d100, panel.id = c("Area", "Year"))
) 

cumw_d100 <- getcumul10_f(modw_d100, dt_d100, "stata_rob/vcov_w_slx_d100.xlsx")
cumw_d100_x1 <- getcumul10_f_x1(modw_d100, dt_d100, "stata_rob/vcov_w_slx_d100.xlsx")

dt_d100$y<-dt_d100$GR_INCOME_PC

modi_d100 <- fixest::feols(
  fml = y ~ l(x,0:10) + 
    l(x_1,0:10) + 
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_d100, panel.id = c("Area", "Year"))
) 

cumi_d100 <- getcumul10_f(modi_d100, dt_d100, "stata_rob/vcov_i_slx_d100.xlsx")
cumi_d100_x1 <- getcumul10_f_x1(modi_d100, dt_d100, "stata_rob/vcov_i_slx_d100.xlsx")


#d50_sq

dt_d50_sq$y<-dt_d50_sq$GR_AAP_TOT_TOT

modw_d50_sq <- fixest::feols(
  fml = y ~ l(x,0:10) + 
    l(x_1,0:10) + 
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_d50_sq, panel.id = c("Area", "Year"))
) 

cumw_d50_sq <- getcumul10_f(modw_d50_sq, dt_d50_sq, "stata_rob/vcov_w_slx_d50_sq.xlsx")
cumw_d50_sq_x1 <- getcumul10_f_x1(modw_d50_sq, dt_d50_sq, "stata_rob/vcov_w_slx_d50_sq.xlsx")

dt_d50_sq$y<-dt_d50_sq$GR_INCOME_PC

modi_d50_sq <- fixest::feols(
  fml = y ~ l(x,0:10) + 
    l(x_1,0:10) + 
    tmp + prec | Area + Year + factor(STATE)[time],
  data = fixest::panel(dt_d50_sq, panel.id = c("Area", "Year"))
) 

cumi_d50_sq <- getcumul10_f(modi_d50_sq, dt_d50_sq, "stata_rob/vcov_i_slx_d50_sq.xlsx")
cumi_d50_sq_x1 <- getcumul10_f_x1(modi_d50_sq, dt_d50_sq, "stata_rob/vcov_i_slx_d50_sq.xlsx")

#### Charts ####

rbind(
  cumw_d50 %>% add_row(coeffsums_sd=0, lower=0, upper=0, year=-1) %>% add_column(type="Direct") %>% add_column(var="Annual Average Pay"),
  cumw_d50_x1 %>% add_row(coeffsums_sd=0, lower=0, upper=0, year=-1) %>% add_column(type="Indirect") %>% add_column(var="Annual Average Pay")
) %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(linewidth=0.8, aes(color=factor(type))) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=year, fill=factor(type)), alpha=0.3) +
  xlab("Years since storm exposure") +
  facet_wrap(var~., scales = "free_y", nrow=1) +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_colour_manual(values = c("Direct"="#d7301f", "Indirect"="#fdae61")) +
  scale_fill_manual(values = c("Direct"="#d7301f", "Indirect"="#fdae61")) +
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
  ) -> gw_d50

rbind(
  cumi_d50 %>% add_row(coeffsums_sd=0, lower=0, upper=0, year=-1) %>% add_column(type="Direct") %>% add_column(var="Income per capita"),
  cumi_d50_x1 %>% add_row(coeffsums_sd=0, lower=0, upper=0, year=-1) %>% add_column(type="Indirect") %>% add_column(var="Income per capita")
) %>% 
  ggplot(aes(x=year, y=coeffsums_sd)) +
  geom_line(linewidth=0.8, aes(color=factor(type))) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=year, fill=factor(type)), alpha=0.3) +
  xlab("Years since storm exposure") +
  facet_wrap(var~., scales = "free_y", nrow=1) +
  ylab(expression("% change per"~s.d.~of~S^(4))) + 
  geom_hline(yintercept=0, linetype=3)+ 
  geom_vline(xintercept = 0)+
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_colour_manual(values = c("Direct"="#08519c", "Indirect"="#41b6c4")) +
  scale_fill_manual(values = c("Direct"="#08519c", "Indirect"="#41b6c4")) +
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
  ) -> gi_d50


g_spatial <- cowplot::plot_grid(gw_d50, gi_d50, nrow=1)

ggsave("charts/slx.pdf", 
       cowplot::plot_grid(gw_d50, gi_d50, nrow=1, labels = c('A','B')),  
       width=11.8, height = 4.4)


#### Table ####


yy <- NULL
for(i in 0:10){
  new <- c(i, "")
  yy <- c(yy, new)
}

cbind(
  tablize_f(cumw0, modw0),
  tablize_f(cumw_d50, modw_d50) %>% select(-year),
  tablize_f(cumw_d50_x1, modw_d50) %>% select(-year),
  tablize_f(cumw_d50_sq, modw_d50_sq) %>% select(-year),
  tablize_f(cumw_d50_sq_x1, modw_d50_sq) %>% select(-year),
  tablize_f(cumw_d100, modw_d100) %>% select(-year),
  tablize_f(cumw_d100_x1, modw_d100) %>% select(-year)
) %>% 
  `colnames<-`(letters[1:8]) %>% 
  mutate(a=yy) %>% 
  add_row(a="County fixed effects", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Year fixed effects",   b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="State-level trends",   b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Climatic controls",    b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Adjusted $R^2$", 
          b=as.character(round(r2(modw0, type="ar2"),3)), 
          c=as.character(round(r2(modw_d50, type="ar2"),3)),
          d=as.character(round(r2(modw_d50, type="ar2"),3)),
          e=as.character(round(r2(modw_d50_sq, type="ar2"),3)),
          f=as.character(round(r2(modw_d50_sq, type="ar2"),3)),
          g=as.character(round(r2(modw_d100, type="ar2"),3)),
          h=as.character(round(r2(modw_d100, type="ar2"),3))
  ) %>% 
  add_row(a="Observations", 
          b=as.character(nobs(modw0)), 
          c=as.character(nobs(modw_d50)),
          d=as.character(nobs(modw_d50)),
          e=as.character(nobs(modw_d50_sq)),
          f=as.character(nobs(modw_d50_sq)),
          g=as.character(nobs(modw_d100)),
          h=as.character(nobs(modw_d100))
  ) %>% 
  `colnames<-`(c("Years since storm exposure", "Direct", "Direct", "Indirect", "Direct", "Indirect", "Direct", "Indirect")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 6, hline_after = T) %>% 
  row_spec(10*2 + 8, hline_after = T) %>% 
  add_header_above(c(" " = 1, 
                     "Baseline (no SL)" = 1,
                     "50km, D^{-1}" = 2,
                     "50km, D^{-2}" = 2,
                     "100km, D^{-1}" = 2
  )) %>% 
  add_header_above(c(" " = 1, 
                     "Annual Average Pay" = 7
  ), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., "tables/slx_wages.tex")



cbind(
  #
  tablize_f(cumi0, modi0),
  tablize_f(cumi_d50, modi_d50) %>% select(-year),
  tablize_f(cumi_d50_x1, modi_d50) %>% select(-year),
  tablize_f(cumi_d50_sq, modi_d50_sq) %>% select(-year),
  tablize_f(cumi_d50_sq_x1, modi_d50_sq) %>% select(-year),
  tablize_f(cumi_d100, modi_d100) %>% select(-year),
  tablize_f(cumi_d100_x1, modi_d100) %>% select(-year)
) %>% 
  `colnames<-`(letters[1:8]) %>% 
  mutate(a=yy) %>% 
  add_row(a="County fixed effects", b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Year fixed effects",   b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="State-level trends",   b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Climatic controls",    b="\\checkmark", c="\\checkmark", d="\\checkmark", e="\\checkmark", f="\\checkmark", g="\\checkmark", h="\\checkmark") %>%
  add_row(a="Adjusted $R^2$", 
          b=as.character(round(r2(modi0, type="ar2"),3)), 
          c=as.character(round(r2(modi_d50, type="ar2"),3)),
          d=as.character(round(r2(modi_d50, type="ar2"),3)),
          e=as.character(round(r2(modi_d50_sq, type="ar2"),3)),
          f=as.character(round(r2(modi_d50_sq, type="ar2"),3)),
          g=as.character(round(r2(modi_d100, type="ar2"),3)),
          h=as.character(round(r2(modi_d100, type="ar2"),3))
  ) %>% 
  add_row(a="Observations", 
          b=as.character(nobs(modi0)), 
          c=as.character(nobs(modi_d50)),
          d=as.character(nobs(modi_d50)),
          e=as.character(nobs(modi_d50_sq)),
          f=as.character(nobs(modi_d50_sq)),
          g=as.character(nobs(modi_d100)),
          h=as.character(nobs(modi_d100))
  ) %>% 
  `colnames<-`(c("Years since storm exposure", "Direct", "Direct", "Indirect", "Direct", "Indirect", "Direct", "Indirect")) %>% 
  kbl(escape = F, booktabs = T, linesep = "", align = paste0("r",paste0(rep("c", ncol(.)-1), collapse = "")), format="latex") %>% 
  kable_styling(position = "center", latex_options = "scale_down") %>% 
  row_spec(10*2 + 2, hline_after = T) %>% 
  row_spec(10*2 + 6, hline_after = T) %>% 
  row_spec(10*2 + 8, hline_after = T) %>% 
  add_header_above(c(" " = 1, 
                     "Baseline (no SL)" = 1,
                     "50km, D^{-1}" = 2,
                     "50km, D^{-2}" = 2,
                     "100km, D^{-1}" = 2
  )) %>% 
  add_header_above(c(" " = 1, 
                     "Income per capita" = 7
  ), bold=T) %>% 
  footnote(general = "$^{***}$p$<0.001$, $^{**}$p$<0.01$, $^{*}$p$<0.05$, $.$p$<0.1$", footnote_as_chunk = T, escape=F) %>% 
  writeLines(., "tables/slx_income.tex")


