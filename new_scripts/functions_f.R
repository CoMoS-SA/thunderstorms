#### 10 lags ####
getcumul10_f <-function(mod, dat, stata_vcov=NULL, stata_beta=NULL, var="x", clust="Area", ssc=T){

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
      filter(var%in%c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10")) %>% 
      select(c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10")) %>% 
      as.matrix()
    
    colnames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", "l(x, 9)", "l(x, 10)")
    rownames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", "l(x, 9)", "l(x, 10)") 
    
  }
  
  var1<-vcovHC["x","x"]
  var2<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+
    2*vcovHC["x","l(x, 1)"]
  var3<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]
  var4<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]
  var5<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]
  var6<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]
  var7<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]
  var8<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]
  var9<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]
  var10<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]
  var11<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+vcovHC["l(x, 10)","l(x, 10)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]+
    2*vcovHC["x","l(x, 10)"]+2*vcovHC["l(x, 1)","l(x, 10)"]+2*vcovHC["l(x, 2)","l(x, 10)"]+2*vcovHC["l(x, 3)","l(x, 10)"]+2*vcovHC["l(x, 4)","l(x, 10)"]+2*vcovHC["l(x, 5)","l(x, 10)"]+2*vcovHC["l(x, 6)","l(x, 10)"]+2*vcovHC["l(x, 7)","l(x, 10)"]+2*vcovHC["l(x, 8)","l(x, 10)"]+2*vcovHC["l(x, 9)","l(x, 10)"]
  varsums<-c(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11)
  sdsums<-varsums^0.5
  
  #estimates from feols or stata
  if(is.null(stata_vcov) | is.null(stata_beta)){
    singlecoeff<-mod$coefficients[c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", "l(x, 9)", "l(x, 10)")]
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
      filter(X1%in%c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10")) %>% 
      mutate(X2 = as.numeric(X2)) %>% 
      rename(var=X1, coefficients=X2) %>% filter(
        var %in% c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10")
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

getcumul_d10_f<- function(mod, dat, stata_vcov=NULL, stata_beta=NULL, var="x", clust="Area", ssc=T){
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
      filter(var%in%c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10")) %>% 
      select(c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10")) %>% 
      as.matrix()
    
    colnames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", "l(x, 9)", "l(x, 10)")
    rownames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", "l(x, 9)", "l(x, 10)") 
    
  }
  
  var10<-vcovHC["x","x"]
  var20<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+
    2*vcovHC["x","l(x, 1)"]
  var30<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]
  var40<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]
  var50<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]
  var60<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]
  var70<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]
  var80<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]
  var90<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]
  var100<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]
  var110<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+vcovHC["l(x, 10)","l(x, 10)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]+
    2*vcovHC["x","l(x, 10)"]+2*vcovHC["l(x, 1)","l(x, 10)"]+2*vcovHC["l(x, 2)","l(x, 10)"]+2*vcovHC["l(x, 3)","l(x, 10)"]+2*vcovHC["l(x, 4)","l(x, 10)"]+2*vcovHC["l(x, 5)","l(x, 10)"]+2*vcovHC["l(x, 6)","l(x, 10)"]+2*vcovHC["l(x, 7)","l(x, 10)"]+2*vcovHC["l(x, 8)","l(x, 10)"]+2*vcovHC["l(x, 9)","l(x, 10)"]
  varsums0<-c(var10,var20,var30,var40,var50,var60,var70,var80,var90,var100,var110)
  sdsums0<-varsums0^0.5
  
  singlecoeff10<-mod$coefficients[c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", "l(x, 9)", "l(x, 10)")]
  
  coeffsums10<-cumsum(singlecoeff10)
  
  var11<-vcovHC["x","x"]+vcovHC["x:dmm","x:dmm"]+
    2*vcovHC["x","x:dmm"]
  var21<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):dmm","l(x, 1):dmm"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+
    2*vcovHC["x","l(x, 1):dmm"]+2*vcovHC["l(x, 1)","l(x, 1):dmm"]+2*vcovHC["x:dmm","l(x, 1):dmm"]
  var31<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):dmm","l(x, 1):dmm"]+vcovHC["l(x, 2):dmm","l(x, 2):dmm"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+
    2*vcovHC["x","l(x, 1):dmm"]+2*vcovHC["l(x, 1)","l(x, 1):dmm"]+2*vcovHC["l(x, 2)","l(x, 1):dmm"]+2*vcovHC["x:dmm","l(x, 1):dmm"]+
    2*vcovHC["x","l(x, 2):dmm"]+2*vcovHC["l(x, 1)","l(x, 2):dmm"]+2*vcovHC["l(x, 2)","l(x, 2):dmm"]+2*vcovHC["x:dmm","l(x, 2):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 2):dmm"]
  var41<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):dmm","l(x, 1):dmm"]+vcovHC["l(x, 2):dmm","l(x, 2):dmm"]+vcovHC["l(x, 3):dmm","l(x, 3):dmm"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+2*vcovHC["l(x, 3)","x:dmm"]+
    2*vcovHC["x","l(x, 1):dmm"]+2*vcovHC["l(x, 1)","l(x, 1):dmm"]+2*vcovHC["l(x, 2)","l(x, 1):dmm"]+2*vcovHC["l(x, 3)","l(x, 1):dmm"]+2*vcovHC["x:dmm","l(x, 1):dmm"]+
    2*vcovHC["x","l(x, 2):dmm"]+2*vcovHC["l(x, 1)","l(x, 2):dmm"]+2*vcovHC["l(x, 2)","l(x, 2):dmm"]+2*vcovHC["l(x, 3)","l(x, 2):dmm"]+2*vcovHC["x:dmm","l(x, 2):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 2):dmm"]+
    2*vcovHC["x","l(x, 3):dmm"]+2*vcovHC["l(x, 1)","l(x, 3):dmm"]+2*vcovHC["l(x, 2)","l(x, 3):dmm"]+2*vcovHC["l(x, 3)","l(x, 3):dmm"]+2*vcovHC["x:dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 3):dmm"]
  var51<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):dmm","l(x, 1):dmm"]+vcovHC["l(x, 2):dmm","l(x, 2):dmm"]+vcovHC["l(x, 3):dmm","l(x, 3):dmm"]+vcovHC["l(x, 4):dmm","l(x, 4):dmm"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+2*vcovHC["l(x, 3)","x:dmm"]+2*vcovHC["l(x, 4)","x:dmm"]+
    2*vcovHC["x","l(x, 1):dmm"]+2*vcovHC["l(x, 1)","l(x, 1):dmm"]+2*vcovHC["l(x, 2)","l(x, 1):dmm"]+2*vcovHC["l(x, 3)","l(x, 1):dmm"]+2*vcovHC["l(x, 4)","l(x, 1):dmm"]+2*vcovHC["x:dmm","l(x, 1):dmm"]+
    2*vcovHC["x","l(x, 2):dmm"]+2*vcovHC["l(x, 1)","l(x, 2):dmm"]+2*vcovHC["l(x, 2)","l(x, 2):dmm"]+2*vcovHC["l(x, 3)","l(x, 2):dmm"]+2*vcovHC["l(x, 4)","l(x, 2):dmm"]+2*vcovHC["x:dmm","l(x, 2):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 2):dmm"]+
    2*vcovHC["x","l(x, 3):dmm"]+2*vcovHC["l(x, 1)","l(x, 3):dmm"]+2*vcovHC["l(x, 2)","l(x, 3):dmm"]+2*vcovHC["l(x, 3)","l(x, 3):dmm"]+2*vcovHC["l(x, 4)","l(x, 3):dmm"]+2*vcovHC["x:dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 3):dmm"]+
    2*vcovHC["x","l(x, 4):dmm"]+2*vcovHC["l(x, 1)","l(x, 4):dmm"]+2*vcovHC["l(x, 2)","l(x, 4):dmm"]+2*vcovHC["l(x, 3)","l(x, 4):dmm"]+2*vcovHC["l(x, 4)","l(x, 4):dmm"]+2*vcovHC["x:dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 4):dmm"]
  
  var61<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):dmm","l(x, 1):dmm"]+vcovHC["l(x, 2):dmm","l(x, 2):dmm"]+vcovHC["l(x, 3):dmm","l(x, 3):dmm"]+vcovHC["l(x, 4):dmm","l(x, 4):dmm"]+vcovHC["l(x, 5):dmm","l(x, 5):dmm"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+2*vcovHC["l(x, 3)","x:dmm"]+2*vcovHC["l(x, 4)","x:dmm"]+2*vcovHC["l(x, 5)","x:dmm"]+
    2*vcovHC["x","l(x, 1):dmm"]+2*vcovHC["l(x, 1)","l(x, 1):dmm"]+2*vcovHC["l(x, 2)","l(x, 1):dmm"]+2*vcovHC["l(x, 3)","l(x, 1):dmm"]+2*vcovHC["l(x, 4)","l(x, 1):dmm"]+2*vcovHC["l(x, 5)","l(x, 1):dmm"]+2*vcovHC["x:dmm","l(x, 1):dmm"]+
    2*vcovHC["x","l(x, 2):dmm"]+2*vcovHC["l(x, 1)","l(x, 2):dmm"]+2*vcovHC["l(x, 2)","l(x, 2):dmm"]+2*vcovHC["l(x, 3)","l(x, 2):dmm"]+2*vcovHC["l(x, 4)","l(x, 2):dmm"]+2*vcovHC["l(x, 5)","l(x, 2):dmm"]+2*vcovHC["x:dmm","l(x, 2):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 2):dmm"]+
    2*vcovHC["x","l(x, 3):dmm"]+2*vcovHC["l(x, 1)","l(x, 3):dmm"]+2*vcovHC["l(x, 2)","l(x, 3):dmm"]+2*vcovHC["l(x, 3)","l(x, 3):dmm"]+2*vcovHC["l(x, 4)","l(x, 3):dmm"]+2*vcovHC["l(x, 5)","l(x, 3):dmm"]+2*vcovHC["x:dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 3):dmm"]+
    2*vcovHC["x","l(x, 4):dmm"]+2*vcovHC["l(x, 1)","l(x, 4):dmm"]+2*vcovHC["l(x, 2)","l(x, 4):dmm"]+2*vcovHC["l(x, 3)","l(x, 4):dmm"]+2*vcovHC["l(x, 4)","l(x, 4):dmm"]+2*vcovHC["l(x, 5)","l(x, 4):dmm"]+2*vcovHC["x:dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 4):dmm"]+
    2*vcovHC["x","l(x, 5):dmm"]+2*vcovHC["l(x, 1)","l(x, 5):dmm"]+2*vcovHC["l(x, 2)","l(x, 5):dmm"]+2*vcovHC["l(x, 3)","l(x, 5):dmm"]+2*vcovHC["l(x, 4)","l(x, 5):dmm"]+2*vcovHC["l(x, 5)","l(x, 5):dmm"]+2*vcovHC["x:dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 5):dmm"]
  var71<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):dmm","l(x, 1):dmm"]+vcovHC["l(x, 2):dmm","l(x, 2):dmm"]+vcovHC["l(x, 3):dmm","l(x, 3):dmm"]+vcovHC["l(x, 4):dmm","l(x, 4):dmm"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):dmm","l(x, 5):dmm"]+vcovHC["l(x, 6):dmm","l(x, 6):dmm"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+2*vcovHC["l(x, 3)","x:dmm"]+2*vcovHC["l(x, 4)","x:dmm"]+2*vcovHC["l(x, 5)","x:dmm"]+2*vcovHC["l(x, 6)","x:dmm"]+
    2*vcovHC["x","l(x, 1):dmm"]+2*vcovHC["l(x, 1)","l(x, 1):dmm"]+2*vcovHC["l(x, 2)","l(x, 1):dmm"]+2*vcovHC["l(x, 3)","l(x, 1):dmm"]+2*vcovHC["l(x, 4)","l(x, 1):dmm"]+2*vcovHC["l(x, 5)","l(x, 1):dmm"]+2*vcovHC["l(x, 6)","l(x, 1):dmm"]+2*vcovHC["x:dmm","l(x, 1):dmm"]+
    2*vcovHC["x","l(x, 2):dmm"]+2*vcovHC["l(x, 1)","l(x, 2):dmm"]+2*vcovHC["l(x, 2)","l(x, 2):dmm"]+2*vcovHC["l(x, 3)","l(x, 2):dmm"]+2*vcovHC["l(x, 4)","l(x, 2):dmm"]+2*vcovHC["l(x, 5)","l(x, 2):dmm"]+2*vcovHC["l(x, 6)","l(x, 2):dmm"]+2*vcovHC["x:dmm","l(x, 2):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 2):dmm"]+
    2*vcovHC["x","l(x, 3):dmm"]+2*vcovHC["l(x, 1)","l(x, 3):dmm"]+2*vcovHC["l(x, 2)","l(x, 3):dmm"]+2*vcovHC["l(x, 3)","l(x, 3):dmm"]+2*vcovHC["l(x, 4)","l(x, 3):dmm"]+2*vcovHC["l(x, 5)","l(x, 3):dmm"]+2*vcovHC["l(x, 6)","l(x, 3):dmm"]+2*vcovHC["x:dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 3):dmm"]+
    2*vcovHC["x","l(x, 4):dmm"]+2*vcovHC["l(x, 1)","l(x, 4):dmm"]+2*vcovHC["l(x, 2)","l(x, 4):dmm"]+2*vcovHC["l(x, 3)","l(x, 4):dmm"]+2*vcovHC["l(x, 4)","l(x, 4):dmm"]+2*vcovHC["l(x, 5)","l(x, 4):dmm"]+2*vcovHC["l(x, 6)","l(x, 4):dmm"]+2*vcovHC["x:dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 4):dmm"]+
    2*vcovHC["x","l(x, 5):dmm"]+2*vcovHC["l(x, 1)","l(x, 5):dmm"]+2*vcovHC["l(x, 2)","l(x, 5):dmm"]+2*vcovHC["l(x, 3)","l(x, 5):dmm"]+2*vcovHC["l(x, 4)","l(x, 5):dmm"]+2*vcovHC["l(x, 5)","l(x, 5):dmm"]+2*vcovHC["l(x, 6)","l(x, 5):dmm"]+2*vcovHC["x:dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 5):dmm"]+
    2*vcovHC["x","l(x, 6):dmm"]+2*vcovHC["l(x, 1)","l(x, 6):dmm"]+2*vcovHC["l(x, 2)","l(x, 6):dmm"]+2*vcovHC["l(x, 3)","l(x, 6):dmm"]+2*vcovHC["l(x, 4)","l(x, 6):dmm"]+2*vcovHC["l(x, 5)","l(x, 6):dmm"]+2*vcovHC["l(x, 6)","l(x, 6):dmm"]+2*vcovHC["x:dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 6):dmm"]
  var81<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):dmm","l(x, 1):dmm"]+vcovHC["l(x, 2):dmm","l(x, 2):dmm"]+vcovHC["l(x, 3):dmm","l(x, 3):dmm"]+vcovHC["l(x, 4):dmm","l(x, 4):dmm"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):dmm","l(x, 5):dmm"]+vcovHC["l(x, 6):dmm","l(x, 6):dmm"]+vcovHC["l(x, 7):dmm","l(x, 7):dmm"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+2*vcovHC["l(x, 3)","x:dmm"]+2*vcovHC["l(x, 4)","x:dmm"]+2*vcovHC["l(x, 5)","x:dmm"]+2*vcovHC["l(x, 6)","x:dmm"]+2*vcovHC["l(x, 7)","x:dmm"]+
    2*vcovHC["x","l(x, 1):dmm"]+2*vcovHC["l(x, 1)","l(x, 1):dmm"]+2*vcovHC["l(x, 2)","l(x, 1):dmm"]+2*vcovHC["l(x, 3)","l(x, 1):dmm"]+2*vcovHC["l(x, 4)","l(x, 1):dmm"]+2*vcovHC["l(x, 5)","l(x, 1):dmm"]+2*vcovHC["l(x, 6)","l(x, 1):dmm"]+2*vcovHC["l(x, 7)","l(x, 1):dmm"]+2*vcovHC["x:dmm","l(x, 1):dmm"]+
    2*vcovHC["x","l(x, 2):dmm"]+2*vcovHC["l(x, 1)","l(x, 2):dmm"]+2*vcovHC["l(x, 2)","l(x, 2):dmm"]+2*vcovHC["l(x, 3)","l(x, 2):dmm"]+2*vcovHC["l(x, 4)","l(x, 2):dmm"]+2*vcovHC["l(x, 5)","l(x, 2):dmm"]+2*vcovHC["l(x, 6)","l(x, 2):dmm"]+2*vcovHC["l(x, 7)","l(x, 2):dmm"]+2*vcovHC["x:dmm","l(x, 2):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 2):dmm"]+
    2*vcovHC["x","l(x, 3):dmm"]+2*vcovHC["l(x, 1)","l(x, 3):dmm"]+2*vcovHC["l(x, 2)","l(x, 3):dmm"]+2*vcovHC["l(x, 3)","l(x, 3):dmm"]+2*vcovHC["l(x, 4)","l(x, 3):dmm"]+2*vcovHC["l(x, 5)","l(x, 3):dmm"]+2*vcovHC["l(x, 6)","l(x, 3):dmm"]+2*vcovHC["l(x, 7)","l(x, 3):dmm"]+2*vcovHC["x:dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 3):dmm"]+
    2*vcovHC["x","l(x, 4):dmm"]+2*vcovHC["l(x, 1)","l(x, 4):dmm"]+2*vcovHC["l(x, 2)","l(x, 4):dmm"]+2*vcovHC["l(x, 3)","l(x, 4):dmm"]+2*vcovHC["l(x, 4)","l(x, 4):dmm"]+2*vcovHC["l(x, 5)","l(x, 4):dmm"]+2*vcovHC["l(x, 6)","l(x, 4):dmm"]+2*vcovHC["l(x, 7)","l(x, 4):dmm"]+2*vcovHC["x:dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 4):dmm"]+
    2*vcovHC["x","l(x, 5):dmm"]+2*vcovHC["l(x, 1)","l(x, 5):dmm"]+2*vcovHC["l(x, 2)","l(x, 5):dmm"]+2*vcovHC["l(x, 3)","l(x, 5):dmm"]+2*vcovHC["l(x, 4)","l(x, 5):dmm"]+2*vcovHC["l(x, 5)","l(x, 5):dmm"]+2*vcovHC["l(x, 6)","l(x, 5):dmm"]+2*vcovHC["l(x, 7)","l(x, 5):dmm"]+2*vcovHC["x:dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 5):dmm"]+
    2*vcovHC["x","l(x, 6):dmm"]+2*vcovHC["l(x, 1)","l(x, 6):dmm"]+2*vcovHC["l(x, 2)","l(x, 6):dmm"]+2*vcovHC["l(x, 3)","l(x, 6):dmm"]+2*vcovHC["l(x, 4)","l(x, 6):dmm"]+2*vcovHC["l(x, 5)","l(x, 6):dmm"]+2*vcovHC["l(x, 6)","l(x, 6):dmm"]+2*vcovHC["l(x, 7)","l(x, 6):dmm"]+2*vcovHC["x:dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 6):dmm"]+
    2*vcovHC["x","l(x, 7):dmm"]+2*vcovHC["l(x, 1)","l(x, 7):dmm"]+2*vcovHC["l(x, 2)","l(x, 7):dmm"]+2*vcovHC["l(x, 3)","l(x, 7):dmm"]+2*vcovHC["l(x, 4)","l(x, 7):dmm"]+2*vcovHC["l(x, 5)","l(x, 7):dmm"]+2*vcovHC["l(x, 6)","l(x, 7):dmm"]+2*vcovHC["l(x, 7)","l(x, 7):dmm"]+2*vcovHC["x:dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 6):dmm","l(x, 7):dmm"]
  var91<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):dmm","l(x, 1):dmm"]+vcovHC["l(x, 2):dmm","l(x, 2):dmm"]+vcovHC["l(x, 3):dmm","l(x, 3):dmm"]+vcovHC["l(x, 4):dmm","l(x, 4):dmm"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):dmm","l(x, 5):dmm"]+vcovHC["l(x, 6):dmm","l(x, 6):dmm"]+vcovHC["l(x, 7):dmm","l(x, 7):dmm"]+vcovHC["l(x, 8):dmm","l(x, 8):dmm"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+2*vcovHC["l(x, 3)","x:dmm"]+2*vcovHC["l(x, 4)","x:dmm"]+2*vcovHC["l(x, 5)","x:dmm"]+2*vcovHC["l(x, 6)","x:dmm"]+2*vcovHC["l(x, 7)","x:dmm"]+2*vcovHC["l(x, 8)","x:dmm"]+
    2*vcovHC["x","l(x, 1):dmm"]+2*vcovHC["l(x, 1)","l(x, 1):dmm"]+2*vcovHC["l(x, 2)","l(x, 1):dmm"]+2*vcovHC["l(x, 3)","l(x, 1):dmm"]+2*vcovHC["l(x, 4)","l(x, 1):dmm"]+2*vcovHC["l(x, 5)","l(x, 1):dmm"]+2*vcovHC["l(x, 6)","l(x, 1):dmm"]+2*vcovHC["l(x, 7)","l(x, 1):dmm"]+2*vcovHC["l(x, 8)","l(x, 1):dmm"]+2*vcovHC["x:dmm","l(x, 1):dmm"]+
    2*vcovHC["x","l(x, 2):dmm"]+2*vcovHC["l(x, 1)","l(x, 2):dmm"]+2*vcovHC["l(x, 2)","l(x, 2):dmm"]+2*vcovHC["l(x, 3)","l(x, 2):dmm"]+2*vcovHC["l(x, 4)","l(x, 2):dmm"]+2*vcovHC["l(x, 5)","l(x, 2):dmm"]+2*vcovHC["l(x, 6)","l(x, 2):dmm"]+2*vcovHC["l(x, 7)","l(x, 2):dmm"]+2*vcovHC["l(x, 8)","l(x, 2):dmm"]+2*vcovHC["x:dmm","l(x, 2):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 2):dmm"]+
    2*vcovHC["x","l(x, 3):dmm"]+2*vcovHC["l(x, 1)","l(x, 3):dmm"]+2*vcovHC["l(x, 2)","l(x, 3):dmm"]+2*vcovHC["l(x, 3)","l(x, 3):dmm"]+2*vcovHC["l(x, 4)","l(x, 3):dmm"]+2*vcovHC["l(x, 5)","l(x, 3):dmm"]+2*vcovHC["l(x, 6)","l(x, 3):dmm"]+2*vcovHC["l(x, 7)","l(x, 3):dmm"]+2*vcovHC["l(x, 8)","l(x, 3):dmm"]+2*vcovHC["x:dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 3):dmm"]+
    2*vcovHC["x","l(x, 4):dmm"]+2*vcovHC["l(x, 1)","l(x, 4):dmm"]+2*vcovHC["l(x, 2)","l(x, 4):dmm"]+2*vcovHC["l(x, 3)","l(x, 4):dmm"]+2*vcovHC["l(x, 4)","l(x, 4):dmm"]+2*vcovHC["l(x, 5)","l(x, 4):dmm"]+2*vcovHC["l(x, 6)","l(x, 4):dmm"]+2*vcovHC["l(x, 7)","l(x, 4):dmm"]+2*vcovHC["l(x, 8)","l(x, 4):dmm"]+2*vcovHC["x:dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 4):dmm"]+
    2*vcovHC["x","l(x, 5):dmm"]+2*vcovHC["l(x, 1)","l(x, 5):dmm"]+2*vcovHC["l(x, 2)","l(x, 5):dmm"]+2*vcovHC["l(x, 3)","l(x, 5):dmm"]+2*vcovHC["l(x, 4)","l(x, 5):dmm"]+2*vcovHC["l(x, 5)","l(x, 5):dmm"]+2*vcovHC["l(x, 6)","l(x, 5):dmm"]+2*vcovHC["l(x, 7)","l(x, 5):dmm"]+2*vcovHC["l(x, 8)","l(x, 5):dmm"]+2*vcovHC["x:dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 5):dmm"]+
    2*vcovHC["x","l(x, 6):dmm"]+2*vcovHC["l(x, 1)","l(x, 6):dmm"]+2*vcovHC["l(x, 2)","l(x, 6):dmm"]+2*vcovHC["l(x, 3)","l(x, 6):dmm"]+2*vcovHC["l(x, 4)","l(x, 6):dmm"]+2*vcovHC["l(x, 5)","l(x, 6):dmm"]+2*vcovHC["l(x, 6)","l(x, 6):dmm"]+2*vcovHC["l(x, 7)","l(x, 6):dmm"]+2*vcovHC["l(x, 8)","l(x, 6):dmm"]+2*vcovHC["x:dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 6):dmm"]+
    2*vcovHC["x","l(x, 7):dmm"]+2*vcovHC["l(x, 1)","l(x, 7):dmm"]+2*vcovHC["l(x, 2)","l(x, 7):dmm"]+2*vcovHC["l(x, 3)","l(x, 7):dmm"]+2*vcovHC["l(x, 4)","l(x, 7):dmm"]+2*vcovHC["l(x, 5)","l(x, 7):dmm"]+2*vcovHC["l(x, 6)","l(x, 7):dmm"]+2*vcovHC["l(x, 7)","l(x, 7):dmm"]+2*vcovHC["l(x, 8)","l(x, 7):dmm"]+2*vcovHC["x:dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 6):dmm","l(x, 7):dmm"]+
    2*vcovHC["x","l(x, 8):dmm"]+2*vcovHC["l(x, 1)","l(x, 8):dmm"]+2*vcovHC["l(x, 2)","l(x, 8):dmm"]+2*vcovHC["l(x, 3)","l(x, 8):dmm"]+2*vcovHC["l(x, 4)","l(x, 8):dmm"]+2*vcovHC["l(x, 5)","l(x, 8):dmm"]+2*vcovHC["l(x, 6)","l(x, 8):dmm"]+2*vcovHC["l(x, 7)","l(x, 8):dmm"]+2*vcovHC["l(x, 8)","l(x, 8):dmm"]+2*vcovHC["x:dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 6):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 7):dmm","l(x, 8):dmm"]
  var101<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):dmm","l(x, 1):dmm"]+vcovHC["l(x, 2):dmm","l(x, 2):dmm"]+vcovHC["l(x, 3):dmm","l(x, 3):dmm"]+vcovHC["l(x, 4):dmm","l(x, 4):dmm"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):dmm","l(x, 5):dmm"]+vcovHC["l(x, 6):dmm","l(x, 6):dmm"]+vcovHC["l(x, 7):dmm","l(x, 7):dmm"]+vcovHC["l(x, 8):dmm","l(x, 8):dmm"]+vcovHC["l(x, 9):dmm","l(x, 9):dmm"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+2*vcovHC["l(x, 3)","x:dmm"]+2*vcovHC["l(x, 4)","x:dmm"]+2*vcovHC["l(x, 5)","x:dmm"]+2*vcovHC["l(x, 6)","x:dmm"]+2*vcovHC["l(x, 7)","x:dmm"]+2*vcovHC["l(x, 8)","x:dmm"]+2*vcovHC["l(x, 9)","x:dmm"]+
    2*vcovHC["x","l(x, 1):dmm"]+2*vcovHC["l(x, 1)","l(x, 1):dmm"]+2*vcovHC["l(x, 2)","l(x, 1):dmm"]+2*vcovHC["l(x, 3)","l(x, 1):dmm"]+2*vcovHC["l(x, 4)","l(x, 1):dmm"]+2*vcovHC["l(x, 5)","l(x, 1):dmm"]+2*vcovHC["l(x, 6)","l(x, 1):dmm"]+2*vcovHC["l(x, 7)","l(x, 1):dmm"]+2*vcovHC["l(x, 8)","l(x, 1):dmm"]+2*vcovHC["l(x, 9)","l(x, 1):dmm"]+2*vcovHC["x:dmm","l(x, 1):dmm"]+
    2*vcovHC["x","l(x, 2):dmm"]+2*vcovHC["l(x, 1)","l(x, 2):dmm"]+2*vcovHC["l(x, 2)","l(x, 2):dmm"]+2*vcovHC["l(x, 3)","l(x, 2):dmm"]+2*vcovHC["l(x, 4)","l(x, 2):dmm"]+2*vcovHC["l(x, 5)","l(x, 2):dmm"]+2*vcovHC["l(x, 6)","l(x, 2):dmm"]+2*vcovHC["l(x, 7)","l(x, 2):dmm"]+2*vcovHC["l(x, 8)","l(x, 2):dmm"]+2*vcovHC["l(x, 9)","l(x, 2):dmm"]+2*vcovHC["x:dmm","l(x, 2):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 2):dmm"]+
    2*vcovHC["x","l(x, 3):dmm"]+2*vcovHC["l(x, 1)","l(x, 3):dmm"]+2*vcovHC["l(x, 2)","l(x, 3):dmm"]+2*vcovHC["l(x, 3)","l(x, 3):dmm"]+2*vcovHC["l(x, 4)","l(x, 3):dmm"]+2*vcovHC["l(x, 5)","l(x, 3):dmm"]+2*vcovHC["l(x, 6)","l(x, 3):dmm"]+2*vcovHC["l(x, 7)","l(x, 3):dmm"]+2*vcovHC["l(x, 8)","l(x, 3):dmm"]+2*vcovHC["l(x, 9)","l(x, 3):dmm"]+2*vcovHC["x:dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 3):dmm"]+
    2*vcovHC["x","l(x, 4):dmm"]+2*vcovHC["l(x, 1)","l(x, 4):dmm"]+2*vcovHC["l(x, 2)","l(x, 4):dmm"]+2*vcovHC["l(x, 3)","l(x, 4):dmm"]+2*vcovHC["l(x, 4)","l(x, 4):dmm"]+2*vcovHC["l(x, 5)","l(x, 4):dmm"]+2*vcovHC["l(x, 6)","l(x, 4):dmm"]+2*vcovHC["l(x, 7)","l(x, 4):dmm"]+2*vcovHC["l(x, 8)","l(x, 4):dmm"]+2*vcovHC["l(x, 9)","l(x, 4):dmm"]+2*vcovHC["x:dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 4):dmm"]+
    2*vcovHC["x","l(x, 5):dmm"]+2*vcovHC["l(x, 1)","l(x, 5):dmm"]+2*vcovHC["l(x, 2)","l(x, 5):dmm"]+2*vcovHC["l(x, 3)","l(x, 5):dmm"]+2*vcovHC["l(x, 4)","l(x, 5):dmm"]+2*vcovHC["l(x, 5)","l(x, 5):dmm"]+2*vcovHC["l(x, 6)","l(x, 5):dmm"]+2*vcovHC["l(x, 7)","l(x, 5):dmm"]+2*vcovHC["l(x, 8)","l(x, 5):dmm"]+2*vcovHC["l(x, 9)","l(x, 5):dmm"]+2*vcovHC["x:dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 5):dmm"]+
    2*vcovHC["x","l(x, 6):dmm"]+2*vcovHC["l(x, 1)","l(x, 6):dmm"]+2*vcovHC["l(x, 2)","l(x, 6):dmm"]+2*vcovHC["l(x, 3)","l(x, 6):dmm"]+2*vcovHC["l(x, 4)","l(x, 6):dmm"]+2*vcovHC["l(x, 5)","l(x, 6):dmm"]+2*vcovHC["l(x, 6)","l(x, 6):dmm"]+2*vcovHC["l(x, 7)","l(x, 6):dmm"]+2*vcovHC["l(x, 8)","l(x, 6):dmm"]+2*vcovHC["l(x, 9)","l(x, 6):dmm"]+2*vcovHC["x:dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 6):dmm"]+
    2*vcovHC["x","l(x, 7):dmm"]+2*vcovHC["l(x, 1)","l(x, 7):dmm"]+2*vcovHC["l(x, 2)","l(x, 7):dmm"]+2*vcovHC["l(x, 3)","l(x, 7):dmm"]+2*vcovHC["l(x, 4)","l(x, 7):dmm"]+2*vcovHC["l(x, 5)","l(x, 7):dmm"]+2*vcovHC["l(x, 6)","l(x, 7):dmm"]+2*vcovHC["l(x, 7)","l(x, 7):dmm"]+2*vcovHC["l(x, 8)","l(x, 7):dmm"]+2*vcovHC["l(x, 9)","l(x, 7):dmm"]+2*vcovHC["x:dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 6):dmm","l(x, 7):dmm"]+
    2*vcovHC["x","l(x, 8):dmm"]+2*vcovHC["l(x, 1)","l(x, 8):dmm"]+2*vcovHC["l(x, 2)","l(x, 8):dmm"]+2*vcovHC["l(x, 3)","l(x, 8):dmm"]+2*vcovHC["l(x, 4)","l(x, 8):dmm"]+2*vcovHC["l(x, 5)","l(x, 8):dmm"]+2*vcovHC["l(x, 6)","l(x, 8):dmm"]+2*vcovHC["l(x, 7)","l(x, 8):dmm"]+2*vcovHC["l(x, 8)","l(x, 8):dmm"]+2*vcovHC["l(x, 9)","l(x, 8):dmm"]+2*vcovHC["x:dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 6):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 7):dmm","l(x, 8):dmm"]+
    2*vcovHC["x","l(x, 9):dmm"]+2*vcovHC["l(x, 1)","l(x, 9):dmm"]+2*vcovHC["l(x, 2)","l(x, 9):dmm"]+2*vcovHC["l(x, 3)","l(x, 9):dmm"]+2*vcovHC["l(x, 4)","l(x, 9):dmm"]+2*vcovHC["l(x, 5)","l(x, 9):dmm"]+2*vcovHC["l(x, 6)","l(x, 9):dmm"]+2*vcovHC["l(x, 7)","l(x, 9):dmm"]+2*vcovHC["l(x, 8)","l(x, 9):dmm"]+2*vcovHC["l(x, 9)","l(x, 9):dmm"]+2*vcovHC["x:dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 6):dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 7):dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 8):dmm","l(x, 9):dmm"]
  var111<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+vcovHC["l(x, 10)","l(x, 10)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):dmm","l(x, 1):dmm"]+vcovHC["l(x, 2):dmm","l(x, 2):dmm"]+vcovHC["l(x, 3):dmm","l(x, 3):dmm"]+vcovHC["l(x, 4):dmm","l(x, 4):dmm"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):dmm","l(x, 5):dmm"]+vcovHC["l(x, 6):dmm","l(x, 6):dmm"]+vcovHC["l(x, 7):dmm","l(x, 7):dmm"]+vcovHC["l(x, 8):dmm","l(x, 8):dmm"]+vcovHC["l(x, 9):dmm","l(x, 9):dmm"]+vcovHC["l(x, 10):dmm","l(x, 10):dmm"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]+
    2*vcovHC["x","l(x, 10)"]+2*vcovHC["l(x, 1)","l(x, 10)"]+2*vcovHC["l(x, 2)","l(x, 10)"]+2*vcovHC["l(x, 3)","l(x, 10)"]+2*vcovHC["l(x, 4)","l(x, 10)"]+2*vcovHC["l(x, 5)","l(x, 10)"]+2*vcovHC["l(x, 6)","l(x, 10)"]+2*vcovHC["l(x, 7)","l(x, 10)"]+2*vcovHC["l(x, 8)","l(x, 10)"]+2*vcovHC["l(x, 9)","l(x, 10)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+2*vcovHC["l(x, 3)","x:dmm"]+2*vcovHC["l(x, 4)","x:dmm"]+2*vcovHC["l(x, 5)","x:dmm"]+2*vcovHC["l(x, 6)","x:dmm"]+2*vcovHC["l(x, 7)","x:dmm"]+2*vcovHC["l(x, 8)","x:dmm"]+2*vcovHC["l(x, 9)","x:dmm"]+2*vcovHC["l(x, 10)","x:dmm"]+
    2*vcovHC["x","l(x, 1):dmm"]+2*vcovHC["l(x, 1)","l(x, 1):dmm"]+2*vcovHC["l(x, 2)","l(x, 1):dmm"]+2*vcovHC["l(x, 3)","l(x, 1):dmm"]+2*vcovHC["l(x, 4)","l(x, 1):dmm"]+2*vcovHC["l(x, 5)","l(x, 1):dmm"]+2*vcovHC["l(x, 6)","l(x, 1):dmm"]+2*vcovHC["l(x, 7)","l(x, 1):dmm"]+2*vcovHC["l(x, 8)","l(x, 1):dmm"]+2*vcovHC["l(x, 9)","l(x, 1):dmm"]+2*vcovHC["l(x, 10)","l(x, 1):dmm"]+2*vcovHC["x:dmm","l(x, 1):dmm"]+
    2*vcovHC["x","l(x, 2):dmm"]+2*vcovHC["l(x, 1)","l(x, 2):dmm"]+2*vcovHC["l(x, 2)","l(x, 2):dmm"]+2*vcovHC["l(x, 3)","l(x, 2):dmm"]+2*vcovHC["l(x, 4)","l(x, 2):dmm"]+2*vcovHC["l(x, 5)","l(x, 2):dmm"]+2*vcovHC["l(x, 6)","l(x, 2):dmm"]+2*vcovHC["l(x, 7)","l(x, 2):dmm"]+2*vcovHC["l(x, 8)","l(x, 2):dmm"]+2*vcovHC["l(x, 9)","l(x, 2):dmm"]+2*vcovHC["l(x, 10)","l(x, 2):dmm"]+2*vcovHC["x:dmm","l(x, 2):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 2):dmm"]+
    2*vcovHC["x","l(x, 3):dmm"]+2*vcovHC["l(x, 1)","l(x, 3):dmm"]+2*vcovHC["l(x, 2)","l(x, 3):dmm"]+2*vcovHC["l(x, 3)","l(x, 3):dmm"]+2*vcovHC["l(x, 4)","l(x, 3):dmm"]+2*vcovHC["l(x, 5)","l(x, 3):dmm"]+2*vcovHC["l(x, 6)","l(x, 3):dmm"]+2*vcovHC["l(x, 7)","l(x, 3):dmm"]+2*vcovHC["l(x, 8)","l(x, 3):dmm"]+2*vcovHC["l(x, 9)","l(x, 3):dmm"]+2*vcovHC["l(x, 10)","l(x, 3):dmm"]+2*vcovHC["x:dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 3):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 3):dmm"]+
    2*vcovHC["x","l(x, 4):dmm"]+2*vcovHC["l(x, 1)","l(x, 4):dmm"]+2*vcovHC["l(x, 2)","l(x, 4):dmm"]+2*vcovHC["l(x, 3)","l(x, 4):dmm"]+2*vcovHC["l(x, 4)","l(x, 4):dmm"]+2*vcovHC["l(x, 5)","l(x, 4):dmm"]+2*vcovHC["l(x, 6)","l(x, 4):dmm"]+2*vcovHC["l(x, 7)","l(x, 4):dmm"]+2*vcovHC["l(x, 8)","l(x, 4):dmm"]+2*vcovHC["l(x, 9)","l(x, 4):dmm"]+2*vcovHC["l(x, 10)","l(x, 4):dmm"]+2*vcovHC["x:dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 4):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 4):dmm"]+
    2*vcovHC["x","l(x, 5):dmm"]+2*vcovHC["l(x, 1)","l(x, 5):dmm"]+2*vcovHC["l(x, 2)","l(x, 5):dmm"]+2*vcovHC["l(x, 3)","l(x, 5):dmm"]+2*vcovHC["l(x, 4)","l(x, 5):dmm"]+2*vcovHC["l(x, 5)","l(x, 5):dmm"]+2*vcovHC["l(x, 6)","l(x, 5):dmm"]+2*vcovHC["l(x, 7)","l(x, 5):dmm"]+2*vcovHC["l(x, 8)","l(x, 5):dmm"]+2*vcovHC["l(x, 9)","l(x, 5):dmm"]+2*vcovHC["l(x, 10)","l(x, 5):dmm"]+2*vcovHC["x:dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 5):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 5):dmm"]+
    2*vcovHC["x","l(x, 6):dmm"]+2*vcovHC["l(x, 1)","l(x, 6):dmm"]+2*vcovHC["l(x, 2)","l(x, 6):dmm"]+2*vcovHC["l(x, 3)","l(x, 6):dmm"]+2*vcovHC["l(x, 4)","l(x, 6):dmm"]+2*vcovHC["l(x, 5)","l(x, 6):dmm"]+2*vcovHC["l(x, 6)","l(x, 6):dmm"]+2*vcovHC["l(x, 7)","l(x, 6):dmm"]+2*vcovHC["l(x, 8)","l(x, 6):dmm"]+2*vcovHC["l(x, 9)","l(x, 6):dmm"]+2*vcovHC["l(x, 10)","l(x, 6):dmm"]+2*vcovHC["x:dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 6):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 6):dmm"]+
    2*vcovHC["x","l(x, 7):dmm"]+2*vcovHC["l(x, 1)","l(x, 7):dmm"]+2*vcovHC["l(x, 2)","l(x, 7):dmm"]+2*vcovHC["l(x, 3)","l(x, 7):dmm"]+2*vcovHC["l(x, 4)","l(x, 7):dmm"]+2*vcovHC["l(x, 5)","l(x, 7):dmm"]+2*vcovHC["l(x, 6)","l(x, 7):dmm"]+2*vcovHC["l(x, 7)","l(x, 7):dmm"]+2*vcovHC["l(x, 8)","l(x, 7):dmm"]+2*vcovHC["l(x, 9)","l(x, 7):dmm"]+2*vcovHC["l(x, 10)","l(x, 7):dmm"]+2*vcovHC["x:dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 7):dmm"]+2*vcovHC["l(x, 6):dmm","l(x, 7):dmm"]+
    2*vcovHC["x","l(x, 8):dmm"]+2*vcovHC["l(x, 1)","l(x, 8):dmm"]+2*vcovHC["l(x, 2)","l(x, 8):dmm"]+2*vcovHC["l(x, 3)","l(x, 8):dmm"]+2*vcovHC["l(x, 4)","l(x, 8):dmm"]+2*vcovHC["l(x, 5)","l(x, 8):dmm"]+2*vcovHC["l(x, 6)","l(x, 8):dmm"]+2*vcovHC["l(x, 7)","l(x, 8):dmm"]+2*vcovHC["l(x, 8)","l(x, 8):dmm"]+2*vcovHC["l(x, 9)","l(x, 8):dmm"]+2*vcovHC["l(x, 10)","l(x, 8):dmm"]+2*vcovHC["x:dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 6):dmm","l(x, 8):dmm"]+2*vcovHC["l(x, 7):dmm","l(x, 8):dmm"]+
    2*vcovHC["x","l(x, 9):dmm"]+2*vcovHC["l(x, 1)","l(x, 9):dmm"]+2*vcovHC["l(x, 2)","l(x, 9):dmm"]+2*vcovHC["l(x, 3)","l(x, 9):dmm"]+2*vcovHC["l(x, 4)","l(x, 9):dmm"]+2*vcovHC["l(x, 5)","l(x, 9):dmm"]+2*vcovHC["l(x, 6)","l(x, 9):dmm"]+2*vcovHC["l(x, 7)","l(x, 9):dmm"]+2*vcovHC["l(x, 8)","l(x, 9):dmm"]+2*vcovHC["l(x, 9)","l(x, 9):dmm"]+2*vcovHC["l(x, 10)","l(x, 9):dmm"]+2*vcovHC["x:dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 6):dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 7):dmm","l(x, 9):dmm"]+2*vcovHC["l(x, 8):dmm","l(x, 9):dmm"]+
    2*vcovHC["x","l(x, 10):dmm"]+2*vcovHC["l(x, 1)","l(x, 10):dmm"]+2*vcovHC["l(x, 2)","l(x, 10):dmm"]+2*vcovHC["l(x, 3)","l(x, 10):dmm"]+2*vcovHC["l(x, 4)","l(x, 10):dmm"]+2*vcovHC["l(x, 5)","l(x, 10):dmm"]+2*vcovHC["l(x, 6)","l(x, 10):dmm"]+2*vcovHC["l(x, 7)","l(x, 10):dmm"]+2*vcovHC["l(x, 8)","l(x, 10):dmm"]+2*vcovHC["l(x, 9)","l(x, 10):dmm"]+2*vcovHC["l(x, 10)","l(x, 10):dmm"]+2*vcovHC["x:dmm","l(x, 10):dmm"]+2*vcovHC["l(x, 1):dmm","l(x, 10):dmm"]+2*vcovHC["l(x, 2):dmm","l(x, 10):dmm"]+2*vcovHC["l(x, 3):dmm","l(x, 10):dmm"]+2*vcovHC["l(x, 4):dmm","l(x, 10):dmm"]+2*vcovHC["l(x, 5):dmm","l(x, 10):dmm"]+2*vcovHC["l(x, 6):dmm","l(x, 10):dmm"]+2*vcovHC["l(x, 7):dmm","l(x, 10):dmm"]+2*vcovHC["l(x, 8):dmm","l(x, 10):dmm"]+2*vcovHC["l(x, 9):dmm","l(x, 10):dmm"]
  
  varsums1<-c(var11,var21,var31,var41,var51,var61,var71,var81,var91,var101,var111)
  sdsums1<-varsums1^0.5
  
  singlecoeff11<-mod$coefficients[14:24]
  singlecoeff11<-mod$coefficients[c("x:dmm", "l(x, 1):dmm", "l(x, 2):dmm", "l(x, 3):dmm", "l(x, 4):dmm", "l(x, 5):dmm", "l(x, 6):dmm", "l(x, 7):dmm", "l(x, 8):dmm", "l(x, 9):dmm", "l(x, 10):dmm")]
  
  coeffsums11<-cumsum(singlecoeff11) + coeffsums10
  
  sdsums<-c(sdsums0,sdsums1)
  coeffsums<-c(coeffsums10,coeffsums11)
  coeffsums_sd<-coeffsums*sd(dat$x)
  sdsums_sd<-sdsums*sd(dat$x)
  
  lower<-coeffsums_sd-sdsums_sd*1.96
  upper<-coeffsums_sd+sdsums_sd*1.96
  
  datar<-cbind(coeffsums_sd,sdsums_sd,lower,upper, coeffsums, sdsums)
  datar<-as.data.frame(datar)
  datar$year <- rep(0:(nrow(datar)/2-1),2)
  
  datar$dmm <- 0
  datar$dmm[grepl("dmm", rownames(datar), fixed = TRUE)] <- 1
  
  return(datar)
}

getcumul_t10_f<- function(mod, dat, stata_vcov=NULL, stata_beta=NULL, var="x", clust="Area", ssc=T){
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
      filter(var%in%c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10",
                      "S4_tmm2", "S4_tmm3", "lag1_tmm2", "lag1_tmm3", "lag2_tmm2", "lag2_tmm3",
                      "lag3_tmm2", "lag3_tmm3", "lag4_tmm2", "lag4_tmm3", "lag5_tmm2", "lag5_tmm3",
                      "lag6_tmm2","lag6_tmm3", "lag7_tmm2", "lag7_tmm3", "lag8_tmm2", "lag8_tmm3",
                      "lag9_tmm2", "lag9_tmm3", "lag10_tmm2", "lag10_tmm3")) %>% 
      select(c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10",
               "S4_tmm2", "S4_tmm3", "lag1_tmm2", "lag1_tmm3", "lag2_tmm2", "lag2_tmm3",
               "lag3_tmm2", "lag3_tmm3", "lag4_tmm2", "lag4_tmm3", "lag5_tmm2", "lag5_tmm3",
               "lag6_tmm2","lag6_tmm3", "lag7_tmm2", "lag7_tmm3", "lag8_tmm2", "lag8_tmm3",
               "lag9_tmm2", "lag9_tmm3", "lag10_tmm2", "lag10_tmm3")) %>% 
      as.matrix()
    
    colnames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", "l(x, 9)", "l(x, 10)",
                          "x:tmm2", "x:tmm3","l(x, 1):tmm2", "l(x, 1):tmm3","l(x, 2):tmm2", "l(x, 2):tmm3","l(x, 3):tmm2", "l(x, 3):tmm3","l(x, 4):tmm2",
                          "l(x, 4):tmm3","l(x, 5):tmm2", "l(x, 5):tmm3","l(x, 6):tmm2", "l(x, 6):tmm3","l(x, 7):tmm2", "l(x, 7):tmm3","l(x, 8):tmm2",
                          "l(x, 8):tmm3","l(x, 9):tmm2", "l(x, 9):tmm3","l(x, 10):tmm2", "l(x, 10):tmm3")
    rownames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", "l(x, 9)", "l(x, 10)",
                          "x:tmm2", "x:tmm3","l(x, 1):tmm2", "l(x, 1):tmm3","l(x, 2):tmm2", "l(x, 2):tmm3","l(x, 3):tmm2", "l(x, 3):tmm3","l(x, 4):tmm2",
                          "l(x, 4):tmm3","l(x, 5):tmm2", "l(x, 5):tmm3","l(x, 6):tmm2", "l(x, 6):tmm3","l(x, 7):tmm2", "l(x, 7):tmm3","l(x, 8):tmm2",
                          "l(x, 8):tmm3","l(x, 9):tmm2", "l(x, 9):tmm3","l(x, 10):tmm2", "l(x, 10):tmm3") 
    
  }
  
  var10<-vcovHC["x","x"]
  var20<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+
    2*vcovHC["x","l(x, 1)"]
  var30<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]
  var40<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]
  var50<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]
  var60<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]
  var70<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]
  var80<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]
  var90<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]
  var100<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]
  var110<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+vcovHC["l(x, 10)","l(x, 10)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]+
    2*vcovHC["x","l(x, 10)"]+2*vcovHC["l(x, 1)","l(x, 10)"]+2*vcovHC["l(x, 2)","l(x, 10)"]+2*vcovHC["l(x, 3)","l(x, 10)"]+2*vcovHC["l(x, 4)","l(x, 10)"]+2*vcovHC["l(x, 5)","l(x, 10)"]+2*vcovHC["l(x, 6)","l(x, 10)"]+2*vcovHC["l(x, 7)","l(x, 10)"]+2*vcovHC["l(x, 8)","l(x, 10)"]+2*vcovHC["l(x, 9)","l(x, 10)"]
  varsums0<-c(var10,var20,var30,var40,var50,var60,var70,var80,var90,var100,var110)
  sdsums0<-varsums0^0.5
  
  singlecoeff10<-mod$coefficients[c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", "l(x, 9)", "l(x, 10)")]
  
  coeffsums10<-cumsum(singlecoeff10)
  
  var11<-vcovHC["x","x"]+vcovHC["x:tmm2","x:tmm2"]+
    2*vcovHC["x","x:tmm2"]
  var21<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["x:tmm2","x:tmm2"]+vcovHC["l(x, 1):tmm2","l(x, 1):tmm2"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","x:tmm2"]+2*vcovHC["l(x, 1)","x:tmm2"]+
    2*vcovHC["x","l(x, 1):tmm2"]+2*vcovHC["l(x, 1)","l(x, 1):tmm2"]+2*vcovHC["x:tmm2","l(x, 1):tmm2"]
  var31<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["x:tmm2","x:tmm2"]+vcovHC["l(x, 1):tmm2","l(x, 1):tmm2"]+vcovHC["l(x, 2):tmm2","l(x, 2):tmm2"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","x:tmm2"]+2*vcovHC["l(x, 1)","x:tmm2"]+2*vcovHC["l(x, 2)","x:tmm2"]+
    2*vcovHC["x","l(x, 1):tmm2"]+2*vcovHC["l(x, 1)","l(x, 1):tmm2"]+2*vcovHC["l(x, 2)","l(x, 1):tmm2"]+2*vcovHC["x:tmm2","l(x, 1):tmm2"]+
    2*vcovHC["x","l(x, 2):tmm2"]+2*vcovHC["l(x, 1)","l(x, 2):tmm2"]+2*vcovHC["l(x, 2)","l(x, 2):tmm2"]+2*vcovHC["x:tmm2","l(x, 2):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 2):tmm2"]
  var41<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["x:tmm2","x:tmm2"]+vcovHC["l(x, 1):tmm2","l(x, 1):tmm2"]+vcovHC["l(x, 2):tmm2","l(x, 2):tmm2"]+vcovHC["l(x, 3):tmm2","l(x, 3):tmm2"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","x:tmm2"]+2*vcovHC["l(x, 1)","x:tmm2"]+2*vcovHC["l(x, 2)","x:tmm2"]+2*vcovHC["l(x, 3)","x:tmm2"]+
    2*vcovHC["x","l(x, 1):tmm2"]+2*vcovHC["l(x, 1)","l(x, 1):tmm2"]+2*vcovHC["l(x, 2)","l(x, 1):tmm2"]+2*vcovHC["l(x, 3)","l(x, 1):tmm2"]+2*vcovHC["x:tmm2","l(x, 1):tmm2"]+
    2*vcovHC["x","l(x, 2):tmm2"]+2*vcovHC["l(x, 1)","l(x, 2):tmm2"]+2*vcovHC["l(x, 2)","l(x, 2):tmm2"]+2*vcovHC["l(x, 3)","l(x, 2):tmm2"]+2*vcovHC["x:tmm2","l(x, 2):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 2):tmm2"]+
    2*vcovHC["x","l(x, 3):tmm2"]+2*vcovHC["l(x, 1)","l(x, 3):tmm2"]+2*vcovHC["l(x, 2)","l(x, 3):tmm2"]+2*vcovHC["l(x, 3)","l(x, 3):tmm2"]+2*vcovHC["x:tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 3):tmm2"]
  var51<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["x:tmm2","x:tmm2"]+vcovHC["l(x, 1):tmm2","l(x, 1):tmm2"]+vcovHC["l(x, 2):tmm2","l(x, 2):tmm2"]+vcovHC["l(x, 3):tmm2","l(x, 3):tmm2"]+vcovHC["l(x, 4):tmm2","l(x, 4):tmm2"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","x:tmm2"]+2*vcovHC["l(x, 1)","x:tmm2"]+2*vcovHC["l(x, 2)","x:tmm2"]+2*vcovHC["l(x, 3)","x:tmm2"]+2*vcovHC["l(x, 4)","x:tmm2"]+
    2*vcovHC["x","l(x, 1):tmm2"]+2*vcovHC["l(x, 1)","l(x, 1):tmm2"]+2*vcovHC["l(x, 2)","l(x, 1):tmm2"]+2*vcovHC["l(x, 3)","l(x, 1):tmm2"]+2*vcovHC["l(x, 4)","l(x, 1):tmm2"]+2*vcovHC["x:tmm2","l(x, 1):tmm2"]+
    2*vcovHC["x","l(x, 2):tmm2"]+2*vcovHC["l(x, 1)","l(x, 2):tmm2"]+2*vcovHC["l(x, 2)","l(x, 2):tmm2"]+2*vcovHC["l(x, 3)","l(x, 2):tmm2"]+2*vcovHC["l(x, 4)","l(x, 2):tmm2"]+2*vcovHC["x:tmm2","l(x, 2):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 2):tmm2"]+
    2*vcovHC["x","l(x, 3):tmm2"]+2*vcovHC["l(x, 1)","l(x, 3):tmm2"]+2*vcovHC["l(x, 2)","l(x, 3):tmm2"]+2*vcovHC["l(x, 3)","l(x, 3):tmm2"]+2*vcovHC["l(x, 4)","l(x, 3):tmm2"]+2*vcovHC["x:tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 3):tmm2"]+
    2*vcovHC["x","l(x, 4):tmm2"]+2*vcovHC["l(x, 1)","l(x, 4):tmm2"]+2*vcovHC["l(x, 2)","l(x, 4):tmm2"]+2*vcovHC["l(x, 3)","l(x, 4):tmm2"]+2*vcovHC["l(x, 4)","l(x, 4):tmm2"]+2*vcovHC["x:tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 4):tmm2"]
  
  var61<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["x:tmm2","x:tmm2"]+vcovHC["l(x, 1):tmm2","l(x, 1):tmm2"]+vcovHC["l(x, 2):tmm2","l(x, 2):tmm2"]+vcovHC["l(x, 3):tmm2","l(x, 3):tmm2"]+vcovHC["l(x, 4):tmm2","l(x, 4):tmm2"]+vcovHC["l(x, 5):tmm2","l(x, 5):tmm2"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","x:tmm2"]+2*vcovHC["l(x, 1)","x:tmm2"]+2*vcovHC["l(x, 2)","x:tmm2"]+2*vcovHC["l(x, 3)","x:tmm2"]+2*vcovHC["l(x, 4)","x:tmm2"]+2*vcovHC["l(x, 5)","x:tmm2"]+
    2*vcovHC["x","l(x, 1):tmm2"]+2*vcovHC["l(x, 1)","l(x, 1):tmm2"]+2*vcovHC["l(x, 2)","l(x, 1):tmm2"]+2*vcovHC["l(x, 3)","l(x, 1):tmm2"]+2*vcovHC["l(x, 4)","l(x, 1):tmm2"]+2*vcovHC["l(x, 5)","l(x, 1):tmm2"]+2*vcovHC["x:tmm2","l(x, 1):tmm2"]+
    2*vcovHC["x","l(x, 2):tmm2"]+2*vcovHC["l(x, 1)","l(x, 2):tmm2"]+2*vcovHC["l(x, 2)","l(x, 2):tmm2"]+2*vcovHC["l(x, 3)","l(x, 2):tmm2"]+2*vcovHC["l(x, 4)","l(x, 2):tmm2"]+2*vcovHC["l(x, 5)","l(x, 2):tmm2"]+2*vcovHC["x:tmm2","l(x, 2):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 2):tmm2"]+
    2*vcovHC["x","l(x, 3):tmm2"]+2*vcovHC["l(x, 1)","l(x, 3):tmm2"]+2*vcovHC["l(x, 2)","l(x, 3):tmm2"]+2*vcovHC["l(x, 3)","l(x, 3):tmm2"]+2*vcovHC["l(x, 4)","l(x, 3):tmm2"]+2*vcovHC["l(x, 5)","l(x, 3):tmm2"]+2*vcovHC["x:tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 3):tmm2"]+
    2*vcovHC["x","l(x, 4):tmm2"]+2*vcovHC["l(x, 1)","l(x, 4):tmm2"]+2*vcovHC["l(x, 2)","l(x, 4):tmm2"]+2*vcovHC["l(x, 3)","l(x, 4):tmm2"]+2*vcovHC["l(x, 4)","l(x, 4):tmm2"]+2*vcovHC["l(x, 5)","l(x, 4):tmm2"]+2*vcovHC["x:tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 4):tmm2"]+
    2*vcovHC["x","l(x, 5):tmm2"]+2*vcovHC["l(x, 1)","l(x, 5):tmm2"]+2*vcovHC["l(x, 2)","l(x, 5):tmm2"]+2*vcovHC["l(x, 3)","l(x, 5):tmm2"]+2*vcovHC["l(x, 4)","l(x, 5):tmm2"]+2*vcovHC["l(x, 5)","l(x, 5):tmm2"]+2*vcovHC["x:tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 5):tmm2"]
  var71<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["x:tmm2","x:tmm2"]+vcovHC["l(x, 1):tmm2","l(x, 1):tmm2"]+vcovHC["l(x, 2):tmm2","l(x, 2):tmm2"]+vcovHC["l(x, 3):tmm2","l(x, 3):tmm2"]+vcovHC["l(x, 4):tmm2","l(x, 4):tmm2"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):tmm2","l(x, 5):tmm2"]+vcovHC["l(x, 6):tmm2","l(x, 6):tmm2"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","x:tmm2"]+2*vcovHC["l(x, 1)","x:tmm2"]+2*vcovHC["l(x, 2)","x:tmm2"]+2*vcovHC["l(x, 3)","x:tmm2"]+2*vcovHC["l(x, 4)","x:tmm2"]+2*vcovHC["l(x, 5)","x:tmm2"]+2*vcovHC["l(x, 6)","x:tmm2"]+
    2*vcovHC["x","l(x, 1):tmm2"]+2*vcovHC["l(x, 1)","l(x, 1):tmm2"]+2*vcovHC["l(x, 2)","l(x, 1):tmm2"]+2*vcovHC["l(x, 3)","l(x, 1):tmm2"]+2*vcovHC["l(x, 4)","l(x, 1):tmm2"]+2*vcovHC["l(x, 5)","l(x, 1):tmm2"]+2*vcovHC["l(x, 6)","l(x, 1):tmm2"]+2*vcovHC["x:tmm2","l(x, 1):tmm2"]+
    2*vcovHC["x","l(x, 2):tmm2"]+2*vcovHC["l(x, 1)","l(x, 2):tmm2"]+2*vcovHC["l(x, 2)","l(x, 2):tmm2"]+2*vcovHC["l(x, 3)","l(x, 2):tmm2"]+2*vcovHC["l(x, 4)","l(x, 2):tmm2"]+2*vcovHC["l(x, 5)","l(x, 2):tmm2"]+2*vcovHC["l(x, 6)","l(x, 2):tmm2"]+2*vcovHC["x:tmm2","l(x, 2):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 2):tmm2"]+
    2*vcovHC["x","l(x, 3):tmm2"]+2*vcovHC["l(x, 1)","l(x, 3):tmm2"]+2*vcovHC["l(x, 2)","l(x, 3):tmm2"]+2*vcovHC["l(x, 3)","l(x, 3):tmm2"]+2*vcovHC["l(x, 4)","l(x, 3):tmm2"]+2*vcovHC["l(x, 5)","l(x, 3):tmm2"]+2*vcovHC["l(x, 6)","l(x, 3):tmm2"]+2*vcovHC["x:tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 3):tmm2"]+
    2*vcovHC["x","l(x, 4):tmm2"]+2*vcovHC["l(x, 1)","l(x, 4):tmm2"]+2*vcovHC["l(x, 2)","l(x, 4):tmm2"]+2*vcovHC["l(x, 3)","l(x, 4):tmm2"]+2*vcovHC["l(x, 4)","l(x, 4):tmm2"]+2*vcovHC["l(x, 5)","l(x, 4):tmm2"]+2*vcovHC["l(x, 6)","l(x, 4):tmm2"]+2*vcovHC["x:tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 4):tmm2"]+
    2*vcovHC["x","l(x, 5):tmm2"]+2*vcovHC["l(x, 1)","l(x, 5):tmm2"]+2*vcovHC["l(x, 2)","l(x, 5):tmm2"]+2*vcovHC["l(x, 3)","l(x, 5):tmm2"]+2*vcovHC["l(x, 4)","l(x, 5):tmm2"]+2*vcovHC["l(x, 5)","l(x, 5):tmm2"]+2*vcovHC["l(x, 6)","l(x, 5):tmm2"]+2*vcovHC["x:tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 5):tmm2"]+
    2*vcovHC["x","l(x, 6):tmm2"]+2*vcovHC["l(x, 1)","l(x, 6):tmm2"]+2*vcovHC["l(x, 2)","l(x, 6):tmm2"]+2*vcovHC["l(x, 3)","l(x, 6):tmm2"]+2*vcovHC["l(x, 4)","l(x, 6):tmm2"]+2*vcovHC["l(x, 5)","l(x, 6):tmm2"]+2*vcovHC["l(x, 6)","l(x, 6):tmm2"]+2*vcovHC["x:tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 6):tmm2"]
  var81<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["x:tmm2","x:tmm2"]+vcovHC["l(x, 1):tmm2","l(x, 1):tmm2"]+vcovHC["l(x, 2):tmm2","l(x, 2):tmm2"]+vcovHC["l(x, 3):tmm2","l(x, 3):tmm2"]+vcovHC["l(x, 4):tmm2","l(x, 4):tmm2"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):tmm2","l(x, 5):tmm2"]+vcovHC["l(x, 6):tmm2","l(x, 6):tmm2"]+vcovHC["l(x, 7):tmm2","l(x, 7):tmm2"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","x:tmm2"]+2*vcovHC["l(x, 1)","x:tmm2"]+2*vcovHC["l(x, 2)","x:tmm2"]+2*vcovHC["l(x, 3)","x:tmm2"]+2*vcovHC["l(x, 4)","x:tmm2"]+2*vcovHC["l(x, 5)","x:tmm2"]+2*vcovHC["l(x, 6)","x:tmm2"]+2*vcovHC["l(x, 7)","x:tmm2"]+
    2*vcovHC["x","l(x, 1):tmm2"]+2*vcovHC["l(x, 1)","l(x, 1):tmm2"]+2*vcovHC["l(x, 2)","l(x, 1):tmm2"]+2*vcovHC["l(x, 3)","l(x, 1):tmm2"]+2*vcovHC["l(x, 4)","l(x, 1):tmm2"]+2*vcovHC["l(x, 5)","l(x, 1):tmm2"]+2*vcovHC["l(x, 6)","l(x, 1):tmm2"]+2*vcovHC["l(x, 7)","l(x, 1):tmm2"]+2*vcovHC["x:tmm2","l(x, 1):tmm2"]+
    2*vcovHC["x","l(x, 2):tmm2"]+2*vcovHC["l(x, 1)","l(x, 2):tmm2"]+2*vcovHC["l(x, 2)","l(x, 2):tmm2"]+2*vcovHC["l(x, 3)","l(x, 2):tmm2"]+2*vcovHC["l(x, 4)","l(x, 2):tmm2"]+2*vcovHC["l(x, 5)","l(x, 2):tmm2"]+2*vcovHC["l(x, 6)","l(x, 2):tmm2"]+2*vcovHC["l(x, 7)","l(x, 2):tmm2"]+2*vcovHC["x:tmm2","l(x, 2):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 2):tmm2"]+
    2*vcovHC["x","l(x, 3):tmm2"]+2*vcovHC["l(x, 1)","l(x, 3):tmm2"]+2*vcovHC["l(x, 2)","l(x, 3):tmm2"]+2*vcovHC["l(x, 3)","l(x, 3):tmm2"]+2*vcovHC["l(x, 4)","l(x, 3):tmm2"]+2*vcovHC["l(x, 5)","l(x, 3):tmm2"]+2*vcovHC["l(x, 6)","l(x, 3):tmm2"]+2*vcovHC["l(x, 7)","l(x, 3):tmm2"]+2*vcovHC["x:tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 3):tmm2"]+
    2*vcovHC["x","l(x, 4):tmm2"]+2*vcovHC["l(x, 1)","l(x, 4):tmm2"]+2*vcovHC["l(x, 2)","l(x, 4):tmm2"]+2*vcovHC["l(x, 3)","l(x, 4):tmm2"]+2*vcovHC["l(x, 4)","l(x, 4):tmm2"]+2*vcovHC["l(x, 5)","l(x, 4):tmm2"]+2*vcovHC["l(x, 6)","l(x, 4):tmm2"]+2*vcovHC["l(x, 7)","l(x, 4):tmm2"]+2*vcovHC["x:tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 4):tmm2"]+
    2*vcovHC["x","l(x, 5):tmm2"]+2*vcovHC["l(x, 1)","l(x, 5):tmm2"]+2*vcovHC["l(x, 2)","l(x, 5):tmm2"]+2*vcovHC["l(x, 3)","l(x, 5):tmm2"]+2*vcovHC["l(x, 4)","l(x, 5):tmm2"]+2*vcovHC["l(x, 5)","l(x, 5):tmm2"]+2*vcovHC["l(x, 6)","l(x, 5):tmm2"]+2*vcovHC["l(x, 7)","l(x, 5):tmm2"]+2*vcovHC["x:tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 5):tmm2"]+
    2*vcovHC["x","l(x, 6):tmm2"]+2*vcovHC["l(x, 1)","l(x, 6):tmm2"]+2*vcovHC["l(x, 2)","l(x, 6):tmm2"]+2*vcovHC["l(x, 3)","l(x, 6):tmm2"]+2*vcovHC["l(x, 4)","l(x, 6):tmm2"]+2*vcovHC["l(x, 5)","l(x, 6):tmm2"]+2*vcovHC["l(x, 6)","l(x, 6):tmm2"]+2*vcovHC["l(x, 7)","l(x, 6):tmm2"]+2*vcovHC["x:tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 6):tmm2"]+
    2*vcovHC["x","l(x, 7):tmm2"]+2*vcovHC["l(x, 1)","l(x, 7):tmm2"]+2*vcovHC["l(x, 2)","l(x, 7):tmm2"]+2*vcovHC["l(x, 3)","l(x, 7):tmm2"]+2*vcovHC["l(x, 4)","l(x, 7):tmm2"]+2*vcovHC["l(x, 5)","l(x, 7):tmm2"]+2*vcovHC["l(x, 6)","l(x, 7):tmm2"]+2*vcovHC["l(x, 7)","l(x, 7):tmm2"]+2*vcovHC["x:tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 6):tmm2","l(x, 7):tmm2"]
  var91<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["x:tmm2","x:tmm2"]+vcovHC["l(x, 1):tmm2","l(x, 1):tmm2"]+vcovHC["l(x, 2):tmm2","l(x, 2):tmm2"]+vcovHC["l(x, 3):tmm2","l(x, 3):tmm2"]+vcovHC["l(x, 4):tmm2","l(x, 4):tmm2"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):tmm2","l(x, 5):tmm2"]+vcovHC["l(x, 6):tmm2","l(x, 6):tmm2"]+vcovHC["l(x, 7):tmm2","l(x, 7):tmm2"]+vcovHC["l(x, 8):tmm2","l(x, 8):tmm2"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","x:tmm2"]+2*vcovHC["l(x, 1)","x:tmm2"]+2*vcovHC["l(x, 2)","x:tmm2"]+2*vcovHC["l(x, 3)","x:tmm2"]+2*vcovHC["l(x, 4)","x:tmm2"]+2*vcovHC["l(x, 5)","x:tmm2"]+2*vcovHC["l(x, 6)","x:tmm2"]+2*vcovHC["l(x, 7)","x:tmm2"]+2*vcovHC["l(x, 8)","x:tmm2"]+
    2*vcovHC["x","l(x, 1):tmm2"]+2*vcovHC["l(x, 1)","l(x, 1):tmm2"]+2*vcovHC["l(x, 2)","l(x, 1):tmm2"]+2*vcovHC["l(x, 3)","l(x, 1):tmm2"]+2*vcovHC["l(x, 4)","l(x, 1):tmm2"]+2*vcovHC["l(x, 5)","l(x, 1):tmm2"]+2*vcovHC["l(x, 6)","l(x, 1):tmm2"]+2*vcovHC["l(x, 7)","l(x, 1):tmm2"]+2*vcovHC["l(x, 8)","l(x, 1):tmm2"]+2*vcovHC["x:tmm2","l(x, 1):tmm2"]+
    2*vcovHC["x","l(x, 2):tmm2"]+2*vcovHC["l(x, 1)","l(x, 2):tmm2"]+2*vcovHC["l(x, 2)","l(x, 2):tmm2"]+2*vcovHC["l(x, 3)","l(x, 2):tmm2"]+2*vcovHC["l(x, 4)","l(x, 2):tmm2"]+2*vcovHC["l(x, 5)","l(x, 2):tmm2"]+2*vcovHC["l(x, 6)","l(x, 2):tmm2"]+2*vcovHC["l(x, 7)","l(x, 2):tmm2"]+2*vcovHC["l(x, 8)","l(x, 2):tmm2"]+2*vcovHC["x:tmm2","l(x, 2):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 2):tmm2"]+
    2*vcovHC["x","l(x, 3):tmm2"]+2*vcovHC["l(x, 1)","l(x, 3):tmm2"]+2*vcovHC["l(x, 2)","l(x, 3):tmm2"]+2*vcovHC["l(x, 3)","l(x, 3):tmm2"]+2*vcovHC["l(x, 4)","l(x, 3):tmm2"]+2*vcovHC["l(x, 5)","l(x, 3):tmm2"]+2*vcovHC["l(x, 6)","l(x, 3):tmm2"]+2*vcovHC["l(x, 7)","l(x, 3):tmm2"]+2*vcovHC["l(x, 8)","l(x, 3):tmm2"]+2*vcovHC["x:tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 3):tmm2"]+
    2*vcovHC["x","l(x, 4):tmm2"]+2*vcovHC["l(x, 1)","l(x, 4):tmm2"]+2*vcovHC["l(x, 2)","l(x, 4):tmm2"]+2*vcovHC["l(x, 3)","l(x, 4):tmm2"]+2*vcovHC["l(x, 4)","l(x, 4):tmm2"]+2*vcovHC["l(x, 5)","l(x, 4):tmm2"]+2*vcovHC["l(x, 6)","l(x, 4):tmm2"]+2*vcovHC["l(x, 7)","l(x, 4):tmm2"]+2*vcovHC["l(x, 8)","l(x, 4):tmm2"]+2*vcovHC["x:tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 4):tmm2"]+
    2*vcovHC["x","l(x, 5):tmm2"]+2*vcovHC["l(x, 1)","l(x, 5):tmm2"]+2*vcovHC["l(x, 2)","l(x, 5):tmm2"]+2*vcovHC["l(x, 3)","l(x, 5):tmm2"]+2*vcovHC["l(x, 4)","l(x, 5):tmm2"]+2*vcovHC["l(x, 5)","l(x, 5):tmm2"]+2*vcovHC["l(x, 6)","l(x, 5):tmm2"]+2*vcovHC["l(x, 7)","l(x, 5):tmm2"]+2*vcovHC["l(x, 8)","l(x, 5):tmm2"]+2*vcovHC["x:tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 5):tmm2"]+
    2*vcovHC["x","l(x, 6):tmm2"]+2*vcovHC["l(x, 1)","l(x, 6):tmm2"]+2*vcovHC["l(x, 2)","l(x, 6):tmm2"]+2*vcovHC["l(x, 3)","l(x, 6):tmm2"]+2*vcovHC["l(x, 4)","l(x, 6):tmm2"]+2*vcovHC["l(x, 5)","l(x, 6):tmm2"]+2*vcovHC["l(x, 6)","l(x, 6):tmm2"]+2*vcovHC["l(x, 7)","l(x, 6):tmm2"]+2*vcovHC["l(x, 8)","l(x, 6):tmm2"]+2*vcovHC["x:tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 6):tmm2"]+
    2*vcovHC["x","l(x, 7):tmm2"]+2*vcovHC["l(x, 1)","l(x, 7):tmm2"]+2*vcovHC["l(x, 2)","l(x, 7):tmm2"]+2*vcovHC["l(x, 3)","l(x, 7):tmm2"]+2*vcovHC["l(x, 4)","l(x, 7):tmm2"]+2*vcovHC["l(x, 5)","l(x, 7):tmm2"]+2*vcovHC["l(x, 6)","l(x, 7):tmm2"]+2*vcovHC["l(x, 7)","l(x, 7):tmm2"]+2*vcovHC["l(x, 8)","l(x, 7):tmm2"]+2*vcovHC["x:tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 6):tmm2","l(x, 7):tmm2"]+
    2*vcovHC["x","l(x, 8):tmm2"]+2*vcovHC["l(x, 1)","l(x, 8):tmm2"]+2*vcovHC["l(x, 2)","l(x, 8):tmm2"]+2*vcovHC["l(x, 3)","l(x, 8):tmm2"]+2*vcovHC["l(x, 4)","l(x, 8):tmm2"]+2*vcovHC["l(x, 5)","l(x, 8):tmm2"]+2*vcovHC["l(x, 6)","l(x, 8):tmm2"]+2*vcovHC["l(x, 7)","l(x, 8):tmm2"]+2*vcovHC["l(x, 8)","l(x, 8):tmm2"]+2*vcovHC["x:tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 6):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 7):tmm2","l(x, 8):tmm2"]
  var101<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+vcovHC["x:tmm2","x:tmm2"]+vcovHC["l(x, 1):tmm2","l(x, 1):tmm2"]+vcovHC["l(x, 2):tmm2","l(x, 2):tmm2"]+vcovHC["l(x, 3):tmm2","l(x, 3):tmm2"]+vcovHC["l(x, 4):tmm2","l(x, 4):tmm2"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):tmm2","l(x, 5):tmm2"]+vcovHC["l(x, 6):tmm2","l(x, 6):tmm2"]+vcovHC["l(x, 7):tmm2","l(x, 7):tmm2"]+vcovHC["l(x, 8):tmm2","l(x, 8):tmm2"]+vcovHC["l(x, 9):tmm2","l(x, 9):tmm2"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]+
    2*vcovHC["x","x:tmm2"]+2*vcovHC["l(x, 1)","x:tmm2"]+2*vcovHC["l(x, 2)","x:tmm2"]+2*vcovHC["l(x, 3)","x:tmm2"]+2*vcovHC["l(x, 4)","x:tmm2"]+2*vcovHC["l(x, 5)","x:tmm2"]+2*vcovHC["l(x, 6)","x:tmm2"]+2*vcovHC["l(x, 7)","x:tmm2"]+2*vcovHC["l(x, 8)","x:tmm2"]+2*vcovHC["l(x, 9)","x:tmm2"]+
    2*vcovHC["x","l(x, 1):tmm2"]+2*vcovHC["l(x, 1)","l(x, 1):tmm2"]+2*vcovHC["l(x, 2)","l(x, 1):tmm2"]+2*vcovHC["l(x, 3)","l(x, 1):tmm2"]+2*vcovHC["l(x, 4)","l(x, 1):tmm2"]+2*vcovHC["l(x, 5)","l(x, 1):tmm2"]+2*vcovHC["l(x, 6)","l(x, 1):tmm2"]+2*vcovHC["l(x, 7)","l(x, 1):tmm2"]+2*vcovHC["l(x, 8)","l(x, 1):tmm2"]+2*vcovHC["l(x, 9)","l(x, 1):tmm2"]+2*vcovHC["x:tmm2","l(x, 1):tmm2"]+
    2*vcovHC["x","l(x, 2):tmm2"]+2*vcovHC["l(x, 1)","l(x, 2):tmm2"]+2*vcovHC["l(x, 2)","l(x, 2):tmm2"]+2*vcovHC["l(x, 3)","l(x, 2):tmm2"]+2*vcovHC["l(x, 4)","l(x, 2):tmm2"]+2*vcovHC["l(x, 5)","l(x, 2):tmm2"]+2*vcovHC["l(x, 6)","l(x, 2):tmm2"]+2*vcovHC["l(x, 7)","l(x, 2):tmm2"]+2*vcovHC["l(x, 8)","l(x, 2):tmm2"]+2*vcovHC["l(x, 9)","l(x, 2):tmm2"]+2*vcovHC["x:tmm2","l(x, 2):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 2):tmm2"]+
    2*vcovHC["x","l(x, 3):tmm2"]+2*vcovHC["l(x, 1)","l(x, 3):tmm2"]+2*vcovHC["l(x, 2)","l(x, 3):tmm2"]+2*vcovHC["l(x, 3)","l(x, 3):tmm2"]+2*vcovHC["l(x, 4)","l(x, 3):tmm2"]+2*vcovHC["l(x, 5)","l(x, 3):tmm2"]+2*vcovHC["l(x, 6)","l(x, 3):tmm2"]+2*vcovHC["l(x, 7)","l(x, 3):tmm2"]+2*vcovHC["l(x, 8)","l(x, 3):tmm2"]+2*vcovHC["l(x, 9)","l(x, 3):tmm2"]+2*vcovHC["x:tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 3):tmm2"]+
    2*vcovHC["x","l(x, 4):tmm2"]+2*vcovHC["l(x, 1)","l(x, 4):tmm2"]+2*vcovHC["l(x, 2)","l(x, 4):tmm2"]+2*vcovHC["l(x, 3)","l(x, 4):tmm2"]+2*vcovHC["l(x, 4)","l(x, 4):tmm2"]+2*vcovHC["l(x, 5)","l(x, 4):tmm2"]+2*vcovHC["l(x, 6)","l(x, 4):tmm2"]+2*vcovHC["l(x, 7)","l(x, 4):tmm2"]+2*vcovHC["l(x, 8)","l(x, 4):tmm2"]+2*vcovHC["l(x, 9)","l(x, 4):tmm2"]+2*vcovHC["x:tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 4):tmm2"]+
    2*vcovHC["x","l(x, 5):tmm2"]+2*vcovHC["l(x, 1)","l(x, 5):tmm2"]+2*vcovHC["l(x, 2)","l(x, 5):tmm2"]+2*vcovHC["l(x, 3)","l(x, 5):tmm2"]+2*vcovHC["l(x, 4)","l(x, 5):tmm2"]+2*vcovHC["l(x, 5)","l(x, 5):tmm2"]+2*vcovHC["l(x, 6)","l(x, 5):tmm2"]+2*vcovHC["l(x, 7)","l(x, 5):tmm2"]+2*vcovHC["l(x, 8)","l(x, 5):tmm2"]+2*vcovHC["l(x, 9)","l(x, 5):tmm2"]+2*vcovHC["x:tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 5):tmm2"]+
    2*vcovHC["x","l(x, 6):tmm2"]+2*vcovHC["l(x, 1)","l(x, 6):tmm2"]+2*vcovHC["l(x, 2)","l(x, 6):tmm2"]+2*vcovHC["l(x, 3)","l(x, 6):tmm2"]+2*vcovHC["l(x, 4)","l(x, 6):tmm2"]+2*vcovHC["l(x, 5)","l(x, 6):tmm2"]+2*vcovHC["l(x, 6)","l(x, 6):tmm2"]+2*vcovHC["l(x, 7)","l(x, 6):tmm2"]+2*vcovHC["l(x, 8)","l(x, 6):tmm2"]+2*vcovHC["l(x, 9)","l(x, 6):tmm2"]+2*vcovHC["x:tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 6):tmm2"]+
    2*vcovHC["x","l(x, 7):tmm2"]+2*vcovHC["l(x, 1)","l(x, 7):tmm2"]+2*vcovHC["l(x, 2)","l(x, 7):tmm2"]+2*vcovHC["l(x, 3)","l(x, 7):tmm2"]+2*vcovHC["l(x, 4)","l(x, 7):tmm2"]+2*vcovHC["l(x, 5)","l(x, 7):tmm2"]+2*vcovHC["l(x, 6)","l(x, 7):tmm2"]+2*vcovHC["l(x, 7)","l(x, 7):tmm2"]+2*vcovHC["l(x, 8)","l(x, 7):tmm2"]+2*vcovHC["l(x, 9)","l(x, 7):tmm2"]+2*vcovHC["x:tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 6):tmm2","l(x, 7):tmm2"]+
    2*vcovHC["x","l(x, 8):tmm2"]+2*vcovHC["l(x, 1)","l(x, 8):tmm2"]+2*vcovHC["l(x, 2)","l(x, 8):tmm2"]+2*vcovHC["l(x, 3)","l(x, 8):tmm2"]+2*vcovHC["l(x, 4)","l(x, 8):tmm2"]+2*vcovHC["l(x, 5)","l(x, 8):tmm2"]+2*vcovHC["l(x, 6)","l(x, 8):tmm2"]+2*vcovHC["l(x, 7)","l(x, 8):tmm2"]+2*vcovHC["l(x, 8)","l(x, 8):tmm2"]+2*vcovHC["l(x, 9)","l(x, 8):tmm2"]+2*vcovHC["x:tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 6):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 7):tmm2","l(x, 8):tmm2"]+
    2*vcovHC["x","l(x, 9):tmm2"]+2*vcovHC["l(x, 1)","l(x, 9):tmm2"]+2*vcovHC["l(x, 2)","l(x, 9):tmm2"]+2*vcovHC["l(x, 3)","l(x, 9):tmm2"]+2*vcovHC["l(x, 4)","l(x, 9):tmm2"]+2*vcovHC["l(x, 5)","l(x, 9):tmm2"]+2*vcovHC["l(x, 6)","l(x, 9):tmm2"]+2*vcovHC["l(x, 7)","l(x, 9):tmm2"]+2*vcovHC["l(x, 8)","l(x, 9):tmm2"]+2*vcovHC["l(x, 9)","l(x, 9):tmm2"]+2*vcovHC["x:tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 6):tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 7):tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 8):tmm2","l(x, 9):tmm2"]
  var111<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+vcovHC["l(x, 10)","l(x, 10)"]+vcovHC["x:tmm2","x:tmm2"]+vcovHC["l(x, 1):tmm2","l(x, 1):tmm2"]+vcovHC["l(x, 2):tmm2","l(x, 2):tmm2"]+vcovHC["l(x, 3):tmm2","l(x, 3):tmm2"]+vcovHC["l(x, 4):tmm2","l(x, 4):tmm2"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):tmm2","l(x, 5):tmm2"]+vcovHC["l(x, 6):tmm2","l(x, 6):tmm2"]+vcovHC["l(x, 7):tmm2","l(x, 7):tmm2"]+vcovHC["l(x, 8):tmm2","l(x, 8):tmm2"]+vcovHC["l(x, 9):tmm2","l(x, 9):tmm2"]+vcovHC["l(x, 10):tmm2","l(x, 10):tmm2"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]+
    2*vcovHC["x","l(x, 10)"]+2*vcovHC["l(x, 1)","l(x, 10)"]+2*vcovHC["l(x, 2)","l(x, 10)"]+2*vcovHC["l(x, 3)","l(x, 10)"]+2*vcovHC["l(x, 4)","l(x, 10)"]+2*vcovHC["l(x, 5)","l(x, 10)"]+2*vcovHC["l(x, 6)","l(x, 10)"]+2*vcovHC["l(x, 7)","l(x, 10)"]+2*vcovHC["l(x, 8)","l(x, 10)"]+2*vcovHC["l(x, 9)","l(x, 10)"]+
    2*vcovHC["x","x:tmm2"]+2*vcovHC["l(x, 1)","x:tmm2"]+2*vcovHC["l(x, 2)","x:tmm2"]+2*vcovHC["l(x, 3)","x:tmm2"]+2*vcovHC["l(x, 4)","x:tmm2"]+2*vcovHC["l(x, 5)","x:tmm2"]+2*vcovHC["l(x, 6)","x:tmm2"]+2*vcovHC["l(x, 7)","x:tmm2"]+2*vcovHC["l(x, 8)","x:tmm2"]+2*vcovHC["l(x, 9)","x:tmm2"]+2*vcovHC["l(x, 10)","x:tmm2"]+
    2*vcovHC["x","l(x, 1):tmm2"]+2*vcovHC["l(x, 1)","l(x, 1):tmm2"]+2*vcovHC["l(x, 2)","l(x, 1):tmm2"]+2*vcovHC["l(x, 3)","l(x, 1):tmm2"]+2*vcovHC["l(x, 4)","l(x, 1):tmm2"]+2*vcovHC["l(x, 5)","l(x, 1):tmm2"]+2*vcovHC["l(x, 6)","l(x, 1):tmm2"]+2*vcovHC["l(x, 7)","l(x, 1):tmm2"]+2*vcovHC["l(x, 8)","l(x, 1):tmm2"]+2*vcovHC["l(x, 9)","l(x, 1):tmm2"]+2*vcovHC["l(x, 10)","l(x, 1):tmm2"]+2*vcovHC["x:tmm2","l(x, 1):tmm2"]+
    2*vcovHC["x","l(x, 2):tmm2"]+2*vcovHC["l(x, 1)","l(x, 2):tmm2"]+2*vcovHC["l(x, 2)","l(x, 2):tmm2"]+2*vcovHC["l(x, 3)","l(x, 2):tmm2"]+2*vcovHC["l(x, 4)","l(x, 2):tmm2"]+2*vcovHC["l(x, 5)","l(x, 2):tmm2"]+2*vcovHC["l(x, 6)","l(x, 2):tmm2"]+2*vcovHC["l(x, 7)","l(x, 2):tmm2"]+2*vcovHC["l(x, 8)","l(x, 2):tmm2"]+2*vcovHC["l(x, 9)","l(x, 2):tmm2"]+2*vcovHC["l(x, 10)","l(x, 2):tmm2"]+2*vcovHC["x:tmm2","l(x, 2):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 2):tmm2"]+
    2*vcovHC["x","l(x, 3):tmm2"]+2*vcovHC["l(x, 1)","l(x, 3):tmm2"]+2*vcovHC["l(x, 2)","l(x, 3):tmm2"]+2*vcovHC["l(x, 3)","l(x, 3):tmm2"]+2*vcovHC["l(x, 4)","l(x, 3):tmm2"]+2*vcovHC["l(x, 5)","l(x, 3):tmm2"]+2*vcovHC["l(x, 6)","l(x, 3):tmm2"]+2*vcovHC["l(x, 7)","l(x, 3):tmm2"]+2*vcovHC["l(x, 8)","l(x, 3):tmm2"]+2*vcovHC["l(x, 9)","l(x, 3):tmm2"]+2*vcovHC["l(x, 10)","l(x, 3):tmm2"]+2*vcovHC["x:tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 3):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 3):tmm2"]+
    2*vcovHC["x","l(x, 4):tmm2"]+2*vcovHC["l(x, 1)","l(x, 4):tmm2"]+2*vcovHC["l(x, 2)","l(x, 4):tmm2"]+2*vcovHC["l(x, 3)","l(x, 4):tmm2"]+2*vcovHC["l(x, 4)","l(x, 4):tmm2"]+2*vcovHC["l(x, 5)","l(x, 4):tmm2"]+2*vcovHC["l(x, 6)","l(x, 4):tmm2"]+2*vcovHC["l(x, 7)","l(x, 4):tmm2"]+2*vcovHC["l(x, 8)","l(x, 4):tmm2"]+2*vcovHC["l(x, 9)","l(x, 4):tmm2"]+2*vcovHC["l(x, 10)","l(x, 4):tmm2"]+2*vcovHC["x:tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 4):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 4):tmm2"]+
    2*vcovHC["x","l(x, 5):tmm2"]+2*vcovHC["l(x, 1)","l(x, 5):tmm2"]+2*vcovHC["l(x, 2)","l(x, 5):tmm2"]+2*vcovHC["l(x, 3)","l(x, 5):tmm2"]+2*vcovHC["l(x, 4)","l(x, 5):tmm2"]+2*vcovHC["l(x, 5)","l(x, 5):tmm2"]+2*vcovHC["l(x, 6)","l(x, 5):tmm2"]+2*vcovHC["l(x, 7)","l(x, 5):tmm2"]+2*vcovHC["l(x, 8)","l(x, 5):tmm2"]+2*vcovHC["l(x, 9)","l(x, 5):tmm2"]+2*vcovHC["l(x, 10)","l(x, 5):tmm2"]+2*vcovHC["x:tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 5):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 5):tmm2"]+
    2*vcovHC["x","l(x, 6):tmm2"]+2*vcovHC["l(x, 1)","l(x, 6):tmm2"]+2*vcovHC["l(x, 2)","l(x, 6):tmm2"]+2*vcovHC["l(x, 3)","l(x, 6):tmm2"]+2*vcovHC["l(x, 4)","l(x, 6):tmm2"]+2*vcovHC["l(x, 5)","l(x, 6):tmm2"]+2*vcovHC["l(x, 6)","l(x, 6):tmm2"]+2*vcovHC["l(x, 7)","l(x, 6):tmm2"]+2*vcovHC["l(x, 8)","l(x, 6):tmm2"]+2*vcovHC["l(x, 9)","l(x, 6):tmm2"]+2*vcovHC["l(x, 10)","l(x, 6):tmm2"]+2*vcovHC["x:tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 6):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 6):tmm2"]+
    2*vcovHC["x","l(x, 7):tmm2"]+2*vcovHC["l(x, 1)","l(x, 7):tmm2"]+2*vcovHC["l(x, 2)","l(x, 7):tmm2"]+2*vcovHC["l(x, 3)","l(x, 7):tmm2"]+2*vcovHC["l(x, 4)","l(x, 7):tmm2"]+2*vcovHC["l(x, 5)","l(x, 7):tmm2"]+2*vcovHC["l(x, 6)","l(x, 7):tmm2"]+2*vcovHC["l(x, 7)","l(x, 7):tmm2"]+2*vcovHC["l(x, 8)","l(x, 7):tmm2"]+2*vcovHC["l(x, 9)","l(x, 7):tmm2"]+2*vcovHC["l(x, 10)","l(x, 7):tmm2"]+2*vcovHC["x:tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 7):tmm2"]+2*vcovHC["l(x, 6):tmm2","l(x, 7):tmm2"]+
    2*vcovHC["x","l(x, 8):tmm2"]+2*vcovHC["l(x, 1)","l(x, 8):tmm2"]+2*vcovHC["l(x, 2)","l(x, 8):tmm2"]+2*vcovHC["l(x, 3)","l(x, 8):tmm2"]+2*vcovHC["l(x, 4)","l(x, 8):tmm2"]+2*vcovHC["l(x, 5)","l(x, 8):tmm2"]+2*vcovHC["l(x, 6)","l(x, 8):tmm2"]+2*vcovHC["l(x, 7)","l(x, 8):tmm2"]+2*vcovHC["l(x, 8)","l(x, 8):tmm2"]+2*vcovHC["l(x, 9)","l(x, 8):tmm2"]+2*vcovHC["l(x, 10)","l(x, 8):tmm2"]+2*vcovHC["x:tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 6):tmm2","l(x, 8):tmm2"]+2*vcovHC["l(x, 7):tmm2","l(x, 8):tmm2"]+
    2*vcovHC["x","l(x, 9):tmm2"]+2*vcovHC["l(x, 1)","l(x, 9):tmm2"]+2*vcovHC["l(x, 2)","l(x, 9):tmm2"]+2*vcovHC["l(x, 3)","l(x, 9):tmm2"]+2*vcovHC["l(x, 4)","l(x, 9):tmm2"]+2*vcovHC["l(x, 5)","l(x, 9):tmm2"]+2*vcovHC["l(x, 6)","l(x, 9):tmm2"]+2*vcovHC["l(x, 7)","l(x, 9):tmm2"]+2*vcovHC["l(x, 8)","l(x, 9):tmm2"]+2*vcovHC["l(x, 9)","l(x, 9):tmm2"]+2*vcovHC["l(x, 10)","l(x, 9):tmm2"]+2*vcovHC["x:tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 6):tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 7):tmm2","l(x, 9):tmm2"]+2*vcovHC["l(x, 8):tmm2","l(x, 9):tmm2"]+
    2*vcovHC["x","l(x, 10):tmm2"]+2*vcovHC["l(x, 1)","l(x, 10):tmm2"]+2*vcovHC["l(x, 2)","l(x, 10):tmm2"]+2*vcovHC["l(x, 3)","l(x, 10):tmm2"]+2*vcovHC["l(x, 4)","l(x, 10):tmm2"]+2*vcovHC["l(x, 5)","l(x, 10):tmm2"]+2*vcovHC["l(x, 6)","l(x, 10):tmm2"]+2*vcovHC["l(x, 7)","l(x, 10):tmm2"]+2*vcovHC["l(x, 8)","l(x, 10):tmm2"]+2*vcovHC["l(x, 9)","l(x, 10):tmm2"]+2*vcovHC["l(x, 10)","l(x, 10):tmm2"]+2*vcovHC["x:tmm2","l(x, 10):tmm2"]+2*vcovHC["l(x, 1):tmm2","l(x, 10):tmm2"]+2*vcovHC["l(x, 2):tmm2","l(x, 10):tmm2"]+2*vcovHC["l(x, 3):tmm2","l(x, 10):tmm2"]+2*vcovHC["l(x, 4):tmm2","l(x, 10):tmm2"]+2*vcovHC["l(x, 5):tmm2","l(x, 10):tmm2"]+2*vcovHC["l(x, 6):tmm2","l(x, 10):tmm2"]+2*vcovHC["l(x, 7):tmm2","l(x, 10):tmm2"]+2*vcovHC["l(x, 8):tmm2","l(x, 10):tmm2"]+2*vcovHC["l(x, 9):tmm2","l(x, 10):tmm2"]
  
  varsums1<-c(var11,var21,var31,var41,var51,var61,var71,var81,var91,var101,var111)
  sdsums1<-varsums1^0.5
  
  singlecoeff11<-mod$coefficients[c("x:tmm2", "l(x, 1):tmm2", "l(x, 2):tmm2", "l(x, 3):tmm2", "l(x, 4):tmm2", "l(x, 5):tmm2", "l(x, 6):tmm2", "l(x, 7):tmm2", "l(x, 8):tmm2", "l(x, 9):tmm2", "l(x, 10):tmm2")]
  
  coeffsums11<-cumsum(singlecoeff11) + coeffsums10
  
  var12<-vcovHC["x","x"]+vcovHC["x:tmm3","x:tmm3"]+
    2*vcovHC["x","x:tmm3"]
  var22<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["x:tmm3","x:tmm3"]+vcovHC["l(x, 1):tmm3","l(x, 1):tmm3"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","x:tmm3"]+2*vcovHC["l(x, 1)","x:tmm3"]+
    2*vcovHC["x","l(x, 1):tmm3"]+2*vcovHC["l(x, 1)","l(x, 1):tmm3"]+2*vcovHC["x:tmm3","l(x, 1):tmm3"]
  var32<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["x:tmm3","x:tmm3"]+vcovHC["l(x, 1):tmm3","l(x, 1):tmm3"]+vcovHC["l(x, 2):tmm3","l(x, 2):tmm3"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","x:tmm3"]+2*vcovHC["l(x, 1)","x:tmm3"]+2*vcovHC["l(x, 2)","x:tmm3"]+
    2*vcovHC["x","l(x, 1):tmm3"]+2*vcovHC["l(x, 1)","l(x, 1):tmm3"]+2*vcovHC["l(x, 2)","l(x, 1):tmm3"]+2*vcovHC["x:tmm3","l(x, 1):tmm3"]+
    2*vcovHC["x","l(x, 2):tmm3"]+2*vcovHC["l(x, 1)","l(x, 2):tmm3"]+2*vcovHC["l(x, 2)","l(x, 2):tmm3"]+2*vcovHC["x:tmm3","l(x, 2):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 2):tmm3"]
  var42<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["x:tmm3","x:tmm3"]+vcovHC["l(x, 1):tmm3","l(x, 1):tmm3"]+vcovHC["l(x, 2):tmm3","l(x, 2):tmm3"]+vcovHC["l(x, 3):tmm3","l(x, 3):tmm3"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","x:tmm3"]+2*vcovHC["l(x, 1)","x:tmm3"]+2*vcovHC["l(x, 2)","x:tmm3"]+2*vcovHC["l(x, 3)","x:tmm3"]+
    2*vcovHC["x","l(x, 1):tmm3"]+2*vcovHC["l(x, 1)","l(x, 1):tmm3"]+2*vcovHC["l(x, 2)","l(x, 1):tmm3"]+2*vcovHC["l(x, 3)","l(x, 1):tmm3"]+2*vcovHC["x:tmm3","l(x, 1):tmm3"]+
    2*vcovHC["x","l(x, 2):tmm3"]+2*vcovHC["l(x, 1)","l(x, 2):tmm3"]+2*vcovHC["l(x, 2)","l(x, 2):tmm3"]+2*vcovHC["l(x, 3)","l(x, 2):tmm3"]+2*vcovHC["x:tmm3","l(x, 2):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 2):tmm3"]+
    2*vcovHC["x","l(x, 3):tmm3"]+2*vcovHC["l(x, 1)","l(x, 3):tmm3"]+2*vcovHC["l(x, 2)","l(x, 3):tmm3"]+2*vcovHC["l(x, 3)","l(x, 3):tmm3"]+2*vcovHC["x:tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 3):tmm3"]
  var52<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["x:tmm3","x:tmm3"]+vcovHC["l(x, 1):tmm3","l(x, 1):tmm3"]+vcovHC["l(x, 2):tmm3","l(x, 2):tmm3"]+vcovHC["l(x, 3):tmm3","l(x, 3):tmm3"]+vcovHC["l(x, 4):tmm3","l(x, 4):tmm3"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","x:tmm3"]+2*vcovHC["l(x, 1)","x:tmm3"]+2*vcovHC["l(x, 2)","x:tmm3"]+2*vcovHC["l(x, 3)","x:tmm3"]+2*vcovHC["l(x, 4)","x:tmm3"]+
    2*vcovHC["x","l(x, 1):tmm3"]+2*vcovHC["l(x, 1)","l(x, 1):tmm3"]+2*vcovHC["l(x, 2)","l(x, 1):tmm3"]+2*vcovHC["l(x, 3)","l(x, 1):tmm3"]+2*vcovHC["l(x, 4)","l(x, 1):tmm3"]+2*vcovHC["x:tmm3","l(x, 1):tmm3"]+
    2*vcovHC["x","l(x, 2):tmm3"]+2*vcovHC["l(x, 1)","l(x, 2):tmm3"]+2*vcovHC["l(x, 2)","l(x, 2):tmm3"]+2*vcovHC["l(x, 3)","l(x, 2):tmm3"]+2*vcovHC["l(x, 4)","l(x, 2):tmm3"]+2*vcovHC["x:tmm3","l(x, 2):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 2):tmm3"]+
    2*vcovHC["x","l(x, 3):tmm3"]+2*vcovHC["l(x, 1)","l(x, 3):tmm3"]+2*vcovHC["l(x, 2)","l(x, 3):tmm3"]+2*vcovHC["l(x, 3)","l(x, 3):tmm3"]+2*vcovHC["l(x, 4)","l(x, 3):tmm3"]+2*vcovHC["x:tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 3):tmm3"]+
    2*vcovHC["x","l(x, 4):tmm3"]+2*vcovHC["l(x, 1)","l(x, 4):tmm3"]+2*vcovHC["l(x, 2)","l(x, 4):tmm3"]+2*vcovHC["l(x, 3)","l(x, 4):tmm3"]+2*vcovHC["l(x, 4)","l(x, 4):tmm3"]+2*vcovHC["x:tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 4):tmm3"]
  
  var62<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["x:tmm3","x:tmm3"]+vcovHC["l(x, 1):tmm3","l(x, 1):tmm3"]+vcovHC["l(x, 2):tmm3","l(x, 2):tmm3"]+vcovHC["l(x, 3):tmm3","l(x, 3):tmm3"]+vcovHC["l(x, 4):tmm3","l(x, 4):tmm3"]+vcovHC["l(x, 5):tmm3","l(x, 5):tmm3"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","x:tmm3"]+2*vcovHC["l(x, 1)","x:tmm3"]+2*vcovHC["l(x, 2)","x:tmm3"]+2*vcovHC["l(x, 3)","x:tmm3"]+2*vcovHC["l(x, 4)","x:tmm3"]+2*vcovHC["l(x, 5)","x:tmm3"]+
    2*vcovHC["x","l(x, 1):tmm3"]+2*vcovHC["l(x, 1)","l(x, 1):tmm3"]+2*vcovHC["l(x, 2)","l(x, 1):tmm3"]+2*vcovHC["l(x, 3)","l(x, 1):tmm3"]+2*vcovHC["l(x, 4)","l(x, 1):tmm3"]+2*vcovHC["l(x, 5)","l(x, 1):tmm3"]+2*vcovHC["x:tmm3","l(x, 1):tmm3"]+
    2*vcovHC["x","l(x, 2):tmm3"]+2*vcovHC["l(x, 1)","l(x, 2):tmm3"]+2*vcovHC["l(x, 2)","l(x, 2):tmm3"]+2*vcovHC["l(x, 3)","l(x, 2):tmm3"]+2*vcovHC["l(x, 4)","l(x, 2):tmm3"]+2*vcovHC["l(x, 5)","l(x, 2):tmm3"]+2*vcovHC["x:tmm3","l(x, 2):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 2):tmm3"]+
    2*vcovHC["x","l(x, 3):tmm3"]+2*vcovHC["l(x, 1)","l(x, 3):tmm3"]+2*vcovHC["l(x, 2)","l(x, 3):tmm3"]+2*vcovHC["l(x, 3)","l(x, 3):tmm3"]+2*vcovHC["l(x, 4)","l(x, 3):tmm3"]+2*vcovHC["l(x, 5)","l(x, 3):tmm3"]+2*vcovHC["x:tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 3):tmm3"]+
    2*vcovHC["x","l(x, 4):tmm3"]+2*vcovHC["l(x, 1)","l(x, 4):tmm3"]+2*vcovHC["l(x, 2)","l(x, 4):tmm3"]+2*vcovHC["l(x, 3)","l(x, 4):tmm3"]+2*vcovHC["l(x, 4)","l(x, 4):tmm3"]+2*vcovHC["l(x, 5)","l(x, 4):tmm3"]+2*vcovHC["x:tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 4):tmm3"]+
    2*vcovHC["x","l(x, 5):tmm3"]+2*vcovHC["l(x, 1)","l(x, 5):tmm3"]+2*vcovHC["l(x, 2)","l(x, 5):tmm3"]+2*vcovHC["l(x, 3)","l(x, 5):tmm3"]+2*vcovHC["l(x, 4)","l(x, 5):tmm3"]+2*vcovHC["l(x, 5)","l(x, 5):tmm3"]+2*vcovHC["x:tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 5):tmm3"]
  var72<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["x:tmm3","x:tmm3"]+vcovHC["l(x, 1):tmm3","l(x, 1):tmm3"]+vcovHC["l(x, 2):tmm3","l(x, 2):tmm3"]+vcovHC["l(x, 3):tmm3","l(x, 3):tmm3"]+vcovHC["l(x, 4):tmm3","l(x, 4):tmm3"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):tmm3","l(x, 5):tmm3"]+vcovHC["l(x, 6):tmm3","l(x, 6):tmm3"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","x:tmm3"]+2*vcovHC["l(x, 1)","x:tmm3"]+2*vcovHC["l(x, 2)","x:tmm3"]+2*vcovHC["l(x, 3)","x:tmm3"]+2*vcovHC["l(x, 4)","x:tmm3"]+2*vcovHC["l(x, 5)","x:tmm3"]+2*vcovHC["l(x, 6)","x:tmm3"]+
    2*vcovHC["x","l(x, 1):tmm3"]+2*vcovHC["l(x, 1)","l(x, 1):tmm3"]+2*vcovHC["l(x, 2)","l(x, 1):tmm3"]+2*vcovHC["l(x, 3)","l(x, 1):tmm3"]+2*vcovHC["l(x, 4)","l(x, 1):tmm3"]+2*vcovHC["l(x, 5)","l(x, 1):tmm3"]+2*vcovHC["l(x, 6)","l(x, 1):tmm3"]+2*vcovHC["x:tmm3","l(x, 1):tmm3"]+
    2*vcovHC["x","l(x, 2):tmm3"]+2*vcovHC["l(x, 1)","l(x, 2):tmm3"]+2*vcovHC["l(x, 2)","l(x, 2):tmm3"]+2*vcovHC["l(x, 3)","l(x, 2):tmm3"]+2*vcovHC["l(x, 4)","l(x, 2):tmm3"]+2*vcovHC["l(x, 5)","l(x, 2):tmm3"]+2*vcovHC["l(x, 6)","l(x, 2):tmm3"]+2*vcovHC["x:tmm3","l(x, 2):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 2):tmm3"]+
    2*vcovHC["x","l(x, 3):tmm3"]+2*vcovHC["l(x, 1)","l(x, 3):tmm3"]+2*vcovHC["l(x, 2)","l(x, 3):tmm3"]+2*vcovHC["l(x, 3)","l(x, 3):tmm3"]+2*vcovHC["l(x, 4)","l(x, 3):tmm3"]+2*vcovHC["l(x, 5)","l(x, 3):tmm3"]+2*vcovHC["l(x, 6)","l(x, 3):tmm3"]+2*vcovHC["x:tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 3):tmm3"]+
    2*vcovHC["x","l(x, 4):tmm3"]+2*vcovHC["l(x, 1)","l(x, 4):tmm3"]+2*vcovHC["l(x, 2)","l(x, 4):tmm3"]+2*vcovHC["l(x, 3)","l(x, 4):tmm3"]+2*vcovHC["l(x, 4)","l(x, 4):tmm3"]+2*vcovHC["l(x, 5)","l(x, 4):tmm3"]+2*vcovHC["l(x, 6)","l(x, 4):tmm3"]+2*vcovHC["x:tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 4):tmm3"]+
    2*vcovHC["x","l(x, 5):tmm3"]+2*vcovHC["l(x, 1)","l(x, 5):tmm3"]+2*vcovHC["l(x, 2)","l(x, 5):tmm3"]+2*vcovHC["l(x, 3)","l(x, 5):tmm3"]+2*vcovHC["l(x, 4)","l(x, 5):tmm3"]+2*vcovHC["l(x, 5)","l(x, 5):tmm3"]+2*vcovHC["l(x, 6)","l(x, 5):tmm3"]+2*vcovHC["x:tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 5):tmm3"]+
    2*vcovHC["x","l(x, 6):tmm3"]+2*vcovHC["l(x, 1)","l(x, 6):tmm3"]+2*vcovHC["l(x, 2)","l(x, 6):tmm3"]+2*vcovHC["l(x, 3)","l(x, 6):tmm3"]+2*vcovHC["l(x, 4)","l(x, 6):tmm3"]+2*vcovHC["l(x, 5)","l(x, 6):tmm3"]+2*vcovHC["l(x, 6)","l(x, 6):tmm3"]+2*vcovHC["x:tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 6):tmm3"]
  var82<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["x:tmm3","x:tmm3"]+vcovHC["l(x, 1):tmm3","l(x, 1):tmm3"]+vcovHC["l(x, 2):tmm3","l(x, 2):tmm3"]+vcovHC["l(x, 3):tmm3","l(x, 3):tmm3"]+vcovHC["l(x, 4):tmm3","l(x, 4):tmm3"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):tmm3","l(x, 5):tmm3"]+vcovHC["l(x, 6):tmm3","l(x, 6):tmm3"]+vcovHC["l(x, 7):tmm3","l(x, 7):tmm3"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","x:tmm3"]+2*vcovHC["l(x, 1)","x:tmm3"]+2*vcovHC["l(x, 2)","x:tmm3"]+2*vcovHC["l(x, 3)","x:tmm3"]+2*vcovHC["l(x, 4)","x:tmm3"]+2*vcovHC["l(x, 5)","x:tmm3"]+2*vcovHC["l(x, 6)","x:tmm3"]+2*vcovHC["l(x, 7)","x:tmm3"]+
    2*vcovHC["x","l(x, 1):tmm3"]+2*vcovHC["l(x, 1)","l(x, 1):tmm3"]+2*vcovHC["l(x, 2)","l(x, 1):tmm3"]+2*vcovHC["l(x, 3)","l(x, 1):tmm3"]+2*vcovHC["l(x, 4)","l(x, 1):tmm3"]+2*vcovHC["l(x, 5)","l(x, 1):tmm3"]+2*vcovHC["l(x, 6)","l(x, 1):tmm3"]+2*vcovHC["l(x, 7)","l(x, 1):tmm3"]+2*vcovHC["x:tmm3","l(x, 1):tmm3"]+
    2*vcovHC["x","l(x, 2):tmm3"]+2*vcovHC["l(x, 1)","l(x, 2):tmm3"]+2*vcovHC["l(x, 2)","l(x, 2):tmm3"]+2*vcovHC["l(x, 3)","l(x, 2):tmm3"]+2*vcovHC["l(x, 4)","l(x, 2):tmm3"]+2*vcovHC["l(x, 5)","l(x, 2):tmm3"]+2*vcovHC["l(x, 6)","l(x, 2):tmm3"]+2*vcovHC["l(x, 7)","l(x, 2):tmm3"]+2*vcovHC["x:tmm3","l(x, 2):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 2):tmm3"]+
    2*vcovHC["x","l(x, 3):tmm3"]+2*vcovHC["l(x, 1)","l(x, 3):tmm3"]+2*vcovHC["l(x, 2)","l(x, 3):tmm3"]+2*vcovHC["l(x, 3)","l(x, 3):tmm3"]+2*vcovHC["l(x, 4)","l(x, 3):tmm3"]+2*vcovHC["l(x, 5)","l(x, 3):tmm3"]+2*vcovHC["l(x, 6)","l(x, 3):tmm3"]+2*vcovHC["l(x, 7)","l(x, 3):tmm3"]+2*vcovHC["x:tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 3):tmm3"]+
    2*vcovHC["x","l(x, 4):tmm3"]+2*vcovHC["l(x, 1)","l(x, 4):tmm3"]+2*vcovHC["l(x, 2)","l(x, 4):tmm3"]+2*vcovHC["l(x, 3)","l(x, 4):tmm3"]+2*vcovHC["l(x, 4)","l(x, 4):tmm3"]+2*vcovHC["l(x, 5)","l(x, 4):tmm3"]+2*vcovHC["l(x, 6)","l(x, 4):tmm3"]+2*vcovHC["l(x, 7)","l(x, 4):tmm3"]+2*vcovHC["x:tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 4):tmm3"]+
    2*vcovHC["x","l(x, 5):tmm3"]+2*vcovHC["l(x, 1)","l(x, 5):tmm3"]+2*vcovHC["l(x, 2)","l(x, 5):tmm3"]+2*vcovHC["l(x, 3)","l(x, 5):tmm3"]+2*vcovHC["l(x, 4)","l(x, 5):tmm3"]+2*vcovHC["l(x, 5)","l(x, 5):tmm3"]+2*vcovHC["l(x, 6)","l(x, 5):tmm3"]+2*vcovHC["l(x, 7)","l(x, 5):tmm3"]+2*vcovHC["x:tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 5):tmm3"]+
    2*vcovHC["x","l(x, 6):tmm3"]+2*vcovHC["l(x, 1)","l(x, 6):tmm3"]+2*vcovHC["l(x, 2)","l(x, 6):tmm3"]+2*vcovHC["l(x, 3)","l(x, 6):tmm3"]+2*vcovHC["l(x, 4)","l(x, 6):tmm3"]+2*vcovHC["l(x, 5)","l(x, 6):tmm3"]+2*vcovHC["l(x, 6)","l(x, 6):tmm3"]+2*vcovHC["l(x, 7)","l(x, 6):tmm3"]+2*vcovHC["x:tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 6):tmm3"]+
    2*vcovHC["x","l(x, 7):tmm3"]+2*vcovHC["l(x, 1)","l(x, 7):tmm3"]+2*vcovHC["l(x, 2)","l(x, 7):tmm3"]+2*vcovHC["l(x, 3)","l(x, 7):tmm3"]+2*vcovHC["l(x, 4)","l(x, 7):tmm3"]+2*vcovHC["l(x, 5)","l(x, 7):tmm3"]+2*vcovHC["l(x, 6)","l(x, 7):tmm3"]+2*vcovHC["l(x, 7)","l(x, 7):tmm3"]+2*vcovHC["x:tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 6):tmm3","l(x, 7):tmm3"]
  var92<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["x:tmm3","x:tmm3"]+vcovHC["l(x, 1):tmm3","l(x, 1):tmm3"]+vcovHC["l(x, 2):tmm3","l(x, 2):tmm3"]+vcovHC["l(x, 3):tmm3","l(x, 3):tmm3"]+vcovHC["l(x, 4):tmm3","l(x, 4):tmm3"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):tmm3","l(x, 5):tmm3"]+vcovHC["l(x, 6):tmm3","l(x, 6):tmm3"]+vcovHC["l(x, 7):tmm3","l(x, 7):tmm3"]+vcovHC["l(x, 8):tmm3","l(x, 8):tmm3"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","x:tmm3"]+2*vcovHC["l(x, 1)","x:tmm3"]+2*vcovHC["l(x, 2)","x:tmm3"]+2*vcovHC["l(x, 3)","x:tmm3"]+2*vcovHC["l(x, 4)","x:tmm3"]+2*vcovHC["l(x, 5)","x:tmm3"]+2*vcovHC["l(x, 6)","x:tmm3"]+2*vcovHC["l(x, 7)","x:tmm3"]+2*vcovHC["l(x, 8)","x:tmm3"]+
    2*vcovHC["x","l(x, 1):tmm3"]+2*vcovHC["l(x, 1)","l(x, 1):tmm3"]+2*vcovHC["l(x, 2)","l(x, 1):tmm3"]+2*vcovHC["l(x, 3)","l(x, 1):tmm3"]+2*vcovHC["l(x, 4)","l(x, 1):tmm3"]+2*vcovHC["l(x, 5)","l(x, 1):tmm3"]+2*vcovHC["l(x, 6)","l(x, 1):tmm3"]+2*vcovHC["l(x, 7)","l(x, 1):tmm3"]+2*vcovHC["l(x, 8)","l(x, 1):tmm3"]+2*vcovHC["x:tmm3","l(x, 1):tmm3"]+
    2*vcovHC["x","l(x, 2):tmm3"]+2*vcovHC["l(x, 1)","l(x, 2):tmm3"]+2*vcovHC["l(x, 2)","l(x, 2):tmm3"]+2*vcovHC["l(x, 3)","l(x, 2):tmm3"]+2*vcovHC["l(x, 4)","l(x, 2):tmm3"]+2*vcovHC["l(x, 5)","l(x, 2):tmm3"]+2*vcovHC["l(x, 6)","l(x, 2):tmm3"]+2*vcovHC["l(x, 7)","l(x, 2):tmm3"]+2*vcovHC["l(x, 8)","l(x, 2):tmm3"]+2*vcovHC["x:tmm3","l(x, 2):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 2):tmm3"]+
    2*vcovHC["x","l(x, 3):tmm3"]+2*vcovHC["l(x, 1)","l(x, 3):tmm3"]+2*vcovHC["l(x, 2)","l(x, 3):tmm3"]+2*vcovHC["l(x, 3)","l(x, 3):tmm3"]+2*vcovHC["l(x, 4)","l(x, 3):tmm3"]+2*vcovHC["l(x, 5)","l(x, 3):tmm3"]+2*vcovHC["l(x, 6)","l(x, 3):tmm3"]+2*vcovHC["l(x, 7)","l(x, 3):tmm3"]+2*vcovHC["l(x, 8)","l(x, 3):tmm3"]+2*vcovHC["x:tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 3):tmm3"]+
    2*vcovHC["x","l(x, 4):tmm3"]+2*vcovHC["l(x, 1)","l(x, 4):tmm3"]+2*vcovHC["l(x, 2)","l(x, 4):tmm3"]+2*vcovHC["l(x, 3)","l(x, 4):tmm3"]+2*vcovHC["l(x, 4)","l(x, 4):tmm3"]+2*vcovHC["l(x, 5)","l(x, 4):tmm3"]+2*vcovHC["l(x, 6)","l(x, 4):tmm3"]+2*vcovHC["l(x, 7)","l(x, 4):tmm3"]+2*vcovHC["l(x, 8)","l(x, 4):tmm3"]+2*vcovHC["x:tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 4):tmm3"]+
    2*vcovHC["x","l(x, 5):tmm3"]+2*vcovHC["l(x, 1)","l(x, 5):tmm3"]+2*vcovHC["l(x, 2)","l(x, 5):tmm3"]+2*vcovHC["l(x, 3)","l(x, 5):tmm3"]+2*vcovHC["l(x, 4)","l(x, 5):tmm3"]+2*vcovHC["l(x, 5)","l(x, 5):tmm3"]+2*vcovHC["l(x, 6)","l(x, 5):tmm3"]+2*vcovHC["l(x, 7)","l(x, 5):tmm3"]+2*vcovHC["l(x, 8)","l(x, 5):tmm3"]+2*vcovHC["x:tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 5):tmm3"]+
    2*vcovHC["x","l(x, 6):tmm3"]+2*vcovHC["l(x, 1)","l(x, 6):tmm3"]+2*vcovHC["l(x, 2)","l(x, 6):tmm3"]+2*vcovHC["l(x, 3)","l(x, 6):tmm3"]+2*vcovHC["l(x, 4)","l(x, 6):tmm3"]+2*vcovHC["l(x, 5)","l(x, 6):tmm3"]+2*vcovHC["l(x, 6)","l(x, 6):tmm3"]+2*vcovHC["l(x, 7)","l(x, 6):tmm3"]+2*vcovHC["l(x, 8)","l(x, 6):tmm3"]+2*vcovHC["x:tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 6):tmm3"]+
    2*vcovHC["x","l(x, 7):tmm3"]+2*vcovHC["l(x, 1)","l(x, 7):tmm3"]+2*vcovHC["l(x, 2)","l(x, 7):tmm3"]+2*vcovHC["l(x, 3)","l(x, 7):tmm3"]+2*vcovHC["l(x, 4)","l(x, 7):tmm3"]+2*vcovHC["l(x, 5)","l(x, 7):tmm3"]+2*vcovHC["l(x, 6)","l(x, 7):tmm3"]+2*vcovHC["l(x, 7)","l(x, 7):tmm3"]+2*vcovHC["l(x, 8)","l(x, 7):tmm3"]+2*vcovHC["x:tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 6):tmm3","l(x, 7):tmm3"]+
    2*vcovHC["x","l(x, 8):tmm3"]+2*vcovHC["l(x, 1)","l(x, 8):tmm3"]+2*vcovHC["l(x, 2)","l(x, 8):tmm3"]+2*vcovHC["l(x, 3)","l(x, 8):tmm3"]+2*vcovHC["l(x, 4)","l(x, 8):tmm3"]+2*vcovHC["l(x, 5)","l(x, 8):tmm3"]+2*vcovHC["l(x, 6)","l(x, 8):tmm3"]+2*vcovHC["l(x, 7)","l(x, 8):tmm3"]+2*vcovHC["l(x, 8)","l(x, 8):tmm3"]+2*vcovHC["x:tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 6):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 7):tmm3","l(x, 8):tmm3"]
  var102<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+vcovHC["x:tmm3","x:tmm3"]+vcovHC["l(x, 1):tmm3","l(x, 1):tmm3"]+vcovHC["l(x, 2):tmm3","l(x, 2):tmm3"]+vcovHC["l(x, 3):tmm3","l(x, 3):tmm3"]+vcovHC["l(x, 4):tmm3","l(x, 4):tmm3"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):tmm3","l(x, 5):tmm3"]+vcovHC["l(x, 6):tmm3","l(x, 6):tmm3"]+vcovHC["l(x, 7):tmm3","l(x, 7):tmm3"]+vcovHC["l(x, 8):tmm3","l(x, 8):tmm3"]+vcovHC["l(x, 9):tmm3","l(x, 9):tmm3"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]+
    2*vcovHC["x","x:tmm3"]+2*vcovHC["l(x, 1)","x:tmm3"]+2*vcovHC["l(x, 2)","x:tmm3"]+2*vcovHC["l(x, 3)","x:tmm3"]+2*vcovHC["l(x, 4)","x:tmm3"]+2*vcovHC["l(x, 5)","x:tmm3"]+2*vcovHC["l(x, 6)","x:tmm3"]+2*vcovHC["l(x, 7)","x:tmm3"]+2*vcovHC["l(x, 8)","x:tmm3"]+2*vcovHC["l(x, 9)","x:tmm3"]+
    2*vcovHC["x","l(x, 1):tmm3"]+2*vcovHC["l(x, 1)","l(x, 1):tmm3"]+2*vcovHC["l(x, 2)","l(x, 1):tmm3"]+2*vcovHC["l(x, 3)","l(x, 1):tmm3"]+2*vcovHC["l(x, 4)","l(x, 1):tmm3"]+2*vcovHC["l(x, 5)","l(x, 1):tmm3"]+2*vcovHC["l(x, 6)","l(x, 1):tmm3"]+2*vcovHC["l(x, 7)","l(x, 1):tmm3"]+2*vcovHC["l(x, 8)","l(x, 1):tmm3"]+2*vcovHC["l(x, 9)","l(x, 1):tmm3"]+2*vcovHC["x:tmm3","l(x, 1):tmm3"]+
    2*vcovHC["x","l(x, 2):tmm3"]+2*vcovHC["l(x, 1)","l(x, 2):tmm3"]+2*vcovHC["l(x, 2)","l(x, 2):tmm3"]+2*vcovHC["l(x, 3)","l(x, 2):tmm3"]+2*vcovHC["l(x, 4)","l(x, 2):tmm3"]+2*vcovHC["l(x, 5)","l(x, 2):tmm3"]+2*vcovHC["l(x, 6)","l(x, 2):tmm3"]+2*vcovHC["l(x, 7)","l(x, 2):tmm3"]+2*vcovHC["l(x, 8)","l(x, 2):tmm3"]+2*vcovHC["l(x, 9)","l(x, 2):tmm3"]+2*vcovHC["x:tmm3","l(x, 2):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 2):tmm3"]+
    2*vcovHC["x","l(x, 3):tmm3"]+2*vcovHC["l(x, 1)","l(x, 3):tmm3"]+2*vcovHC["l(x, 2)","l(x, 3):tmm3"]+2*vcovHC["l(x, 3)","l(x, 3):tmm3"]+2*vcovHC["l(x, 4)","l(x, 3):tmm3"]+2*vcovHC["l(x, 5)","l(x, 3):tmm3"]+2*vcovHC["l(x, 6)","l(x, 3):tmm3"]+2*vcovHC["l(x, 7)","l(x, 3):tmm3"]+2*vcovHC["l(x, 8)","l(x, 3):tmm3"]+2*vcovHC["l(x, 9)","l(x, 3):tmm3"]+2*vcovHC["x:tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 3):tmm3"]+
    2*vcovHC["x","l(x, 4):tmm3"]+2*vcovHC["l(x, 1)","l(x, 4):tmm3"]+2*vcovHC["l(x, 2)","l(x, 4):tmm3"]+2*vcovHC["l(x, 3)","l(x, 4):tmm3"]+2*vcovHC["l(x, 4)","l(x, 4):tmm3"]+2*vcovHC["l(x, 5)","l(x, 4):tmm3"]+2*vcovHC["l(x, 6)","l(x, 4):tmm3"]+2*vcovHC["l(x, 7)","l(x, 4):tmm3"]+2*vcovHC["l(x, 8)","l(x, 4):tmm3"]+2*vcovHC["l(x, 9)","l(x, 4):tmm3"]+2*vcovHC["x:tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 4):tmm3"]+
    2*vcovHC["x","l(x, 5):tmm3"]+2*vcovHC["l(x, 1)","l(x, 5):tmm3"]+2*vcovHC["l(x, 2)","l(x, 5):tmm3"]+2*vcovHC["l(x, 3)","l(x, 5):tmm3"]+2*vcovHC["l(x, 4)","l(x, 5):tmm3"]+2*vcovHC["l(x, 5)","l(x, 5):tmm3"]+2*vcovHC["l(x, 6)","l(x, 5):tmm3"]+2*vcovHC["l(x, 7)","l(x, 5):tmm3"]+2*vcovHC["l(x, 8)","l(x, 5):tmm3"]+2*vcovHC["l(x, 9)","l(x, 5):tmm3"]+2*vcovHC["x:tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 5):tmm3"]+
    2*vcovHC["x","l(x, 6):tmm3"]+2*vcovHC["l(x, 1)","l(x, 6):tmm3"]+2*vcovHC["l(x, 2)","l(x, 6):tmm3"]+2*vcovHC["l(x, 3)","l(x, 6):tmm3"]+2*vcovHC["l(x, 4)","l(x, 6):tmm3"]+2*vcovHC["l(x, 5)","l(x, 6):tmm3"]+2*vcovHC["l(x, 6)","l(x, 6):tmm3"]+2*vcovHC["l(x, 7)","l(x, 6):tmm3"]+2*vcovHC["l(x, 8)","l(x, 6):tmm3"]+2*vcovHC["l(x, 9)","l(x, 6):tmm3"]+2*vcovHC["x:tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 6):tmm3"]+
    2*vcovHC["x","l(x, 7):tmm3"]+2*vcovHC["l(x, 1)","l(x, 7):tmm3"]+2*vcovHC["l(x, 2)","l(x, 7):tmm3"]+2*vcovHC["l(x, 3)","l(x, 7):tmm3"]+2*vcovHC["l(x, 4)","l(x, 7):tmm3"]+2*vcovHC["l(x, 5)","l(x, 7):tmm3"]+2*vcovHC["l(x, 6)","l(x, 7):tmm3"]+2*vcovHC["l(x, 7)","l(x, 7):tmm3"]+2*vcovHC["l(x, 8)","l(x, 7):tmm3"]+2*vcovHC["l(x, 9)","l(x, 7):tmm3"]+2*vcovHC["x:tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 6):tmm3","l(x, 7):tmm3"]+
    2*vcovHC["x","l(x, 8):tmm3"]+2*vcovHC["l(x, 1)","l(x, 8):tmm3"]+2*vcovHC["l(x, 2)","l(x, 8):tmm3"]+2*vcovHC["l(x, 3)","l(x, 8):tmm3"]+2*vcovHC["l(x, 4)","l(x, 8):tmm3"]+2*vcovHC["l(x, 5)","l(x, 8):tmm3"]+2*vcovHC["l(x, 6)","l(x, 8):tmm3"]+2*vcovHC["l(x, 7)","l(x, 8):tmm3"]+2*vcovHC["l(x, 8)","l(x, 8):tmm3"]+2*vcovHC["l(x, 9)","l(x, 8):tmm3"]+2*vcovHC["x:tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 6):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 7):tmm3","l(x, 8):tmm3"]+
    2*vcovHC["x","l(x, 9):tmm3"]+2*vcovHC["l(x, 1)","l(x, 9):tmm3"]+2*vcovHC["l(x, 2)","l(x, 9):tmm3"]+2*vcovHC["l(x, 3)","l(x, 9):tmm3"]+2*vcovHC["l(x, 4)","l(x, 9):tmm3"]+2*vcovHC["l(x, 5)","l(x, 9):tmm3"]+2*vcovHC["l(x, 6)","l(x, 9):tmm3"]+2*vcovHC["l(x, 7)","l(x, 9):tmm3"]+2*vcovHC["l(x, 8)","l(x, 9):tmm3"]+2*vcovHC["l(x, 9)","l(x, 9):tmm3"]+2*vcovHC["x:tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 6):tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 7):tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 8):tmm3","l(x, 9):tmm3"]
  var112<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+vcovHC["l(x, 10)","l(x, 10)"]+vcovHC["x:tmm3","x:tmm3"]+vcovHC["l(x, 1):tmm3","l(x, 1):tmm3"]+vcovHC["l(x, 2):tmm3","l(x, 2):tmm3"]+vcovHC["l(x, 3):tmm3","l(x, 3):tmm3"]+vcovHC["l(x, 4):tmm3","l(x, 4):tmm3"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):tmm3","l(x, 5):tmm3"]+vcovHC["l(x, 6):tmm3","l(x, 6):tmm3"]+vcovHC["l(x, 7):tmm3","l(x, 7):tmm3"]+vcovHC["l(x, 8):tmm3","l(x, 8):tmm3"]+vcovHC["l(x, 9):tmm3","l(x, 9):tmm3"]+vcovHC["l(x, 10):tmm3","l(x, 10):tmm3"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]+
    2*vcovHC["x","l(x, 10)"]+2*vcovHC["l(x, 1)","l(x, 10)"]+2*vcovHC["l(x, 2)","l(x, 10)"]+2*vcovHC["l(x, 3)","l(x, 10)"]+2*vcovHC["l(x, 4)","l(x, 10)"]+2*vcovHC["l(x, 5)","l(x, 10)"]+2*vcovHC["l(x, 6)","l(x, 10)"]+2*vcovHC["l(x, 7)","l(x, 10)"]+2*vcovHC["l(x, 8)","l(x, 10)"]+2*vcovHC["l(x, 9)","l(x, 10)"]+
    2*vcovHC["x","x:tmm3"]+2*vcovHC["l(x, 1)","x:tmm3"]+2*vcovHC["l(x, 2)","x:tmm3"]+2*vcovHC["l(x, 3)","x:tmm3"]+2*vcovHC["l(x, 4)","x:tmm3"]+2*vcovHC["l(x, 5)","x:tmm3"]+2*vcovHC["l(x, 6)","x:tmm3"]+2*vcovHC["l(x, 7)","x:tmm3"]+2*vcovHC["l(x, 8)","x:tmm3"]+2*vcovHC["l(x, 9)","x:tmm3"]+2*vcovHC["l(x, 10)","x:tmm3"]+
    2*vcovHC["x","l(x, 1):tmm3"]+2*vcovHC["l(x, 1)","l(x, 1):tmm3"]+2*vcovHC["l(x, 2)","l(x, 1):tmm3"]+2*vcovHC["l(x, 3)","l(x, 1):tmm3"]+2*vcovHC["l(x, 4)","l(x, 1):tmm3"]+2*vcovHC["l(x, 5)","l(x, 1):tmm3"]+2*vcovHC["l(x, 6)","l(x, 1):tmm3"]+2*vcovHC["l(x, 7)","l(x, 1):tmm3"]+2*vcovHC["l(x, 8)","l(x, 1):tmm3"]+2*vcovHC["l(x, 9)","l(x, 1):tmm3"]+2*vcovHC["l(x, 10)","l(x, 1):tmm3"]+2*vcovHC["x:tmm3","l(x, 1):tmm3"]+
    2*vcovHC["x","l(x, 2):tmm3"]+2*vcovHC["l(x, 1)","l(x, 2):tmm3"]+2*vcovHC["l(x, 2)","l(x, 2):tmm3"]+2*vcovHC["l(x, 3)","l(x, 2):tmm3"]+2*vcovHC["l(x, 4)","l(x, 2):tmm3"]+2*vcovHC["l(x, 5)","l(x, 2):tmm3"]+2*vcovHC["l(x, 6)","l(x, 2):tmm3"]+2*vcovHC["l(x, 7)","l(x, 2):tmm3"]+2*vcovHC["l(x, 8)","l(x, 2):tmm3"]+2*vcovHC["l(x, 9)","l(x, 2):tmm3"]+2*vcovHC["l(x, 10)","l(x, 2):tmm3"]+2*vcovHC["x:tmm3","l(x, 2):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 2):tmm3"]+
    2*vcovHC["x","l(x, 3):tmm3"]+2*vcovHC["l(x, 1)","l(x, 3):tmm3"]+2*vcovHC["l(x, 2)","l(x, 3):tmm3"]+2*vcovHC["l(x, 3)","l(x, 3):tmm3"]+2*vcovHC["l(x, 4)","l(x, 3):tmm3"]+2*vcovHC["l(x, 5)","l(x, 3):tmm3"]+2*vcovHC["l(x, 6)","l(x, 3):tmm3"]+2*vcovHC["l(x, 7)","l(x, 3):tmm3"]+2*vcovHC["l(x, 8)","l(x, 3):tmm3"]+2*vcovHC["l(x, 9)","l(x, 3):tmm3"]+2*vcovHC["l(x, 10)","l(x, 3):tmm3"]+2*vcovHC["x:tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 3):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 3):tmm3"]+
    2*vcovHC["x","l(x, 4):tmm3"]+2*vcovHC["l(x, 1)","l(x, 4):tmm3"]+2*vcovHC["l(x, 2)","l(x, 4):tmm3"]+2*vcovHC["l(x, 3)","l(x, 4):tmm3"]+2*vcovHC["l(x, 4)","l(x, 4):tmm3"]+2*vcovHC["l(x, 5)","l(x, 4):tmm3"]+2*vcovHC["l(x, 6)","l(x, 4):tmm3"]+2*vcovHC["l(x, 7)","l(x, 4):tmm3"]+2*vcovHC["l(x, 8)","l(x, 4):tmm3"]+2*vcovHC["l(x, 9)","l(x, 4):tmm3"]+2*vcovHC["l(x, 10)","l(x, 4):tmm3"]+2*vcovHC["x:tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 4):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 4):tmm3"]+
    2*vcovHC["x","l(x, 5):tmm3"]+2*vcovHC["l(x, 1)","l(x, 5):tmm3"]+2*vcovHC["l(x, 2)","l(x, 5):tmm3"]+2*vcovHC["l(x, 3)","l(x, 5):tmm3"]+2*vcovHC["l(x, 4)","l(x, 5):tmm3"]+2*vcovHC["l(x, 5)","l(x, 5):tmm3"]+2*vcovHC["l(x, 6)","l(x, 5):tmm3"]+2*vcovHC["l(x, 7)","l(x, 5):tmm3"]+2*vcovHC["l(x, 8)","l(x, 5):tmm3"]+2*vcovHC["l(x, 9)","l(x, 5):tmm3"]+2*vcovHC["l(x, 10)","l(x, 5):tmm3"]+2*vcovHC["x:tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 5):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 5):tmm3"]+
    2*vcovHC["x","l(x, 6):tmm3"]+2*vcovHC["l(x, 1)","l(x, 6):tmm3"]+2*vcovHC["l(x, 2)","l(x, 6):tmm3"]+2*vcovHC["l(x, 3)","l(x, 6):tmm3"]+2*vcovHC["l(x, 4)","l(x, 6):tmm3"]+2*vcovHC["l(x, 5)","l(x, 6):tmm3"]+2*vcovHC["l(x, 6)","l(x, 6):tmm3"]+2*vcovHC["l(x, 7)","l(x, 6):tmm3"]+2*vcovHC["l(x, 8)","l(x, 6):tmm3"]+2*vcovHC["l(x, 9)","l(x, 6):tmm3"]+2*vcovHC["l(x, 10)","l(x, 6):tmm3"]+2*vcovHC["x:tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 6):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 6):tmm3"]+
    2*vcovHC["x","l(x, 7):tmm3"]+2*vcovHC["l(x, 1)","l(x, 7):tmm3"]+2*vcovHC["l(x, 2)","l(x, 7):tmm3"]+2*vcovHC["l(x, 3)","l(x, 7):tmm3"]+2*vcovHC["l(x, 4)","l(x, 7):tmm3"]+2*vcovHC["l(x, 5)","l(x, 7):tmm3"]+2*vcovHC["l(x, 6)","l(x, 7):tmm3"]+2*vcovHC["l(x, 7)","l(x, 7):tmm3"]+2*vcovHC["l(x, 8)","l(x, 7):tmm3"]+2*vcovHC["l(x, 9)","l(x, 7):tmm3"]+2*vcovHC["l(x, 10)","l(x, 7):tmm3"]+2*vcovHC["x:tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 7):tmm3"]+2*vcovHC["l(x, 6):tmm3","l(x, 7):tmm3"]+
    2*vcovHC["x","l(x, 8):tmm3"]+2*vcovHC["l(x, 1)","l(x, 8):tmm3"]+2*vcovHC["l(x, 2)","l(x, 8):tmm3"]+2*vcovHC["l(x, 3)","l(x, 8):tmm3"]+2*vcovHC["l(x, 4)","l(x, 8):tmm3"]+2*vcovHC["l(x, 5)","l(x, 8):tmm3"]+2*vcovHC["l(x, 6)","l(x, 8):tmm3"]+2*vcovHC["l(x, 7)","l(x, 8):tmm3"]+2*vcovHC["l(x, 8)","l(x, 8):tmm3"]+2*vcovHC["l(x, 9)","l(x, 8):tmm3"]+2*vcovHC["l(x, 10)","l(x, 8):tmm3"]+2*vcovHC["x:tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 6):tmm3","l(x, 8):tmm3"]+2*vcovHC["l(x, 7):tmm3","l(x, 8):tmm3"]+
    2*vcovHC["x","l(x, 9):tmm3"]+2*vcovHC["l(x, 1)","l(x, 9):tmm3"]+2*vcovHC["l(x, 2)","l(x, 9):tmm3"]+2*vcovHC["l(x, 3)","l(x, 9):tmm3"]+2*vcovHC["l(x, 4)","l(x, 9):tmm3"]+2*vcovHC["l(x, 5)","l(x, 9):tmm3"]+2*vcovHC["l(x, 6)","l(x, 9):tmm3"]+2*vcovHC["l(x, 7)","l(x, 9):tmm3"]+2*vcovHC["l(x, 8)","l(x, 9):tmm3"]+2*vcovHC["l(x, 9)","l(x, 9):tmm3"]+2*vcovHC["l(x, 10)","l(x, 9):tmm3"]+2*vcovHC["x:tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 6):tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 7):tmm3","l(x, 9):tmm3"]+2*vcovHC["l(x, 8):tmm3","l(x, 9):tmm3"]+
    2*vcovHC["x","l(x, 10):tmm3"]+2*vcovHC["l(x, 1)","l(x, 10):tmm3"]+2*vcovHC["l(x, 2)","l(x, 10):tmm3"]+2*vcovHC["l(x, 3)","l(x, 10):tmm3"]+2*vcovHC["l(x, 4)","l(x, 10):tmm3"]+2*vcovHC["l(x, 5)","l(x, 10):tmm3"]+2*vcovHC["l(x, 6)","l(x, 10):tmm3"]+2*vcovHC["l(x, 7)","l(x, 10):tmm3"]+2*vcovHC["l(x, 8)","l(x, 10):tmm3"]+2*vcovHC["l(x, 9)","l(x, 10):tmm3"]+2*vcovHC["l(x, 10)","l(x, 10):tmm3"]+2*vcovHC["x:tmm3","l(x, 10):tmm3"]+2*vcovHC["l(x, 1):tmm3","l(x, 10):tmm3"]+2*vcovHC["l(x, 2):tmm3","l(x, 10):tmm3"]+2*vcovHC["l(x, 3):tmm3","l(x, 10):tmm3"]+2*vcovHC["l(x, 4):tmm3","l(x, 10):tmm3"]+2*vcovHC["l(x, 5):tmm3","l(x, 10):tmm3"]+2*vcovHC["l(x, 6):tmm3","l(x, 10):tmm3"]+2*vcovHC["l(x, 7):tmm3","l(x, 10):tmm3"]+2*vcovHC["l(x, 8):tmm3","l(x, 10):tmm3"]+2*vcovHC["l(x, 9):tmm3","l(x, 10):tmm3"]
  
  varsums2<-c(var12,var22,var32,var42,var52,var62,var72,var82,var92,var102,var112)
  sdsums2<-varsums2^0.5
  
  singlecoeff12<-mod$coefficients[c("x:tmm3", "l(x, 1):tmm3", "l(x, 2):tmm3", "l(x, 3):tmm3", "l(x, 4):tmm3", "l(x, 5):tmm3", "l(x, 6):tmm3", "l(x, 7):tmm3", "l(x, 8):tmm3", "l(x, 9):tmm3", "l(x, 10):tmm3")]
  
  coeffsums12<-cumsum(singlecoeff12) + coeffsums10
  
  sdsums<-c(sdsums0,sdsums1, sdsums2)
  coeffsums<-c(coeffsums10,coeffsums11, coeffsums12)
  coeffsums_sd<-coeffsums*sd(dat$x)
  sdsums_sd<-sdsums*sd(dat$x)
  
  lower<-coeffsums_sd-sdsums_sd*1.96
  upper<-coeffsums_sd+sdsums_sd*1.96
  
  datar<-cbind(coeffsums_sd,sdsums_sd,lower,upper, coeffsums, sdsums)
  datar<-as.data.frame(datar)
  datar$year <- rep(0:(nrow(datar)/3-1),3)
  
  datar$tmm <- 1
  datar$tmm[grepl("tmm2", rownames(datar), fixed = TRUE)] <- 2
  datar$tmm[grepl("tmm3", rownames(datar), fixed = TRUE)] <- 3
  
  return(datar)
}


#### 8 lags ####
getcumul8_f <-function(mod, dat, stata_vcov=NULL, stata_beta=NULL, var="x", clust="Area", ssc=T){
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
      filter(var%in%c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8")) %>% 
      select(c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8")) %>% 
      as.matrix()
    
    colnames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)")
    rownames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)") 
    
  }
  
  var1<-vcovHC["x","x"]
  var2<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+
    2*vcovHC["x","l(x, 1)"]
  var3<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]
  var4<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]
  var5<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]
  var6<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]
  var7<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]
  var8<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]
  var9<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]
  
  varsums<-c(var1,var2,var3,var4,var5,var6,var7,var8,var9)
  sdsums<-varsums^0.5
  
  #estimates from feols or stata
  if(is.null(stata_vcov) | is.null(stata_beta)){
    singlecoeff<-mod$coefficients[c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)")]
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
      filter(X1%in%c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8")) %>% 
      mutate(X2 = as.numeric(X2)) %>% 
      rename(var=X1, coefficients=X2) %>% filter(
        var %in% c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8")
      ) %>% pull(coefficients)
  }
  coeffsums<-cumsum(singlecoeff)
  
  coeffsums_sd<-coeffsums*sd(dat[,var] %>% pull())
  sdsums_sd<-sdsums*sd(dat[,var] %>% pull())
  
  lower<-coeffsums_sd-sdsums_sd*1.96
  upper<-coeffsums_sd+sdsums_sd*1.96
  datar<-cbind(coeffsums_sd,sdsums_sd,lower,upper, coeffsums, sdsums)
  datar<-as.data.frame(datar)
  datar$year <- 0:(nrow(datar)-1)
  return(datar)
}


#### 5 lags ####
getcumul5_f <-function(mod, dat, stata_vcov=NULL, stata_beta=NULL, var="x", clust="Area", ssc=T){
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
      filter(var%in%c("S4","lag1","lag2","lag3","lag4","lag5")) %>% 
      select(c("S4","lag1","lag2","lag3","lag4","lag5")) %>% 
      as.matrix()
    
    colnames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)")
    rownames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)") 
    
  }
  
  var1<-vcovHC["x","x"]
  var2<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+
    2*vcovHC["x","l(x, 1)"]
  var3<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]
  var4<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]
  var5<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]
  var6<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]
  
  varsums<-c(var1,var2,var3,var4,var5,var6)
  sdsums<-varsums^0.5
  
  #estimates from feols or stata
  if(is.null(stata_vcov) | is.null(stata_beta)){
    singlecoeff<-mod$coefficients[c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)")]
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
      filter(X1%in%c("S4","lag1","lag2","lag3","lag4","lag5")) %>% 
      mutate(X2 = as.numeric(X2)) %>% 
      rename(var=X1, coefficients=X2) %>% filter(
        var %in% c("S4","lag1","lag2","lag3","lag4","lag5")
      ) %>% pull(coefficients)
  }
  
  coeffsums<-cumsum(singlecoeff)
  
  coeffsums_sd<-coeffsums*sd(dat[,var])
  sdsums_sd<-sdsums*sd(dat[,var])
  
  lower<-coeffsums_sd-sdsums_sd*1.96
  upper<-coeffsums_sd+sdsums_sd*1.96
  datar<-cbind(coeffsums_sd,sdsums_sd,lower,upper, coeffsums, sdsums)
  datar<-as.data.frame(datar)
  datar$year <- 0:(nrow(datar)-1)
  return(datar)
}


#### 12 lags ####
getcumul12_f <-function(mod, dat, stata_vcov=NULL, stata_beta=NULL, var="x", clust="Area", ssc=T){
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
      filter(var%in%c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12")) %>% 
      select(c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12")) %>% 
      as.matrix()
    
    colnames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", "l(x, 9)", "l(x, 10)", "l(x, 11)", "l(x, 12)")
    rownames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", "l(x, 9)", "l(x, 10)", "l(x, 11)", "l(x, 12)") 
    
  }
  
  var1<-vcovHC["x","x"]
  var2<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+
    2*vcovHC["x","l(x, 1)"]
  var3<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]
  var4<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]
  var5<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]
  var6<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]
  var7<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]
  var8<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]
  var9<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]
  var10<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]
  var11<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+vcovHC["l(x, 10)","l(x, 10)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]+
    2*vcovHC["x","l(x, 10)"]+2*vcovHC["l(x, 1)","l(x, 10)"]+2*vcovHC["l(x, 2)","l(x, 10)"]+2*vcovHC["l(x, 3)","l(x, 10)"]+2*vcovHC["l(x, 4)","l(x, 10)"]+2*vcovHC["l(x, 5)","l(x, 10)"]+2*vcovHC["l(x, 6)","l(x, 10)"]+2*vcovHC["l(x, 7)","l(x, 10)"]+2*vcovHC["l(x, 8)","l(x, 10)"]+2*vcovHC["l(x, 9)","l(x, 10)"]
  
  var12<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+vcovHC["l(x, 10)","l(x, 10)"]+vcovHC["l(x, 11)","l(x, 11)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]+
    2*vcovHC["x","l(x, 10)"]+2*vcovHC["l(x, 1)","l(x, 10)"]+2*vcovHC["l(x, 2)","l(x, 10)"]+2*vcovHC["l(x, 3)","l(x, 10)"]+2*vcovHC["l(x, 4)","l(x, 10)"]+2*vcovHC["l(x, 5)","l(x, 10)"]+2*vcovHC["l(x, 6)","l(x, 10)"]+2*vcovHC["l(x, 7)","l(x, 10)"]+2*vcovHC["l(x, 8)","l(x, 10)"]+2*vcovHC["l(x, 9)","l(x, 10)"] +
    2*vcovHC["x","l(x, 11)"]+2*vcovHC["l(x, 1)","l(x, 11)"]+2*vcovHC["l(x, 2)","l(x, 11)"]+2*vcovHC["l(x, 3)","l(x, 11)"]+2*vcovHC["l(x, 4)","l(x, 11)"]+2*vcovHC["l(x, 5)","l(x, 11)"]+2*vcovHC["l(x, 6)","l(x, 11)"]+2*vcovHC["l(x, 7)","l(x, 11)"]+2*vcovHC["l(x, 8)","l(x, 11)"]+2*vcovHC["l(x, 9)","l(x, 11)"]+2*vcovHC["l(x, 10)","l(x, 11)"]
  
  var13<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+vcovHC["l(x, 10)","l(x, 10)"]+vcovHC["l(x, 11)","l(x, 11)"]+vcovHC["l(x, 12)","l(x, 12)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","l(x, 9)"]+2*vcovHC["l(x, 1)","l(x, 9)"]+2*vcovHC["l(x, 2)","l(x, 9)"]+2*vcovHC["l(x, 3)","l(x, 9)"]+2*vcovHC["l(x, 4)","l(x, 9)"]+2*vcovHC["l(x, 5)","l(x, 9)"]+2*vcovHC["l(x, 6)","l(x, 9)"]+2*vcovHC["l(x, 7)","l(x, 9)"]+2*vcovHC["l(x, 8)","l(x, 9)"]+
    2*vcovHC["x","l(x, 10)"]+2*vcovHC["l(x, 1)","l(x, 10)"]+2*vcovHC["l(x, 2)","l(x, 10)"]+2*vcovHC["l(x, 3)","l(x, 10)"]+2*vcovHC["l(x, 4)","l(x, 10)"]+2*vcovHC["l(x, 5)","l(x, 10)"]+2*vcovHC["l(x, 6)","l(x, 10)"]+2*vcovHC["l(x, 7)","l(x, 10)"]+2*vcovHC["l(x, 8)","l(x, 10)"]+2*vcovHC["l(x, 9)","l(x, 10)"] +
    2*vcovHC["x","l(x, 11)"]+2*vcovHC["l(x, 1)","l(x, 11)"]+2*vcovHC["l(x, 2)","l(x, 11)"]+2*vcovHC["l(x, 3)","l(x, 11)"]+2*vcovHC["l(x, 4)","l(x, 11)"]+2*vcovHC["l(x, 5)","l(x, 11)"]+2*vcovHC["l(x, 6)","l(x, 11)"]+2*vcovHC["l(x, 7)","l(x, 11)"]+2*vcovHC["l(x, 8)","l(x, 11)"]+2*vcovHC["l(x, 9)","l(x, 11)"]+2*vcovHC["l(x, 10)","l(x, 11)"] +
    2*vcovHC["x","l(x, 12)"]+2*vcovHC["l(x, 1)","l(x, 12)"]+2*vcovHC["l(x, 2)","l(x, 12)"]+2*vcovHC["l(x, 3)","l(x, 12)"]+2*vcovHC["l(x, 4)","l(x, 12)"]+2*vcovHC["l(x, 5)","l(x, 12)"]+2*vcovHC["l(x, 6)","l(x, 12)"]+2*vcovHC["l(x, 7)","l(x, 12)"]+2*vcovHC["l(x, 8)","l(x, 12)"]+2*vcovHC["l(x, 9)","l(x, 12)"]+2*vcovHC["l(x, 10)","l(x, 12)"] +2*vcovHC["l(x, 11)","l(x, 12)"]
  
  varsums<-c(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11, var12, var13)
  sdsums<-varsums^0.5
  
  #estimates from feols or stata
  if(is.null(stata_vcov) | is.null(stata_beta)){
    singlecoeff<-mod$coefficients[c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", "l(x, 9)", "l(x, 10)", "l(x, 11)", "l(x, 12)")]
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
      filter(X1%in%c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12")) %>% 
      mutate(X2 = as.numeric(X2)) %>% 
      rename(var=X1, coefficients=X2) %>% filter(
        var %in% c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10","lag11","lag12")
      ) %>% pull(coefficients)
  }
  coeffsums<-cumsum(singlecoeff)
  
  coeffsums_sd<-coeffsums*sd(dat[,var] %>% pull())
  sdsums_sd<-sdsums*sd(dat[,var] %>% pull())
  
  lower<-coeffsums_sd-sdsums_sd*1.96
  upper<-coeffsums_sd+sdsums_sd*1.96
  datar<-cbind(coeffsums_sd,sdsums_sd,lower,upper, coeffsums, sdsums)
  datar<-as.data.frame(datar)
  datar$year <- 0:(nrow(datar)-1)
  return(datar)
}

#### Others ####

get_leads <- function(mod, dat, stata_vcov=NULL, stata_beta=NULL, var="x", clust="Area", ssc=T){
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
      filter(var%in%c("lead2", "lead3", "lead4")) %>% 
      select(c("lead2", "lead3", "lead4")) %>% 
      as.matrix()
    
    colnames(vcovHC) <- c("f(x, 2)", "f(x, 3)", "f(x, 4)")
    rownames(vcovHC) <- c("f(x, 2)", "f(x, 3)", "f(x, 4)") 
    
  }
  
  vl2 <- vcovHC["f(x, 2)", "f(x, 2)"]
  vl3 <- vcovHC["f(x, 2)", "f(x, 2)"] + vcovHC["f(x, 3)", "f(x, 3)"] + 2*vcovHC["f(x, 2)", "f(x, 3)"]
  vl4 <- vcovHC["f(x, 2)", "f(x, 2)"] + vcovHC["f(x, 3)", "f(x, 3)"] + vcovHC["f(x, 4)", "f(x, 4)"] + 
    2*vcovHC["f(x, 2)", "f(x, 3)"] + 2*vcovHC["f(x, 3)", "f(x, 4)"] + 2*vcovHC["f(x, 2)", "f(x, 4)"]
  
  varsums<-c(vl2,vl3,vl4)
  sdsums<-varsums^0.5
  
  singlecoeff<-mod$coefficients[c("f(x, 2)", "f(x, 3)", "f(x, 4)")]
  coeffsums<-cumsum(singlecoeff)
  
  
  coeffsums_sd<- -coeffsums*sd(dat[,var] %>% pull())
  
  
  sdsums_sd<-sdsums*sd(dat[,var] %>% pull())
  
  lower<-coeffsums_sd-sdsums_sd*1.96
  upper<-coeffsums_sd+sdsums_sd*1.96
  
  datar<-cbind(coeffsums_sd,sdsums_sd,lower,upper)
  datar<-as.data.frame(datar)
  return(datar)
}


stars_f<- function(p, latex=F){
  if(latex){
    if(p<0.001){
      return("$^{***}$")
    }else{
      if(p<0.01){
        return("$^{**}$")
      }else{
        if(p<0.05){
          return("$^{*}$")
        }else{
          if(p<0.1){
            return(".")
          }else{
            return("")
          }
        }
      }
    }
  }else{
    if(p<0.001){
      return("***")
    }else{
      if(p<0.01){
        return("**")
      }else{
        if(p<0.05){
          return("*")
        }else{
          if(p<0.1){
            return(".")
          }else{
            return("")
          }
        }
      }
    }
  }
}


tablize_f <- function(cumul, mod, tmm=F, dmm=F, latex=F){
  
  cumul %>% 
    select(coeffsums_sd, sdsums_sd, year) %>% 
    rename(coeffsums=coeffsums_sd, sdsums=sdsums_sd) -> x

  x %<>% 
    as.data.frame() %>%
    rownames_to_column() %>% 
    as_tibble() %>% 
    mutate(
      cum_t.value=coeffsums/sdsums,
      cum_p.value=2*pt(q=abs(cum_t.value), df=degrees_freedom(mod, type="t"), lower.tail=FALSE)
    )
  
  x %<>% 
    rowwise() %>% 
    mutate(
      coeffsums=paste0(round(coeffsums,3),stars_f(cum_p.value, latex)),
      sdsums=paste0("(",round(sdsums,3),")")
    ) %>% 
    select(-cum_t.value, -cum_p.value)
  
  if(tmm==T){
    x %<>%
      mutate(
        grp=case_when(
          grepl("tmm2", rowname, fixed = TRUE) ~ 2,
          grepl("tmm3", rowname, fixed = TRUE) ~ 3,
          T ~ 1
        )
      )
  }else{
    if(dmm==T){
      x %<>%
        mutate(
          grp=case_when(
            grepl("dmm", rowname, fixed = TRUE) ~ 1,
            T ~ 0
          )
        )
    }
  }
  
  if(dmm==T | tmm==T){
    x %<>% 
      select(-rowname) %>% 
      pivot_longer(cols = c(coeffsums, sdsums), names_to = "name", values_to="val") %>% 
      pivot_wider(names_from = grp, values_from = val) 
  }else{
    x %<>% 
      select(-rowname) %>% 
      pivot_longer(cols = c(coeffsums, sdsums), names_to = "name", values_to="val")
  }
  x %<>% select(-name)
  return(x)
}
