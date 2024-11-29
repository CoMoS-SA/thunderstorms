getcumul_d10_dyn<- function(mod, dat, stata_vcov=NULL, stata_beta=NULL, var="x", clust="Area", ssc=T){
  
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
                      "S4_dmm", "lag1_dmm_lag1", "lag2_dmm_lag2","lag3_dmm_lag3", "lag4_dmm_lag4", "lag5_dmm_lag5",
                      "lag6_dmm_lag6", "lag7_dmm_lag7", "lag8_dmm_lag8", "lag9_dmm_lag9", "lag10_dmm_lag10"
                      )) %>% 
      select(c("S4","lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","lag9","lag10",
               "S4_dmm", "lag1_dmm_lag1", "lag2_dmm_lag2","lag3_dmm_lag3", "lag4_dmm_lag4", "lag5_dmm_lag5",
               "lag6_dmm_lag6", "lag7_dmm_lag7", "lag8_dmm_lag8", "lag9_dmm_lag9", "lag10_dmm_lag10"
               )) %>% 
      as.matrix()
    

    
    colnames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", "l(x, 9)", "l(x, 10)",
                          "x:dmm", "l(x, 1):l(dmm, 1)","l(x, 2):l(dmm, 2)","l(x, 3):l(dmm, 3)","l(x, 4):l(dmm, 4)",
                          "l(x, 5):l(dmm, 5)","l(x, 6):l(dmm, 6)","l(x, 7):l(dmm, 7)","l(x, 8):l(dmm, 8)",
                          "l(x, 9):l(dmm, 9)","l(x, 10):l(dmm, 10)")
    rownames(vcovHC) <- c("x", "l(x, 1)", "l(x, 2)", "l(x, 3)", "l(x, 4)", "l(x, 5)", "l(x, 6)", "l(x, 7)", "l(x, 8)", "l(x, 9)", "l(x, 10)",
                          "x:dmm", "l(x, 1):l(dmm, 1)","l(x, 2):l(dmm, 2)","l(x, 3):l(dmm, 3)","l(x, 4):l(dmm, 4)",
                          "l(x, 5):l(dmm, 5)","l(x, 6):l(dmm, 6)","l(x, 7):l(dmm, 7)","l(x, 8):l(dmm, 8)",
                          "l(x, 9):l(dmm, 9)","l(x, 10):l(dmm, 10)") 
    
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
  var21<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):l(dmm, 1)","l(x, 1):l(dmm, 1)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+
    2*vcovHC["x","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 1)","l(x, 1):l(dmm, 1)"]+2*vcovHC["x:dmm","l(x, 1):l(dmm, 1)"]
  var31<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):l(dmm, 1)","l(x, 1):l(dmm, 1)"]+vcovHC["l(x, 2):l(dmm, 2)","l(x, 2):l(dmm, 2)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+
    2*vcovHC["x","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 1)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 2)","l(x, 1):l(dmm, 1)"]+2*vcovHC["x:dmm","l(x, 1):l(dmm, 1)"]+
    2*vcovHC["x","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 2)","l(x, 2):l(dmm, 2)"]+2*vcovHC["x:dmm","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 2):l(dmm, 2)"]
  var41<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):l(dmm, 1)","l(x, 1):l(dmm, 1)"]+vcovHC["l(x, 2):l(dmm, 2)","l(x, 2):l(dmm, 2)"]+vcovHC["l(x, 3):l(dmm, 3)","l(x, 3):l(dmm, 3)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+2*vcovHC["l(x, 3)","x:dmm"]+
    2*vcovHC["x","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 1)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 2)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 3)","l(x, 1):l(dmm, 1)"]+2*vcovHC["x:dmm","l(x, 1):l(dmm, 1)"]+
    2*vcovHC["x","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 2)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 3)","l(x, 2):l(dmm, 2)"]+2*vcovHC["x:dmm","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 2):l(dmm, 2)"]+
    2*vcovHC["x","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 3)","l(x, 3):l(dmm, 3)"]+2*vcovHC["x:dmm","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 3):l(dmm, 3)"]
  var51<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):l(dmm, 1)","l(x, 1):l(dmm, 1)"]+vcovHC["l(x, 2):l(dmm, 2)","l(x, 2):l(dmm, 2)"]+vcovHC["l(x, 3):l(dmm, 3)","l(x, 3):l(dmm, 3)"]+vcovHC["l(x, 4):l(dmm, 4)","l(x, 4):l(dmm, 4)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+2*vcovHC["l(x, 3)","x:dmm"]+2*vcovHC["l(x, 4)","x:dmm"]+
    2*vcovHC["x","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 1)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 2)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 3)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 4)","l(x, 1):l(dmm, 1)"]+2*vcovHC["x:dmm","l(x, 1):l(dmm, 1)"]+
    2*vcovHC["x","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 2)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 3)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 4)","l(x, 2):l(dmm, 2)"]+2*vcovHC["x:dmm","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 2):l(dmm, 2)"]+
    2*vcovHC["x","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 3)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 4)","l(x, 3):l(dmm, 3)"]+2*vcovHC["x:dmm","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 3):l(dmm, 3)"]+
    2*vcovHC["x","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 1)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 2)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 3)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 4)","l(x, 4):l(dmm, 4)"]+2*vcovHC["x:dmm","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 4):l(dmm, 4)"]
  
  var61<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):l(dmm, 1)","l(x, 1):l(dmm, 1)"]+vcovHC["l(x, 2):l(dmm, 2)","l(x, 2):l(dmm, 2)"]+vcovHC["l(x, 3):l(dmm, 3)","l(x, 3):l(dmm, 3)"]+vcovHC["l(x, 4):l(dmm, 4)","l(x, 4):l(dmm, 4)"]+vcovHC["l(x, 5):l(dmm, 5)","l(x, 5):l(dmm, 5)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+2*vcovHC["l(x, 3)","x:dmm"]+2*vcovHC["l(x, 4)","x:dmm"]+2*vcovHC["l(x, 5)","x:dmm"]+
    2*vcovHC["x","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 1)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 2)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 3)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 4)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 5)","l(x, 1):l(dmm, 1)"]+2*vcovHC["x:dmm","l(x, 1):l(dmm, 1)"]+
    2*vcovHC["x","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 2)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 3)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 4)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 5)","l(x, 2):l(dmm, 2)"]+2*vcovHC["x:dmm","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 2):l(dmm, 2)"]+
    2*vcovHC["x","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 3)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 4)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 5)","l(x, 3):l(dmm, 3)"]+2*vcovHC["x:dmm","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 3):l(dmm, 3)"]+
    2*vcovHC["x","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 1)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 2)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 3)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 4)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 5)","l(x, 4):l(dmm, 4)"]+2*vcovHC["x:dmm","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 4):l(dmm, 4)"]+
    2*vcovHC["x","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 1)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 2)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 3)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 4)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 5)","l(x, 5):l(dmm, 5)"]+2*vcovHC["x:dmm","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 5):l(dmm, 5)"]
  var71<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):l(dmm, 1)","l(x, 1):l(dmm, 1)"]+vcovHC["l(x, 2):l(dmm, 2)","l(x, 2):l(dmm, 2)"]+vcovHC["l(x, 3):l(dmm, 3)","l(x, 3):l(dmm, 3)"]+vcovHC["l(x, 4):l(dmm, 4)","l(x, 4):l(dmm, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):l(dmm, 5)","l(x, 5):l(dmm, 5)"]+vcovHC["l(x, 6):l(dmm, 6)","l(x, 6):l(dmm, 6)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+2*vcovHC["l(x, 3)","x:dmm"]+2*vcovHC["l(x, 4)","x:dmm"]+2*vcovHC["l(x, 5)","x:dmm"]+2*vcovHC["l(x, 6)","x:dmm"]+
    2*vcovHC["x","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 1)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 2)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 3)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 4)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 5)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 6)","l(x, 1):l(dmm, 1)"]+2*vcovHC["x:dmm","l(x, 1):l(dmm, 1)"]+
    2*vcovHC["x","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 2)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 3)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 4)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 5)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 6)","l(x, 2):l(dmm, 2)"]+2*vcovHC["x:dmm","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 2):l(dmm, 2)"]+
    2*vcovHC["x","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 3)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 4)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 5)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 6)","l(x, 3):l(dmm, 3)"]+2*vcovHC["x:dmm","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 3):l(dmm, 3)"]+
    2*vcovHC["x","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 1)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 2)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 3)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 4)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 5)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 6)","l(x, 4):l(dmm, 4)"]+2*vcovHC["x:dmm","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 4):l(dmm, 4)"]+
    2*vcovHC["x","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 1)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 2)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 3)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 4)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 5)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 6)","l(x, 5):l(dmm, 5)"]+2*vcovHC["x:dmm","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 5):l(dmm, 5)"]+
    2*vcovHC["x","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 1)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 2)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 3)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 4)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 5)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 6)","l(x, 6):l(dmm, 6)"]+2*vcovHC["x:dmm","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 6):l(dmm, 6)"]
  var81<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):l(dmm, 1)","l(x, 1):l(dmm, 1)"]+vcovHC["l(x, 2):l(dmm, 2)","l(x, 2):l(dmm, 2)"]+vcovHC["l(x, 3):l(dmm, 3)","l(x, 3):l(dmm, 3)"]+vcovHC["l(x, 4):l(dmm, 4)","l(x, 4):l(dmm, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):l(dmm, 5)","l(x, 5):l(dmm, 5)"]+vcovHC["l(x, 6):l(dmm, 6)","l(x, 6):l(dmm, 6)"]+vcovHC["l(x, 7):l(dmm, 7)","l(x, 7):l(dmm, 7)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+2*vcovHC["l(x, 3)","x:dmm"]+2*vcovHC["l(x, 4)","x:dmm"]+2*vcovHC["l(x, 5)","x:dmm"]+2*vcovHC["l(x, 6)","x:dmm"]+2*vcovHC["l(x, 7)","x:dmm"]+
    2*vcovHC["x","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 1)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 2)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 3)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 4)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 5)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 6)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 7)","l(x, 1):l(dmm, 1)"]+2*vcovHC["x:dmm","l(x, 1):l(dmm, 1)"]+
    2*vcovHC["x","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 2)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 3)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 4)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 5)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 6)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 7)","l(x, 2):l(dmm, 2)"]+2*vcovHC["x:dmm","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 2):l(dmm, 2)"]+
    2*vcovHC["x","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 3)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 4)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 5)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 6)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 7)","l(x, 3):l(dmm, 3)"]+2*vcovHC["x:dmm","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 3):l(dmm, 3)"]+
    2*vcovHC["x","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 1)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 2)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 3)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 4)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 5)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 6)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 7)","l(x, 4):l(dmm, 4)"]+2*vcovHC["x:dmm","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 4):l(dmm, 4)"]+
    2*vcovHC["x","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 1)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 2)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 3)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 4)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 5)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 6)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 7)","l(x, 5):l(dmm, 5)"]+2*vcovHC["x:dmm","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 5):l(dmm, 5)"]+
    2*vcovHC["x","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 1)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 2)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 3)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 4)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 5)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 6)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 7)","l(x, 6):l(dmm, 6)"]+2*vcovHC["x:dmm","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 6):l(dmm, 6)"]+
    2*vcovHC["x","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 1)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 2)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 3)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 4)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 5)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 6)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 7)","l(x, 7):l(dmm, 7)"]+2*vcovHC["x:dmm","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 6):l(dmm, 6)","l(x, 7):l(dmm, 7)"]
  var91<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):l(dmm, 1)","l(x, 1):l(dmm, 1)"]+vcovHC["l(x, 2):l(dmm, 2)","l(x, 2):l(dmm, 2)"]+vcovHC["l(x, 3):l(dmm, 3)","l(x, 3):l(dmm, 3)"]+vcovHC["l(x, 4):l(dmm, 4)","l(x, 4):l(dmm, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):l(dmm, 5)","l(x, 5):l(dmm, 5)"]+vcovHC["l(x, 6):l(dmm, 6)","l(x, 6):l(dmm, 6)"]+vcovHC["l(x, 7):l(dmm, 7)","l(x, 7):l(dmm, 7)"]+vcovHC["l(x, 8):l(dmm, 8)","l(x, 8):l(dmm, 8)"]+
    2*vcovHC["x","l(x, 1)"]+
    2*vcovHC["x","l(x, 2)"]+2*vcovHC["l(x, 1)","l(x, 2)"]+
    2*vcovHC["x","l(x, 3)"]+2*vcovHC["l(x, 1)","l(x, 3)"]+2*vcovHC["l(x, 2)","l(x, 3)"]+
    2*vcovHC["x","l(x, 4)"]+2*vcovHC["l(x, 1)","l(x, 4)"]+2*vcovHC["l(x, 2)","l(x, 4)"]+2*vcovHC["l(x, 3)","l(x, 4)"]+
    2*vcovHC["x","l(x, 5)"]+2*vcovHC["l(x, 1)","l(x, 5)"]+2*vcovHC["l(x, 2)","l(x, 5)"]+2*vcovHC["l(x, 3)","l(x, 5)"]+2*vcovHC["l(x, 4)","l(x, 5)"]+
    2*vcovHC["x","l(x, 6)"]+2*vcovHC["l(x, 1)","l(x, 6)"]+2*vcovHC["l(x, 2)","l(x, 6)"]+2*vcovHC["l(x, 3)","l(x, 6)"]+2*vcovHC["l(x, 4)","l(x, 6)"]+2*vcovHC["l(x, 5)","l(x, 6)"]+
    2*vcovHC["x","l(x, 7)"]+2*vcovHC["l(x, 1)","l(x, 7)"]+2*vcovHC["l(x, 2)","l(x, 7)"]+2*vcovHC["l(x, 3)","l(x, 7)"]+2*vcovHC["l(x, 4)","l(x, 7)"]+2*vcovHC["l(x, 5)","l(x, 7)"]+2*vcovHC["l(x, 6)","l(x, 7)"]+
    2*vcovHC["x","l(x, 8)"]+2*vcovHC["l(x, 1)","l(x, 8)"]+2*vcovHC["l(x, 2)","l(x, 8)"]+2*vcovHC["l(x, 3)","l(x, 8)"]+2*vcovHC["l(x, 4)","l(x, 8)"]+2*vcovHC["l(x, 5)","l(x, 8)"]+2*vcovHC["l(x, 6)","l(x, 8)"]+2*vcovHC["l(x, 7)","l(x, 8)"]+
    2*vcovHC["x","x:dmm"]+2*vcovHC["l(x, 1)","x:dmm"]+2*vcovHC["l(x, 2)","x:dmm"]+2*vcovHC["l(x, 3)","x:dmm"]+2*vcovHC["l(x, 4)","x:dmm"]+2*vcovHC["l(x, 5)","x:dmm"]+2*vcovHC["l(x, 6)","x:dmm"]+2*vcovHC["l(x, 7)","x:dmm"]+2*vcovHC["l(x, 8)","x:dmm"]+
    2*vcovHC["x","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 1)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 2)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 3)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 4)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 5)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 6)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 7)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 8)","l(x, 1):l(dmm, 1)"]+2*vcovHC["x:dmm","l(x, 1):l(dmm, 1)"]+
    2*vcovHC["x","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 2)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 3)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 4)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 5)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 6)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 7)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 8)","l(x, 2):l(dmm, 2)"]+2*vcovHC["x:dmm","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 2):l(dmm, 2)"]+
    2*vcovHC["x","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 3)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 4)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 5)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 6)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 7)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 8)","l(x, 3):l(dmm, 3)"]+2*vcovHC["x:dmm","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 3):l(dmm, 3)"]+
    2*vcovHC["x","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 1)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 2)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 3)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 4)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 5)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 6)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 7)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 8)","l(x, 4):l(dmm, 4)"]+2*vcovHC["x:dmm","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 4):l(dmm, 4)"]+
    2*vcovHC["x","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 1)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 2)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 3)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 4)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 5)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 6)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 7)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 8)","l(x, 5):l(dmm, 5)"]+2*vcovHC["x:dmm","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 5):l(dmm, 5)"]+
    2*vcovHC["x","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 1)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 2)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 3)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 4)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 5)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 6)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 7)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 8)","l(x, 6):l(dmm, 6)"]+2*vcovHC["x:dmm","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 6):l(dmm, 6)"]+
    2*vcovHC["x","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 1)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 2)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 3)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 4)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 5)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 6)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 7)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 8)","l(x, 7):l(dmm, 7)"]+2*vcovHC["x:dmm","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 6):l(dmm, 6)","l(x, 7):l(dmm, 7)"]+
    2*vcovHC["x","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 1)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 2)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 3)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 4)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 5)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 6)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 7)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 8)","l(x, 8):l(dmm, 8)"]+2*vcovHC["x:dmm","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 6):l(dmm, 6)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 7):l(dmm, 7)","l(x, 8):l(dmm, 8)"]
  var101<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):l(dmm, 1)","l(x, 1):l(dmm, 1)"]+vcovHC["l(x, 2):l(dmm, 2)","l(x, 2):l(dmm, 2)"]+vcovHC["l(x, 3):l(dmm, 3)","l(x, 3):l(dmm, 3)"]+vcovHC["l(x, 4):l(dmm, 4)","l(x, 4):l(dmm, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):l(dmm, 5)","l(x, 5):l(dmm, 5)"]+vcovHC["l(x, 6):l(dmm, 6)","l(x, 6):l(dmm, 6)"]+vcovHC["l(x, 7):l(dmm, 7)","l(x, 7):l(dmm, 7)"]+vcovHC["l(x, 8):l(dmm, 8)","l(x, 8):l(dmm, 8)"]+vcovHC["l(x, 9):l(dmm, 9)","l(x, 9):l(dmm, 9)"]+
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
    2*vcovHC["x","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 1)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 2)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 3)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 4)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 5)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 6)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 7)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 8)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 9)","l(x, 1):l(dmm, 1)"]+2*vcovHC["x:dmm","l(x, 1):l(dmm, 1)"]+
    2*vcovHC["x","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 2)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 3)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 4)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 5)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 6)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 7)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 8)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 9)","l(x, 2):l(dmm, 2)"]+2*vcovHC["x:dmm","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 2):l(dmm, 2)"]+
    2*vcovHC["x","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 3)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 4)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 5)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 6)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 7)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 8)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 9)","l(x, 3):l(dmm, 3)"]+2*vcovHC["x:dmm","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 3):l(dmm, 3)"]+
    2*vcovHC["x","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 1)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 2)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 3)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 4)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 5)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 6)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 7)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 8)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 9)","l(x, 4):l(dmm, 4)"]+2*vcovHC["x:dmm","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 4):l(dmm, 4)"]+
    2*vcovHC["x","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 1)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 2)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 3)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 4)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 5)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 6)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 7)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 8)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 9)","l(x, 5):l(dmm, 5)"]+2*vcovHC["x:dmm","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 5):l(dmm, 5)"]+
    2*vcovHC["x","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 1)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 2)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 3)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 4)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 5)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 6)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 7)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 8)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 9)","l(x, 6):l(dmm, 6)"]+2*vcovHC["x:dmm","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 6):l(dmm, 6)"]+
    2*vcovHC["x","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 1)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 2)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 3)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 4)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 5)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 6)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 7)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 8)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 9)","l(x, 7):l(dmm, 7)"]+2*vcovHC["x:dmm","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 6):l(dmm, 6)","l(x, 7):l(dmm, 7)"]+
    2*vcovHC["x","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 1)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 2)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 3)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 4)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 5)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 6)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 7)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 8)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 9)","l(x, 8):l(dmm, 8)"]+2*vcovHC["x:dmm","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 6):l(dmm, 6)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 7):l(dmm, 7)","l(x, 8):l(dmm, 8)"]+
    2*vcovHC["x","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 1)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 2)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 3)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 4)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 5)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 6)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 7)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 8)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 9)","l(x, 9):l(dmm, 9)"]+2*vcovHC["x:dmm","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 6):l(dmm, 6)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 7):l(dmm, 7)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 8):l(dmm, 8)","l(x, 9):l(dmm, 9)"]
  var111<-vcovHC["x","x"]+vcovHC["l(x, 1)","l(x, 1)"]+vcovHC["l(x, 2)","l(x, 2)"]+vcovHC["l(x, 3)","l(x, 3)"]+vcovHC["l(x, 4)","l(x, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 6)","l(x, 6)"]+vcovHC["l(x, 7)","l(x, 7)"]+vcovHC["l(x, 8)","l(x, 8)"]+vcovHC["l(x, 9)","l(x, 9)"]+vcovHC["l(x, 10)","l(x, 10)"]+vcovHC["x:dmm","x:dmm"]+vcovHC["l(x, 1):l(dmm, 1)","l(x, 1):l(dmm, 1)"]+vcovHC["l(x, 2):l(dmm, 2)","l(x, 2):l(dmm, 2)"]+vcovHC["l(x, 3):l(dmm, 3)","l(x, 3):l(dmm, 3)"]+vcovHC["l(x, 4):l(dmm, 4)","l(x, 4):l(dmm, 4)"]+vcovHC["l(x, 5)","l(x, 5)"]+vcovHC["l(x, 5):l(dmm, 5)","l(x, 5):l(dmm, 5)"]+vcovHC["l(x, 6):l(dmm, 6)","l(x, 6):l(dmm, 6)"]+vcovHC["l(x, 7):l(dmm, 7)","l(x, 7):l(dmm, 7)"]+vcovHC["l(x, 8):l(dmm, 8)","l(x, 8):l(dmm, 8)"]+vcovHC["l(x, 9):l(dmm, 9)","l(x, 9):l(dmm, 9)"]+vcovHC["l(x, 10):l(dmm, 10)","l(x, 10):l(dmm, 10)"]+
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
    2*vcovHC["x","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 1)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 2)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 3)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 4)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 5)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 6)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 7)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 8)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 9)","l(x, 1):l(dmm, 1)"]+2*vcovHC["l(x, 10)","l(x, 1):l(dmm, 1)"]+2*vcovHC["x:dmm","l(x, 1):l(dmm, 1)"]+
    2*vcovHC["x","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 2)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 3)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 4)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 5)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 6)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 7)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 8)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 9)","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 10)","l(x, 2):l(dmm, 2)"]+2*vcovHC["x:dmm","l(x, 2):l(dmm, 2)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 2):l(dmm, 2)"]+
    2*vcovHC["x","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 3)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 4)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 5)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 6)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 7)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 8)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 9)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 10)","l(x, 3):l(dmm, 3)"]+2*vcovHC["x:dmm","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 3):l(dmm, 3)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 3):l(dmm, 3)"]+
    2*vcovHC["x","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 1)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 2)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 3)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 4)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 5)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 6)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 7)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 8)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 9)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 10)","l(x, 4):l(dmm, 4)"]+2*vcovHC["x:dmm","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 4):l(dmm, 4)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 4):l(dmm, 4)"]+
    2*vcovHC["x","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 1)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 2)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 3)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 4)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 5)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 6)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 7)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 8)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 9)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 10)","l(x, 5):l(dmm, 5)"]+2*vcovHC["x:dmm","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 5):l(dmm, 5)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 5):l(dmm, 5)"]+
    2*vcovHC["x","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 1)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 2)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 3)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 4)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 5)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 6)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 7)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 8)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 9)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 10)","l(x, 6):l(dmm, 6)"]+2*vcovHC["x:dmm","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 6):l(dmm, 6)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 6):l(dmm, 6)"]+
    2*vcovHC["x","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 1)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 2)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 3)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 4)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 5)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 6)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 7)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 8)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 9)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 10)","l(x, 7):l(dmm, 7)"]+2*vcovHC["x:dmm","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 7):l(dmm, 7)"]+2*vcovHC["l(x, 6):l(dmm, 6)","l(x, 7):l(dmm, 7)"]+
    2*vcovHC["x","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 1)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 2)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 3)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 4)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 5)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 6)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 7)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 8)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 9)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 10)","l(x, 8):l(dmm, 8)"]+2*vcovHC["x:dmm","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 6):l(dmm, 6)","l(x, 8):l(dmm, 8)"]+2*vcovHC["l(x, 7):l(dmm, 7)","l(x, 8):l(dmm, 8)"]+
    2*vcovHC["x","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 1)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 2)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 3)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 4)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 5)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 6)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 7)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 8)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 9)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 10)","l(x, 9):l(dmm, 9)"]+2*vcovHC["x:dmm","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 6):l(dmm, 6)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 7):l(dmm, 7)","l(x, 9):l(dmm, 9)"]+2*vcovHC["l(x, 8):l(dmm, 8)","l(x, 9):l(dmm, 9)"]+
    2*vcovHC["x","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 1)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 2)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 3)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 4)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 5)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 6)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 7)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 8)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 9)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 10)","l(x, 10):l(dmm, 10)"]+2*vcovHC["x:dmm","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 1):l(dmm, 1)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 2):l(dmm, 2)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 3):l(dmm, 3)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 4):l(dmm, 4)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 5):l(dmm, 5)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 6):l(dmm, 6)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 7):l(dmm, 7)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 8):l(dmm, 8)","l(x, 10):l(dmm, 10)"]+2*vcovHC["l(x, 9):l(dmm, 9)","l(x, 10):l(dmm, 10)"]
  
  varsums1<-c(var11,var21,var31,var41,var51,var61,var71,var81,var91,var101,var111)
  sdsums1<-varsums1^0.5

  singlecoeff11<-mod$coefficients[c("x:dmm", "l(x, 1):l(dmm, 1)", "l(x, 2):l(dmm, 2)", "l(x, 3):l(dmm, 3)", "l(x, 4):l(dmm, 4)", "l(x, 5):l(dmm, 5)", "l(x, 6):l(dmm, 6)", "l(x, 7):l(dmm, 7)", "l(x, 8):l(dmm, 8)", "l(x, 9):l(dmm, 9)", "l(x, 10):l(dmm, 10)")]
  
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
