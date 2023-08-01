library(optimx)


#fitting the data to the mechanistic change in CV for two scenariors: 
#1. witthout considering correlations between gfp int and decay rates

cvt.opt.fn<- function(df1, par){ 
  #fitting for the CV in gfp at t = 0 
  cv0.a <- par[1] #gfp cv0
  # cvd.b <- par[2] #cv decay rate
  
  mech.cv.df <- df1 %>% 
    mutate(mech.cvt2 = noise.cvt.fn(cv0 = cv0.a, 
                                    cvd = cv.d,
                                    md = mean.d,
                                    t = time.min), 
           cvt2.err = (norm.cv2 - mech.cvt2)^2) %>% 
    summarise(sum.err = sum(cvt2.err))
  
  return(mech.cv.df$sum.err)
}

#2. With correlations between gfp int. and decay rates

cvt.cor.opt.fn <- function(df1, par){ 
  #fitting for CV in gfp att t = 0 and corr between gfp and decay rates
  gfp.cor.a <- par[1] #should be negative
  cv0.b <- par[2]
  # cvd.c <- par[3]
  
  mech.cv.df <- df1 %>% 
    mutate(`cor.cvd` = noise.cvt.cor.fn(cv0 = cv0.b,
                                        cvd = cv.d,
                                        md = mean.d,
                                        t = time.min,
                                        gfp.dy.cor = gfp.cor.a), 
           cvt2.err = (norm.cv2 - cor.cvd)^2) %>% 
    summarise(sum.err = sum(cvt2.err))
  
  return(mech.cv.df$sum.err)
}

