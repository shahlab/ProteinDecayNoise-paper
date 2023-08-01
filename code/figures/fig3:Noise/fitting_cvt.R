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

#Split the dataframe based on the degron 
list.cvt.opt <- cv.df %>% 
  left_join(.,cv.1, by = "degron") %>% 
  split(.$degron)


#Run the optimization 
#without cor
cvt.fits <- lapply(c(1,2), function(a){ 
  
  df <- list.cvt.opt[[a]]
  
  par.optim <- c(0.2 #cv.gfp at t=0
                 # 0.1
  ) #cvd
  
  names(par.optim) <- c("gfp.cv0")
  
  optimx(par = par.optim, 
         fn = cvt.opt.fn, 
         method = "L-BFGS-B", 
         lower = c(0.1), 
         upper = c(0.6),
         df1 = df,
         itnmax = 100000)
})


#Making dataframes
#without cor
cvt.fits.df <- cvt.fits %>% bind_rows(.id = "degron") %>% 
  mutate(degron = case_when(degron == "1" ~ "yeGFP-mODC", 
                            degron == "2" ~ "yeGFP-CLN2")) 


