
files <- list.files("./R", pattern = "\\.R$", full.names = TRUE)
lapply(files, source)


library(tidyverse)


designs <- tibble(
  N.0 = rep(200,6)
  ,N.1 = rep(200,6)
  ,m = c(rep(6,3),rep(18,3))
  ,s = rep(12,6)
  ,alpha0.t = c(rep(0.8,3),rep(1.2,3))
  ,nu0.t = rep(20,6)
  ,HR = c(rep(0.8,3),rep(1.2,3))
  ,l = c(3,8,15,5,15,20)
  ,d = c(c(10,50,60),c(8,40,70))
)


#res <- designs[5,] %>% rowwise() %>% mutate( gamma.c = round(TrialPred.TwoArm(N.0=N.0,N.1=N.1,alpha0.t = alpha0.t,nu0.t=nu0.t,HR=HR,l=l,s=s,m=m,d=d)$gamma.c,2))
TrialPred.TwoArm(N.0=200,N.1=200,alpha0.t = 1.2,nu0.t=20,HR=1.2,l=20,s=12,m=18,d=70)
