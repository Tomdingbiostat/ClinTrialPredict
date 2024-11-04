
library(ggplot2)
library(tidyverse)
library(pracma)

files <- list.files("./R", pattern = "\\.R$", full.names = TRUE)
lapply(files, source)

meanD <- function(N.0,N.1,alpha0.t,nu0.t,HR,gamma.c,s,m,l){
  design <- TrialPred.TwoArm(N.0=N.0,N.1=N.1,alpha0.t = alpha0.t,nu0.t=nu0.t,HR=HR,gamma.c=gamma.c,s=s,m=m,l=l)
  ds <- SimDataTwoArm(design = design,seed=1234,nsim=100)
  ds %>% arrange(sim,arm) %>%  group_by(sim) %>% summarise(sumD=sum(event)) %>% ungroup() %>% summarise(meanD = mean(sumD)) %>% {.[["meanD"]]}
}


meanD(N.0=200,N.1=200,alpha0.t = 1,nu0.t=20,HR=0.8,gamma.c=0.1,s=12,m=6,l=10)



#design <- TrialPred.TwoArm(N.0=200,N.1=200,alpha0.t = 1,nu0.t=20,HR=0.8,gamma.c=0.1,s=12,m=6,l=10)
#ds %>% arrange(sim,arm) %>%  group_by(sim) %>% summarise(sumD=sum(event)) %>% ungroup() %>% summarise(meanD = mean(sumD)) %>% {.[["meanD"]]}
