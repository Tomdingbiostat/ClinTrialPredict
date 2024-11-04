library(tidyverse)
library(ggplot2)
source("./R/TrialPred.TwoArm.R")
source("./R/NumEventsSubTwoArm.R")

N.0 <- 200
N.1 <- 200
m <- 6
s <- 12
alpha0.t <- c(1)
nu0.t <- 20
HR <- c(0.8,1,1.5)
gamma.c <- c(0.1,0.2,0.3)
l <- c(3,9,15,20)

designs <- expand_grid(N.0,N.1,m,s,alpha0.t,nu0.t,HR=HR,gamma.c,l)

meanD <- function(N.0,N.1,alpha0.t,nu0.t,HR,gamma.c,s,m,l){
  design <- TrialPred.TwoArm(N.0=N.0,N.1=N.1,alpha0.t = alpha0.t,nu0.t=nu0.t,HR=HR,gamma.c=gamma.c,s=s,m=m,l=l)
  ds <- SimDataTwoArm(design = design,seed=1234,nsim=10000)
  ds %>% arrange(sim,arm) %>%  group_by(sim) %>% summarise(sumD=sum(event)) %>% ungroup() %>% summarise(meanD = mean(sumD)) %>% {.[["meanD"]]}
}

res <- designs %>% rowwise() %>% mutate( ED = TrialPred.TwoArm(N.0=N.0,N.1=N.1,alpha0.t = alpha0.t,nu0.t=nu0.t,HR=HR,gamma.c=gamma.c,s=s,m=m,l=l)$d
                                         ,meanD = meanD(N.0=N.0,N.1=N.1,alpha0.t = alpha0.t,nu0.t=nu0.t,HR=HR,gamma.c=gamma.c,s=s,m=m,l=l))



# ggplot(data=res,aes(x=l,y=ED,colour=HR)) +
#   geom_point()
