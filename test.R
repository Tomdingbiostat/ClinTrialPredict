



NumEventsSubTwoArm(ratio=1,d=10,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,gamma.c=1,s=5,m=4,l=6)


# TrialPred.TwoArm(ratio=1,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,gamma.c=1,s=5,m=4,l=6, d=23.87)
# TrialPred.TwoArm(N.0=100,N.1=100,d=23.87,ratio=1,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,gamma.c=1,s=5,m=4)
# TrialPred.TwoArm(N.0=100,N.1=100,d=23.87,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,s=5,m=4,l=6)


ObsTime(N=100,d=4.14, s=5,m=3,alpha=1,nu=5,gamma=1)
ObsTime(N=100,d=10.57,s=5,m=3,alpha=1,nu=5,gamma=1)
ObsTime(N=100,d=15.83,s=5,m=7,alpha=1,nu=5,gamma=1)
ObsTime(N=100,d=16.59,s=5,m=7,alpha=1,nu=5,gamma=1)
ObsTime(N=100,d=16.6629188,s=5,m=7,alpha=1,nu=5,gamma=1)

NumEventsSub(N=100000,s=5,m=3,l=9,alpha=1,nu=5,gamma=1)

sim2 <- SimData(list=s2,seed=1234)
mean(sim2$ds$event)



library(ggplot2)
library(tidyverse)


NumEventsSub(N=200,s=24,m=12,alpha=1,nu=20,gamma=0.0001,l=12)

l <- seq(0.1,50,by=0.1)

ds <- expand_grid(l) %>% rowwise() %>% mutate(y=NumEventsSub(N=200,s=24,m=12,alpha=1,nu=20,gamma=0.0001,l=l)$P.delta.0)

y <- unlist( sapply(x, NumEventsSub, N=200,s=24,m=12,alpha=1,nu=20,gamma=0.0001)[1,] )

ds <- tibble(
   x=x
  ,y=y
)

ggplot(ds)+
  geom_point(x=x,y=y) +
  scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(limits = c(0,50))





NumEventsSubTwoArm(N.1=100,N.2=100,alpha1.t = 1,nu1.t=5,alpha2.t=2,nu2.t=4,gamma.c=1,s=5,m=4,l=6)
ObsTimeTwoArm(N.0=100,N.1=100,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,gamma.c=1,s=5,m=4,d=30)








CensRate(N=100,d=5,s=5,m=3,l=2, alpha=1,nu=5)
NumEventsSub(N=100,s=5,m=3,l=2, alpha=1,nu=5,gamma=0.6047)

CensRate(N=100,d=10.54,s=5,m=3,l=4, alpha=1,nu=5)
NumEventsSub(N=100,s=5,m=3,l=4,  alpha=1,nu=5,gamma=1)

CensRate(N=100,s=5,m=7,l=6,  alpha=1,nu=5,d=15.83)
NumEventsObs(N=100,s=5,m=7,l=6,  alpha=1,nu=5,gamma=1)

CensRate(N=100,s=5,m=7,l=9,  alpha=1,nu=5,d=16)
NumEventsObs(N=100,s=5,m=7,l=8,  alpha=1,nu=5,gamma=1)

CensRate(N=100,s=5,m=7,l=14,  alpha=1,nu=5,d=16.6)
NumEventsObs(N=100,s=5,m=7,l=14,  alpha=1,nu=5,gamma=1)




