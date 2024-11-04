

library(ggplot2)
library(tidyverse)
library(pracma)

source("./R/Integral.R")
source("./R/NumEventsSub.R")

NumEventsSub(N=200,s=24,m=12,alpha=1,nu=50,gamma=0.3,l=29)

l <- seq(0.1,50,by=0.1)

ds <- expand_grid(l) %>% rowwise() %>% mutate(y=NumEventsSub(N=200,s=24,m=12,alpha=1,nu=50,gamma=0.003,l=l)$P.delta.0)

ggplot(ds,aes(l,y))+
  geom_point() +
  scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(limits = c(0,50))


