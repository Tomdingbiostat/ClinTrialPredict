
#### predict number of events for a single arm ####

# N: number of subjects enrolled
# s: enrollment period
# m: maximum follow-up for a single subject
# alpha: shape parameter of a weibull distribution for survival time
# nu: scale parameter of a weibull distribution for survival time
# gamma: rate of an exponential distribution for random censoring time
# d: number of expected events observed


NumEventsSub <- function(N,s,m,l,alpha,nu,gamma,d){


  # Scenario 1
  #
  if(l<=s & l<=m){
    P.delta.0 <- integral.s1(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
  }

  # Scenario 2
  else if(l<=s & l>m){
    P.delta.0 <- integral.s2(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)

  }

  # Scenario 3
  else if(l>s & l<=m){
    P.delta.0 <-  integral.s3(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
  }

  # Scenario 4
  else if(l>s & l>m & l <s+m){
    P.delta.0 <-  integral.s4(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
  }

  # Scenario 5
  else if(l>s & l>m & l>=s+m){
    P.delta.0 <-  integral.s5(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
  }
  else{
    stop('wrong input')
  }


  res <- list(
               P.delta.0=P.delta.0
              ,d=ifelse(missing(d),N * P.delta.0,d)
              ,N=ifelse(missing(N),d / P.delta.0,N)
              ,s=s
              ,m=m
              ,l=l
              ,alpha=alpha
              ,nu=nu
              ,gamma=gamma
              ,Narm = 1
              )

  return(res)

}









