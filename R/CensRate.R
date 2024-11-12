

CensRate <- function(N,d,s,m,l,alpha,nu){

  P.delta.0 <- d/N
  #print(paste("P.delta.0:",P.delta.0))

  # f2.0 <- function(a,t){
  #   1/s * dweibull(t,shape=alpha,scale = nu) * (1-pexp(t,rate=0.001))
  # }

  # Scenario 1
  if(l<=s & l<=m){
    #P.delta.0.0 <-  integral2(f2.0, 0, l, 0, function(x) l-x)$Q
    P.delta.0.0 <- integral.s1(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=0.001)

    if(P.delta.0 > P.delta.0.0){
      stop("Error: can not acheieve the expected number of events")
    } else{
      # of <- function(gamma){
      #   f2.1 <- function(a,t){
      #     1/s * dweibull(t,shape=alpha,scale = nu) * (1-pexp(t,rate=gamma))
      #   }
      #   int <- integral2(f2.1, 0, l, 0, function(x) l-x)$Q
      #   abs(int - P.delta.0)
      # }
      of <- function(gamma){
        int <- integral.s1(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
        abs(int - P.delta.0)
      }
      gamma <- optimize(of,interval = c(0,nu*10))
    }
  }

  # Scenario 2
  else if(l<=s & l>m){
    #P.delta.0.0 <- integral2(f2.0,0,l-m,0,m)$Q + integral2(f2.0,l-m,l,0,function(x) l-x)$Q
    P.delta.0.0 <- integral.s2(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=0.001)
    if(P.delta.0 > P.delta.0.0){
      stop("Error: can not acheieve the expected number of events")
    } else{
      # of <- function(gamma){
      #   f2.1 <- function(a,t){
      #     1/s * dweibull(t,shape=alpha,scale = nu) * (1-pexp(t,rate=gamma))
      #   }
      #   int <- integral2(f2.1,0,l-m,0,m)$Q + integral2(f2.1,l-m,l,0,function(x) l-x)$Q
      #   abs(int - P.delta.0)
      # }
      of <- function(gamma){
        int <- integral.s2(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
        abs(int - P.delta.0)
      }
      gamma <- optimize(of,interval = c(0,10*nu))
    }
  }

  # Scenario 3
  else if(l>s & l<=m){
    #P.delta.0.0 <-  integral2(f2.0, 0, s, 0, function(x) l-x)$Q
    P.delta.0.0 <- integral.s3(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=0.001)
    if(P.delta.0 > P.delta.0.0){
      stop("Error: can not acheieve the expected number of events")
    } else{
      # of <- function(gamma){
      #   f2.1 <- function(a,t){
      #     1/s * dweibull(t,shape=alpha,scale = nu) * (1-pexp(t,rate=gamma))
      #   }
      #   int <- integral2(f2.1, 0, s, 0, function(x) l-x)$Q
      #   abs(int - P.delta.0)
      # }
      of <- function(gamma){
        int <- integral.s3(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
        abs(int - P.delta.0)
      }
      gamma <- optimize(of,interval = c(0,10*nu))
    }
  }

  # Scenario 4
  else if(l>s & l>m & l <s+m){
    #P.delta.0.0 <-  integral2(f2.0,0   ,l-m ,0 ,m)$Q + integral2(f2.0,l-m ,s   ,0 ,function(x) l-x)$Q
    P.delta.0.0 <- integral.s4(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=0.001)
    if(P.delta.0 > P.delta.0.0){
      stop("Error: can not acheieve the expected number of events")
    } else{
      # of <- function(gamma){
      #   f2.1 <- function(a,t){
      #     1/s * dweibull(t,shape=alpha,scale = nu) * (1-pexp(t,rate=gamma))
      #   }
      #   int <- integral2(f2.1,0   ,l-m ,0 ,m)$Q + integral2(f2.1,l-m ,s   ,0 ,function(x) l-x)$Q
      #   abs(int - P.delta.0)
      # }
      of <- function(gamma){
        int <- integral.s4(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
        abs(int - P.delta.0)
      }
      gamma <- optimize(of,interval = c(0,10*nu))
    }
  }

  # Scenario 5
  else if(l>s & l>m & l>=s+m){
    #P.delta.0.0 <-  integral2(f2.0,0   ,s ,0 ,m)$Q
    P.delta.0.0 <- integral.s5(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=0.001)
    if(P.delta.0 > P.delta.0.0){
      stop("Error: can not acheieve the expected number of events")
    } else{
      # of <- function(gamma){
      #   f2.1 <- function(a,t){
      #     1/s * dweibull(t,shape=alpha,scale = nu) * (1-pexp(t,rate=gamma))
      #   }
      #   int <- integral2(f2.1,0   ,s ,0 ,m)$Q
      #   abs(int - P.delta.0)
      # }
      of <- function(gamma){
        int <- integral.s5(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
        abs(int - P.delta.0)
      }
      gamma <- optimize(of,interval = c(0,10*nu))
      return(gamma)
    }
  }

  res <- list(
    P.delta.0=P.delta.0
    ,d=d
    ,N=N
    ,s=s
    ,m=m
    ,l=l
    ,alpha=alpha
    ,nu=nu
    ,gamma=gamma$minimum
  )

  return(res)
}

