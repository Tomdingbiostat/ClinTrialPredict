
#integration functions for each scenarios

integral.s1 <- function(s,m,l,alpha,nu,gamma){
  integral2( function(a,t) 1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma)),0,l,0,function(x) l-x)$Q
}

integral.s2 <- function(s,m,l,alpha,nu,gamma){
  if(l != m){
    integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))},0,l-m,0,m)$Q +
    integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))},l-m,l,0,function(x) l-x)$Q
  }else{
    integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))},l-m,l,0,function(x) l-x)$Q
  }
}

integral.s3 <- function(s,m,l,alpha,nu,gamma){
  integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))}, 0, s, 0, function(x) l-x)$Q
}

integral.s4 <- function(s,m,l,alpha,nu,gamma){
  integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))}, 0, l-m, 0, m)$Q +
  integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))}, l-m, s, 0, function(x) l-x)$Q
}

integral.s5 <- function(s,m,l,alpha,nu,gamma){
  integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))},0,s,0 ,m,reltol = 1e-10)$Q
}









