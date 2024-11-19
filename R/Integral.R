

fun <- function(a,t,alpha,nu,gamma,s=s){
  1/s * stats::dweibull(t,shape=alpha,scale=nu) * (1-stats::pexp(t,rate=gamma))
}

integral.s1 <- function(s,m,l,alpha,nu,gamma){
  stats::integrate(Vectorize( function(a){ stats::integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,rel.tol=1e-10,subdivisions=1000)$value } ),0,l,rel.tol=1e-10)$value
}

integral.s2 <- function(s,m,l,alpha,nu,gamma){
  if(l != m){
    stats::integrate(Vectorize( function(a){ stats::integrate(fun,0,m,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,  rel.tol=1e-10)$value } ),0,l-m,rel.tol=1e-10)$value +
      stats::integrate(Vectorize( function(a){ stats::integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,rel.tol=1e-10)$value } ),l-m,l,rel.tol=1e-10)$value
  }else{
    stats::integrate(Vectorize( function(a){ stats::integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,rel.tol=1e-10)$value } ),l-m,l,rel.tol=1e-10)$value
  }
}

integral.s3 <- function(s,m,l,alpha,nu,gamma){
  stats::integrate(Vectorize( function(a){ stats::integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,rel.tol=1e-10)$value } ),0,s,rel.tol=1e-10)$value
}


integral.s4 <- function(s,m,l,alpha,nu,gamma){
  stats::integrate(Vectorize( function(a){ stats::integrate(fun,0,m,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,rel.tol=1e-10)$value } ),0,l-m,rel.tol=1e-10)$value +
  stats::integrate(Vectorize( function(a){ stats::integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,rel.tol=1e-10)$value } ),l-m,s,rel.tol=1e-10)$value
}

integral.s5 <- function(s,m,l,alpha,nu,gamma){
  stats::integrate(Vectorize( function(a){ stats::integrate(fun,0,m,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,rel.tol=1e-10)$value } ),0,s,rel.tol=1e-10)$value
}









