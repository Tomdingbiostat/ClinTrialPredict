

#' Title
#'
#' @param a
#' @param t
#' @param alpha
#' @param nu
#' @param gamma
#' @param s
#'
#' @return
#'
#' @examples
fun <- function(a,t,alpha,nu,gamma,s=s){
  1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))
}

#' Title
#'
#' @param s
#' @param m
#' @param l
#' @param alpha
#' @param nu
#' @param gamma
#'
#' @return
#'
#' @examples

integral.s1 <- function(s,m,l,alpha,nu,gamma){
  integrate(Vectorize( function(a){ integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,rel.tol=1e-10,subdivisions=1000)$value } ),0,l,rel.tol=1e-10)$value
}
#integral.s1(s=12,m=6,l=3,alpha=0.1,nu=20,gamma=0.1)





#' Title
#'
#' @param s
#' @param m
#' @param l
#' @param alpha
#' @param nu
#' @param gamma
#'
#' @return
#'
#' @examples

integral.s2 <- function(s,m,l,alpha,nu,gamma){
  if(l != m){
    integrate(Vectorize( function(a){ integrate(fun,0,m,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,  rel.tol=1e-10)$value } ),0,l-m,rel.tol=1e-10)$value +
    integrate(Vectorize( function(a){ integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,rel.tol=1e-10)$value } ),l-m,l,rel.tol=1e-10)$value
  }else{
    integrate(Vectorize( function(a){ integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,rel.tol=1e-10)$value } ),l-m,l,rel.tol=1e-10)$value
  }
}



#' Title
#'
#' @param s
#' @param m
#' @param l
#' @param alpha
#' @param nu
#' @param gamma
#'
#' @return
#'
#' @examples

integral.s3 <- function(s,m,l,alpha,nu,gamma){
  integrate(Vectorize( function(a){ integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,rel.tol=1e-10)$value } ),0,s,rel.tol=1e-10)$value
}

#' Title
#'
#' @param s
#' @param m
#' @param l
#' @param alpha
#' @param nu
#' @param gamma
#'
#' @return
#'
#' @examples

integral.s4 <- function(s,m,l,alpha,nu,gamma){
  integrate(Vectorize( function(a){ integrate(fun,0,m,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,rel.tol=1e-10)$value } ),0,l-m,rel.tol=1e-10)$value +
  integrate(Vectorize( function(a){ integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,rel.tol=1e-10)$value } ),l-m,s,rel.tol=1e-10)$value
}

#' Title
#'
#' @param s
#' @param m
#' @param l
#' @param alpha
#' @param nu
#' @param gamma
#'
#' @return
#'
#' @examples

integral.s5 <- function(s,m,l,alpha,nu,gamma){
  integrate(Vectorize( function(a){ integrate(fun,0,m,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s,rel.tol=1e-10)$value } ),0,s,rel.tol=1e-10)$value
}









