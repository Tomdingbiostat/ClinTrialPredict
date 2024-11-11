

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
#' @export
#'
#' @examples
# integral.s1 <- function(s,m,l,alpha,nu,gamma){
#   pracma::integral2( function(a,t) 1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma)),0,l,0,function(x) l-x)$Q
# }
integral.s1 <- function(s,m,l,alpha,nu,gamma){
  integrate(Vectorize( function(a){ integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s)$value } ),0,l)
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
#' @export
#'
#' @examples
# integral.s2 <- function(s,m,l,alpha,nu,gamma){
#   if(l != m){
#     pracma::integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))},0,l-m,0,m)$Q +
#       pracma::integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))},l-m,l,0,function(x) l-x)$Q
#   }else{
#     pracma::integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))},l-m,l,0,function(x) l-x)$Q
#   }
# }


integral.s2 <- function(s,m,l,alpha,nu,gamma){
  if(l != m){
    integrate(Vectorize( function(a){ integrate(fun,0,m,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s)$value } ),0,l-m) +
    integrate(Vectorize( function(a){ integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s)$value } ),l-m,l)
  }else{
    integrate(Vectorize( function(a){ integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s)$value } ),l-m,l)
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
#' @export
#'
#' @examples
# integral.s3 <- function(s,m,l,alpha,nu,gamma){
#   pracma::integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))}, 0, s, 0, function(x) l-x)$Q
# }

integral.s3 <- function(s,m,l,alpha,nu,gamma){
  integrate(Vectorize( function(a){ integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s)$value } ),0,s)
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
#' @export
#'
#' @examples
# integral.s4 <- function(s,m,l,alpha,nu,gamma){
#   pracma::integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))}, 0, l-m, 0, m)$Q +
#   pracma::integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))}, l-m, s, 0, function(x) l-x)$Q
# }

integral.s4 <- function(s,m,l,alpha,nu,gamma){
  integrate(Vectorize( function(a){ integrate(fun,0,m,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s)$value } ),0,l-m) +
  integrate(Vectorize( function(a){ integrate(fun,0,l-a,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s)$value } ),l-m,s)
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
#' @export
#'
#' @examples
# integral.s5 <- function(s,m,l,alpha,nu,gamma){
#   pracma::integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))},0,s,0 ,m,reltol = 1e-10)$Q
# }

integral.s5 <- function(s,m,l,alpha,nu,gamma){
  integrate(Vectorize( function(a){ integrate(fun,0,m,a=a,alpha=alpha,nu=nu,gamma=gamma,s=s)$value } ),0,s)
}









