
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
integral.s1 <- function(s,m,l,alpha,nu,gamma){
  pracma::integral2( function(a,t) 1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma)),0,l,0,function(x) l-x)$Q
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
integral.s2 <- function(s,m,l,alpha,nu,gamma){
  if(l != m){
    pracma::integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))},0,l-m,0,m)$Q +
      pracma::integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))},l-m,l,0,function(x) l-x)$Q
  }else{
    pracma::integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))},l-m,l,0,function(x) l-x)$Q
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
integral.s3 <- function(s,m,l,alpha,nu,gamma){
  pracma::integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))}, 0, s, 0, function(x) l-x)$Q
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
integral.s4 <- function(s,m,l,alpha,nu,gamma){
  pracma::integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))}, 0, l-m, 0, m)$Q +
  pracma::integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))}, l-m, s, 0, function(x) l-x)$Q
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
integral.s5 <- function(s,m,l,alpha,nu,gamma){
  pracma::integral2(function(a,t){1/s * dweibull(t,shape=alpha,scale=nu) * (1-pexp(t,rate=gamma))},0,s,0 ,m,reltol = 1e-10)$Q
}









