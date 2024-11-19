

#' Calculate the observation time for a one-arm clinical trial
#'
#' @inheritParams TrialPred.OneArm
#'
#' @return This function returns a list containing all design parameters, including the calculated observation time `l`.
#' @export
#'
#' @examples ObsTime.OneArm(N=100,d=10,gamma=0.1,s=12,m=6,alpha=1,nu=20)
ObsTime.OneArm <- function(N,d,s,m,alpha,nu,gamma){

  P.delta.0 <- d/N

  P.delta.0.s <- NumEventsSub.OneArm(N=N,s=s,m=m,l=s,alpha=alpha,nu=nu,gamma=gamma)$P.delta.0

  P.delta.0.m <- NumEventsSub.OneArm(N=N,s=s,m=m,l=m,alpha=alpha,nu=nu,gamma=gamma)$P.delta.0

  P.delta.0.sm <- NumEventsSub.OneArm(N=N,s=s,m=m,l=m+s,alpha=alpha,nu=nu,gamma=gamma)$P.delta.0


  # Scenario 1
  if(P.delta.0<P.delta.0.m & P.delta.0<P.delta.0.s){

    of <- function(l){
       int <- integral.s1(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
       abs(int - P.delta.0)
    }
    l <- stats::optimize(of,interval=c(0,s))
  }

  # Scenario 2
  else if(m < s & P.delta.0 < P.delta.0.s & P.delta.0 > P.delta.0.m){
    of <- function(l){
      int <- integral.s2(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
    }
     l <- stats::optimize(of,interval = c(0,s))
  }

  # Scenario 3

  else if(m >s & P.delta.0 > P.delta.0.s & P.delta.0 < P.delta.0.m){
    of <- function(l){
      int <- integral.s3(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
    }
    l <- stats::optimize(of,interval = c(0,m))
  }

  # Scenario 4

  else if(P.delta.0 > P.delta.0.m & P.delta.0 > P.delta.0.s & P.delta.0 <= P.delta.0.sm){
    of <- function(l){
      int <- integral.s4(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
    }
    l <- stats::optimize(of,interval = c(0,m+s))
  }

  # Scenario 5

  else if(P.delta.0 > P.delta.0.sm){
    stop("Error: can not acheieve the expected number of events")
  }

  res <- list(
    P.delta.0=P.delta.0
    ,d=d
    ,N=N
    ,s=s
    ,m=m
    ,l=l$minimum
    ,alpha=alpha
    ,nu=nu
    ,gamma=gamma
  )

  return(res)

}




