

#' Calculate the censoring rate for a one-arm design
#'
#' @inheritParams TrialPred.OneArm
#'
#' @return This function returns a list containing all design parameters, including the calculated censoring rate `gamma`.
#' @export
#'
#' @examples CensRate.OneArm(N=100,d=10,l=10,s=12,m=6,alpha=1,nu=20)
CensRate.OneArm <- function( N = NULL
                            ,d = NULL
                            ,s = NULL
                            ,m = NULL
                            ,l = NULL
                            ,alpha = NULL
                            ,nu = NULL
                            ){

  P.delta.0 <- d/N

  # Scenario 1
  if(l<=s & l<=m){
    P.delta.0.0 <- integral.s1(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=0.001)

    if(P.delta.0 > P.delta.0.0){
      stop("Error: can not acheieve the expected number of events")
    } else{

      of <- function(gamma){
        int <- integral.s1(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
        abs(int - P.delta.0)
      }
      gamma <- stats::optimize(of,interval = c(0,nu*10))
    }
  }

  # Scenario 2
  else if(l<=s & l>m){
    P.delta.0.0 <- integral.s2(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=0.001)
    if(P.delta.0 > P.delta.0.0){
      stop("Error: can not acheieve the expected number of events")
    } else{
      of <- function(gamma){
        int <- integral.s2(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
        abs(int - P.delta.0)
      }
      gamma <- stats::optimize(of,interval = c(0,10*nu))
    }
  }

  # Scenario 3
  else if(l>s & l<=m){
    P.delta.0.0 <- integral.s3(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=0.001)
    if(P.delta.0 > P.delta.0.0){
      stop("Error: can not acheieve the expected number of events")
    } else{
      of <- function(gamma){
        int <- integral.s3(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
        abs(int - P.delta.0)
      }
      gamma <- stats::optimize(of,interval = c(0,10*nu))
    }
  }

  # Scenario 4
  else if(l>s & l>m & l <s+m){
    P.delta.0.0 <- integral.s4(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=0.001)
    if(P.delta.0 > P.delta.0.0){
      stop("Error: can not acheieve the expected number of events")
    } else{
      of <- function(gamma){
        int <- integral.s4(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
        abs(int - P.delta.0)
      }
      gamma <- stats::optimize(of,interval = c(0,10*nu))
    }
  }

  # Scenario 5
  else if(l>s & l>m & l>=s+m){
    P.delta.0.0 <- integral.s5(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=0.001)
    if(P.delta.0 > P.delta.0.0){
      stop("Error: can not acheieve the expected number of events")
    } else{
      of <- function(gamma){
        int <- integral.s5(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
        abs(int - P.delta.0)
      }
      gamma <- stats::optimize(of,interval = c(0,10*nu))
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

