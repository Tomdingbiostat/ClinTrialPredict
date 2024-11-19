

#' Calculate the censoring rate for a two-arm clinical trial
#'
#' @inheritParams TrialPred.TwoArm
#'
#' @return This function returns a list containing all design parameters, including the calculated censoring rate `gamma.c`
#' @export
#'
#' @examples #calculate the censoring parameter
#' CensTime.TwoArm(N.0=100,N.1=100,d=10,l=3,alpha0.t=1,nu0.t=5,alpha1.t=2,nu1.t=4,s=5,m=4)
CensTime.TwoArm <- function(   N.0=NULL,
                               N.1=NULL,
                               d=NULL,
                               l=NULL,
                               alpha0.t=NULL,
                               nu0.t=NULL,
                               alpha1.t=NULL,
                               nu1.t=NULL,
                               s=NULL,
                               m=NULL,
                               design2=NULL
)
{
  if(!is.null(design2)){
    for (name in names(design2)) { assign(name, design2[[name]]) }
  }

  # Scenario 1: l<s, l<m
  if(l<=s & l<=m){
    d.0 <- N.0 * integral.s1(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=0.001) + N.1 * integral.s1(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=0.001)
    if(d> d.0){
      stop("Error: can not acheieve the expected number of events")
    } else {

      of <- function(gamma){
        int <- N.0 * integral.s1(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma) + N.1 * integral.s1(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma)
        abs(int - d)
      }
      gamma <- stats::optimize(of,interval=c(0,log(0.01)/(-min(m,l))))
    }
  }

  # Scenario 2:
  else if(l<=s & l>m){
    d.0 <- N.0*integral.s2(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=0.001) + N.1*integral.s2(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=0.001)
    if(d>d.0){
      stop("Error: can not acheieve the expected number of events")
    } else{

      of <- function(gamma){
        int <- N.0 * integral.s2(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma) + N.1*integral.s2(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma)
        abs(int - d)
      }
      gamma <- stats::optimize(of,interval=c(0,10*nu1.t))
    }
  }

  # Scenario 3:
  else if(l>s & l<=m){
    d.0 <- N.0*integral.s3(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=0.001) + N.1*integral.s3(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=0.001)
    if(d>d.0){
      stop("Error: can not acheieve the expected number of events")
    } else{

      of <- function(gamma){
        int <- N.0 * integral.s3(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma) + N.1*integral.s3(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma)
        abs(int - d)
      }
      gamma <- stats::optimize(of,interval=c(0,10*nu1.t))
    }
  }

  # Scenario 4:
  else if(l>s & l>m & l <s+m){
    d.0 <- N.0*integral.s4(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=0.001) + N.1*integral.s4(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=0.001)
    if(d>d.0){
      stop("Error: can not acheieve the expected number of events")
    } else{

      of <- function(gamma){
        int <- N.0 * integral.s4(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma) + N.1*integral.s4(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma)
        abs(int - d)
      }
      gamma <- stats::optimize(of,interval=c(0,10*nu1.t))
    }
  }

  # Scenario 5:
  else if(l>s & l>m & l>=s+m){
    #d.0 <- N.0 * integral2(f2.0.0,0,s,0,m)$Q + N.1 * integral2(f2.1.0,0,s,0,m)$Q
    d.0 <- N.0*integral.s5(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma) + N.1*integral.s5(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=0.001)
    if(d>d.0){
      stop("Error: can not acheieve the expected number of events")
    } else{
      of <- function(gamma){
        int <- N.0 * integral.s5(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma) + N.1*integral.s5(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma)
        abs(int - d)
      }
      gamma <- stats::optimize(of,interval=c(0,10*nu1.t))
    }
  }

  result <- list(
                   N.0 = N.0
                  ,N.1 = N.1
                  ,alpha0.t = alpha0.t
                  ,nu0.t    = nu0.t
                  ,alpha1.t = alpha1.t
                  ,nu1.t    = nu1.t
                  ,gamma.c = gamma$minimum
                  ,s=s
                  ,m=m
                  ,l=l
                  ,d=d
            )

  return(result)
}


#CensTimeTwoArm(N.0=100,N.1=100,d=23.87,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,s=5,m=4,l=6)



