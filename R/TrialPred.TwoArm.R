
#' Title
#'
#' @param N.0 s
#' @param N.1 s
#' @param ratio s
#' @param d s
#' @param l s
#' @param gamma.c s
#' @param alpha0.t s
#' @param nu0.t s
#' @param HR s
#' @param alpha1.t s
#' @param nu1.t s
#' @param s s
#' @param m s
#'
#' @return ssss
#' @export
#'
#' @examples sssss
TrialPred.TwoArm <- function(
                                N.0=NULL,       # Number of subjects plan to be enrolled in group 1
                                N.1=NULL,       # Number of subjects plan to be enrolled in group 2
                                ratio=NULL,     # N.1 / N.0
                                d=NULL,         # expected number of events observed at l
                                l=NULL,         # observation time
                                gamma.c=NULL,   # parameters of a weibull censoring distribution for both groups
                                alpha0.t=NULL,  # shape parameter of a weibull survival distribution for group 1
                                nu0.t=NULL,     # scale parameter of a weibull survival distribution for group 1
                                HR = NULL,      # hazard ratio of arm 1 / arm 2
                                alpha1.t=NULL,  # shape parameters of a weibull survival distribution for group 2
                                nu1.t=NULL,     # scale parameter of a weibull survival distribution for group 2
                                s=NULL,         # enrollment period time
                                m=NULL          # maximum follow-up time for a subject


){

  if(!is.null(HR)){
    alpha1.t <- alpha0.t
    nu1.t <- nu0.t * HR^(1/alpha0.t)
  }

  if(!is.null(ratio) & is.null(N.1)){
    N.1 <- N.0 * ratio
  }else if(!is.null(ratio) & is.null(N.0)){
    N.0 <- N.1 / ratio
  }

  design <- list(N.0=N.0,N.1=N.1,ratio=ratio,d=d,l=l,gamma.c=gamma.c,alpha0.t=alpha0.t,nu0.t=nu0.t,alpha1.t=alpha1.t,nu1.t=nu1.t,HR=HR,s=s,m=m)

  if( is.null(d) ){
    return(NumEventsSubTwoArm(N.0=N.0,N.1=N.1,alpha0.t = alpha0.t,nu0.t=nu0.t,alpha1.t=alpha1.t,nu1.t=nu1.t,gamma.c=gamma.c,s=s,m=m,l=l))
  }

  else if( is.null(N.0) & is.null(N.1) ){
    return(NumEventsSubTwoArm(ratio=ratio,d=d,alpha0.t = alpha0.t,nu0.t=nu0.t,alpha1.t=alpha1.t,nu1.t=nu1.t,gamma.c=gamma.c,s=s,m=m,l=l))
  }

  else if( is.null(l) ){
    return(ObsTimeTwoArm(N.0=N.0,N.1=N.1,d=d,alpha0.t = alpha0.t,nu0.t=nu0.t,alpha1.t=alpha1.t,nu1.t=nu1.t,gamma.c=gamma.c,s=s,m=m))
  }

  else if( is.null(gamma.c) ){
    return(CensTimeTwoArm(N.0=N.0,N.1=N.1,d=d,alpha0.t = alpha0.t,nu0.t=nu0.t,alpha1.t=alpha1.t,nu1.t=nu1.t,s=s,m=m,l=l))
  }
}


