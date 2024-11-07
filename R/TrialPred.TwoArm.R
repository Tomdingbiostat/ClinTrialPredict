
#' Title
#'
#' @param N.0 number of subjects plan to be enrolled in control arm
#' @param N.1 number of subjects plan to be enrolled in experimental arm
#' @param ratio randomization ratio between two arms: N.1 / N.0
#' @param d expected number of events observed at time l
#' @param l observation time
#' @param gamma.c parameter of the exponential distribution of censoring time
#' @param alpha0.t shape parameter of weibull survival distribution for control arm
#' @param nu0.t scale parameter of weibull survival distribution for control arm
#' @param HR hazard ratio of experimental group over control group
#' @param alpha1.t shape parameters of weibull survival distribution for experimental arm
#' @param nu1.t scale parameter of a weibull survival distribution for control arm
#' @param s  enrollment time
#' @param m maximum follow-up time for a subject
#'
#' @return This function returns a list including all design parameters as the same with input parameters of this function. If parameter d is missing, this function will calculate l, based on other parameters. If parameter N.0 or N.1 is missing, this function will calculate N.0 or $N.1$, based on other parameters. If parameter $l$ is missing, this function will calculate $l$ based on other parameters. If parameter gamma.c is missing, this function will calculate gamma.c based on other parameters.
#' @export
#'
#' @examples # calculate the expected number of events
#' TrialPred.TwoArm(N.0=100,N.1=100,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,gamma.c=1,s=5,m=4,l=6)
TrialPred.TwoArm <- function(
                                N.0=NULL,
                                N.1=NULL,
                                ratio=NULL,
                                d=NULL,
                                l=NULL,
                                gamma.c=NULL,
                                alpha0.t=NULL,
                                nu0.t=NULL,
                                HR = NULL,
                                alpha1.t=NULL,
                                nu1.t=NULL,
                                s=NULL,
                                m=NULL


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


