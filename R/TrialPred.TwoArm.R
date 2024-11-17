
#' Function for predicting two-arm clinical trial
#'
#' @param N.0 number of subjects plan to be enrolled in control arm
#' @param N.1 number of subjects plan to be enrolled in experimental arm
#' @param ratio randomization ratio between two arms: `N.1` / `N.0`
#' @param d expected number of events observed at time `l`
#' @param l observation time
#' @param gamma.c parameter of the exponential distribution of censoring time
#' @param alpha0.t shape parameter of weibull survival distribution for control arm
#' @param nu0.t scale parameter of weibull survival distribution for control arm
#' @param HR hazard ratio of experimental group over control group
#' @param alpha1.t shape parameters of weibull survival distribution for experimental arm
#' @param nu1.t scale parameter of a weibull survival distribution for control arm
#' @param s  enrollment time
#' @param m maximum follow-up time for a subject
#' @param design2 a list containing all the above parameters for two-arm design
#'
#' @return This function returns a list containing all design parameters as the same with input parameters of this function. If any one of the parameters `d`, `N.0`(or `N.1`), `l` or `gamma.c` is missing, it can be calculated based on the other parameters.
#'
#' @export
#'
#' @description
#' predicting two-arm clinical trial
#'
#'
#' @examples # calculate the expected number of events
#' TrialPred.TwoArm(N.0=100,N.1=100,d=NULL,l=6,gamma.c=1,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,s=5,m=4)
#'
#' # calculate the expected number of events using a list as input
#' design2 <- list(N.0=100,N.1=100,d=NULL,l=6,gamma.c=1,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,s=5,m=4)
#' TrialPred.TwoArm(design2=design2)
#'
#' # calculate the number of subject enrolled
#' TrialPred.TwoArm(N.0=NULL,N.1=NULL,ratio=1,d=24,l=6,gamma.c=1,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,s=5,m=4)
#'
#' # calculate the observation time
#' TrialPred.TwoArm(N.0=100,N.1=100,d=10,l=NULL,gamma.c=1,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,s=5,m=4)
#'
#' # calculate the censoring parameter
#' TrialPred.TwoArm(N.0=100,N.1=100,d=10,l=3,gamma.c=NULL,alpha0.t=1,nu0.t=5,alpha1.t=2,nu1.t=4,s=5,m=4)

TrialPred.TwoArm <- function(
                                 N.0=NULL
                                ,N.1=NULL
                                ,ratio=NULL
                                ,d=NULL
                                ,l=NULL
                                ,gamma.c=NULL
                                ,alpha0.t=NULL
                                ,nu0.t=NULL
                                ,HR = NULL
                                ,alpha1.t=NULL
                                ,nu1.t=NULL
                                ,s=NULL
                                ,m=NULL
                                ,design2=NULL
){

  if(!is.null(design2)){
    for (name in names(design2)) { assign(name, design2[[name]]) }
  }

  if(!is.null(HR)){
    alpha1.t <- alpha0.t
    nu1.t <- nu0.t * HR^(1/alpha0.t)
  }

  if(!is.null(ratio) & !is.null(N.0) & is.null(N.1)){
    N.1 <- N.0 * ratio
  }else if(!is.null(ratio) & !is.null(N.1) & is.null(N.0)){
    N.0 <- N.1 / ratio
  }else if(!is.null(N.0) & !is.null(N.1)){
    ratio <- N.1 / N.0
  }

  design2 <- list(N.0=N.0,N.1=N.1,ratio=ratio,d=d,l=l,gamma.c=gamma.c,alpha0.t=alpha0.t,nu0.t=nu0.t,alpha1.t=alpha1.t,nu1.t=nu1.t,HR=HR,s=s,m=m)

  if( is.null(d) ){
    return(NumEventsSub.TwoArm(design2=design2))
  }

  else if( is.null(N.0) & is.null(N.1) ){
    return(NumEventsSub.TwoArm(design2=design2))
  }

  else if( is.null(l) ){
    return(ObsTime.TwoArm(design2=design2))
  }

  else if( is.null(gamma.c) ){
    return(CensTime.TwoArm(design2=design2))
  }
}


