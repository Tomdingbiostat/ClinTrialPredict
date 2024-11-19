

#' Function for predicting one-arm clinical trial
#'
#' @param N Number of subjects plan to enrolled
#' @param d expected number of events observed at time `l`
#' @param l observation time
#' @param gamma parameter of the exponential distribution of censoring time
#' @param s enrollment period
#' @param m maximum follow-up for a single subject
#' @param alpha shape parameter of weibull survival distribution
#' @param nu scale parameter of weibull survival distribution
#' @param design1 a list containing all the above parameters for one-arm design
#'
#' @return This function returns a list containing all design parameters as the same with input parameters of this function. If any one of the parameters `d`, `N`, `l` or `gamma` is missing, it can be calculated based on the other parameters.
#' @export
#'
#' @examples # Calculate the expected number of events in a one-arm clinical trial
#' TrialPred.OneArm(N=100,d=NULL,l=3,gamma=0.1,s=12,m=6,alpha=1,nu=20)
#'
#' #Calculate the expected number of events using a list as input
#' design1 <- list(N=100,d=NULL,l=3,gamma=0.1,s=12,m=6,alpha=1,nu=20)
#' TrialPred.OneArm(design1=design1)
#'
#' #Calculate the number of subjects enrolled
#' TrialPred.OneArm(N=NULL,d=8,l=15,gamma=0.1,s=12,m=6,alpha=1,nu=20)
#'
#' #Calculate the observation time
#' TrialPred.OneArm(N=100,d=10,l=NULL,gamma=0.1,s=12,m=6,alpha=1,nu=20)
#'
#' #Calculate the censoring parameter gamma
#' TrialPred.OneArm(N=100,d=10,l=10,gamma=NULL,s=12,m=6,alpha=1,nu=20)
TrialPred.OneArm <- function(
     N=NULL
    ,d=NULL
    ,l=NULL
    ,gamma=NULL
    ,s=NULL
    ,m=NULL
    ,alpha=NULL
    ,nu=NULL
    ,design1=NULL
){

  if(!is.null(design1)){
    for (name in names(design1)) { assign(name, design1[[name]]) }
  }

  if( is.null(s) | is.null(m) | is.null(alpha) | is.null(nu) ) {
    stop("missing parameter s,m,alpha, or nu")
  }

  if(is.null(N) & !is.null(d) & !is.null(l) & !is.null(gamma)){

    return(NumEventsSub.OneArm(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma,d=d))
  }

  if(is.null(d) & !is.null(N) & !is.null(l) & !is.null(gamma)){
    print("x")
    return(NumEventsSub.OneArm(N=N,s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma))
  }

  if(is.null(l) & !is.null(d) & !is.null(N) & !is.null(gamma)){
    return(ObsTime.OneArm(N=N,d=d,s=s,m=m,alpha=alpha,nu=nu,gamma=gamma))
  }

  if(is.null(gamma) & !is.null(d) & !is.null(N) & !is.null(l) ){
    return(CensRate.OneArm(N=N,d=d,s=s,m=m,l=l,alpha=alpha,nu=nu))
  }
}
