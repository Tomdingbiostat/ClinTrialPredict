

#' Title
#'
#' @param N  example
#' @param d  example
#' @param l  example
#' @param gamma example
#' @param s example
#' @param m example
#' @param alpha example
#' @param nu example
#' @param design1 example
#'
#' @return example
#' @export
#'
#' @examples ex
TrialPred.OneArm <- function(
    N=NULL        # Number of subjects enrolled
    ,d=NULL        # expected number of events observed at l
    ,l=NULL        # observation calendar time
    ,gamma=NULL    # random censoring rate
    ,s=NULL        # enrollment period
    ,m=NULL        # maximum follow-up for a single subject
    ,alpha=NULL    # shape parameter
    ,nu=NULL       # scale parameter
    ,design1=NULL



){

  if(!is.null(design1)){
    for (name in names(design1)) { assign(name, design1[[name]]) }
  }

  if( is.null(s) | is.null(m) | is.null(alpha) | is.null(nu) ) {
    stop("missing parameter s,m,alpha, or nu")
  }

  if(is.null(N) & !is.null(d) & !is.null(l) & !is.null(gamma)){
    return(NumEventsSub(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma,d=d))
  }

  if(is.null(d) & !is.null(N) & is.null(l) & !is.null(gamma)){
    return(NumEventsSub(N=N,s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma))
  }

  if(is.null(l) & !is.null(d) & !is.null(N) & !is.null(gamma)){
    return(ObsTime(N=N,d=d,s=s,m=m,alpha=alpha,nu=nu,gamma=gamma))
  }

  if(is.null(gamma) & !is.null(d) & !is.null(N) & is.null(l) ){
    return(CensRate(N=N,d=d,s=s,m=m,l=l,alpha=alpha,nu=nu))
  }
}
