

#' Calculate the expected number of events or number of subjects enrolled in a two-arm clinical trial
#'
#' @description
#' Calculate the expected number of events or number of subjects enrolled in a two-arm clinical trial
#'
#' @inheritParams TrialPred.TwoArm
#'
#' @return This function returns a list containing all design parameters as the same with input parameters of this function.
#' @export
#'
#' @examples # calculate the expected number of events
#' NumEventsSub.TwoArm(N.0=100,N.1=100,l=6,gamma.c=1,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,s=5,m=4)
#'
#' # calculate the expeTrcted number of events using a list as input
#' design2 <- list(N.0=100,N.1=100,l=6,gamma.c=1,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,s=5,m=4)
#' NumEventsSub.TwoArm(design2=design2)
#'
#' # calculate the number of subject enrolled
#' NumEventsSub.TwoArm(ratio=1,d=24,l=6,gamma.c=1,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,s=5,m=4)
#'
NumEventsSub.TwoArm <- function(
                               N.0=NULL,
                               N.1=NULL,
                               ratio=NULL,
                               d=NULL,
                               l=NULL,
                               gamma.c=NULL,
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

  design1.0 <- list(N=N.0,d=d,l=l,gamma=gamma.c,s=s,m=m,alpha=alpha0.t,nu=nu0.t)
  design1.1 <- list(N=N.1,d=d,l=l,gamma=gamma.c,s=s,m=m,alpha=alpha1.t,nu=nu1.t)

  if(!is.null(N.0) & !is.null(N.1) & is.null(d)){
      g0 <- NumEventsSub.OneArm(design1 = design1.0)
      g1 <- NumEventsSub.OneArm(design1 = design1.1)
      result <- list(
                       N.0 = N.0
                      ,N.1 = N.1
                      ,alpha0.t = alpha0.t
                      ,nu0.t    = nu0.t
                      ,alpha1.t = alpha1.t
                      ,nu1.t    = nu1.t
                      ,gamma.c = gamma.c
                      ,s=s
                      ,m=m
                      ,l=l
                      ,P0.delta.0 = g0$P.delta.0
                      ,d0 = g0$d
                      ,P1.delta.0 = g1$P.delta.0
                      ,d1 = g1$d
                      ,d = g0$d + g1$d
      )
  }
  else if(is.null(N.0) & is.null(N.1) & !is.null(d)){
      g0 <- NumEventsSub.OneArm(design1 = design1.0)
      g1 <- NumEventsSub.OneArm(design1 = design1.1)

      P0.delta.0 <-  g0$P.delta.0
      P1.delta.0 <-  g1$P.delta.0
      N.0 <- d / (P0.delta.0 + ratio * P1.delta.0)
      N.1 <- N.0 * ratio

      result <- list(
                       N.0 = N.0
                      ,N.1 = N.1
                      ,d = d
                      ,l=l
                      ,gamma.c = gamma.c
                      ,alpha0.t = alpha0.t
                      ,nu0.t    = nu0.t
                      ,alpha1.t = alpha1.t
                      ,nu1.t    = nu1.t
                      ,s=s
                      ,m=m
                      ,P0.delta.0 = P0.delta.0
                      ,d0 = N.0 * P0.delta.0
                      ,P1.delta.0 = P1.delta.0
                      ,d1 = N.1 * P1.delta.0

      )

  }
  return(result)
}
