

#' Title
#'
#' @inheritParams TrialPred.TwoArm
#'
#' @return
#' @export
#'
#' @examples
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
                               s,
                               m,
                               design2=NULL
                              )
  {

  if(!is.null(design2)){
    for (name in names(design2)) { assign(name, design2[[name]]) }
  }

  design1.0 <- list(N=N.0,d=d,l=l,gamma=gamma.c,s=s,m=m,alpha=alpha0.t,nu=nu0.t)
  design1.1 <- list(N=N.1,d=d,l=l,gamma=gamma.c,s=s,m=m,alpha=alpha1.t,nu=nu1.t)

  if(!is.null(N.0) & !is.null(N.1) & is.null(d)){
      g0 <- NumEventsSub(design1 = design1.0)
      g1 <- NumEventsSub(design1 = design1.1)
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
      g0 <- NumEventsSub(design1 = design1.0)
      g1 <- NumEventsSub(design1 = design1.1)

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
