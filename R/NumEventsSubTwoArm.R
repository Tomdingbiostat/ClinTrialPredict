

#' Title
#'
#' @param N.0 example
#' @param N.1 example
#' @param ratio example
#' @param d 1
#' @param alpha0.t 1
#' @param nu0.t 1
#' @param alpha1.t 1
#' @param nu1.t 1
#' @param gamma.c 1
#' @param s 1
#' @param m 1
#' @param l 1
#'
#' @return
#' @export
#'
#' @examples
NumEventsSubTwoArm <- function(
                               N.0=NULL,       # Number of subjects plan to be enrolled in group 1
                               N.1=NULL,       # Number of subjects plan to be enrolled in group 2
                               ratio=NULL,     # N.1 / N.0
                               d=NULL,         # expected number of events observed at l
                               alpha0.t=NULL,  # shape parameter of a weibull survival distribution for group 1
                               nu0.t=NULL,     # scale parameter of a weibull survival distribution for group 1
                               alpha1.t,       # shape parameters of a weibull survival distribution for group 2
                               nu1.t,          # scale parameter of a weibull survival distribution for group 2
                               gamma.c,        # parameters of a weibull censoring distribution for both groups
                               s,              # enrollment period time
                               m,              # maximum follow-up time for a subject
                               l,              # observation time
                               design2=NULL     # all design parameters for two-arm
                              )
  {

  if(!is.null(design2)){
    for (name in names(design2)) { assign(name, design2[[name]]) }
  }

  design1.0 <- list(N=N.0,d=d,l=l,gamma=gamma.c,s=s,m=m,alpha=alpha0.t,nu=nu0.t)
  design1.1 <- list(N=N.1,d=d,l=l,gamma=gamma.c,s=s,m=m,alpha=alpha1.t,nu=nu1.t)

  #print(design1.0)

  if(!is.null(N.0) & !is.null(N.1) & is.null(d)){
      #g0 <- NumEventsSub(N=N.0,s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)
      g0 <- NumEventsSub(design1 = design1.0)
      #g1 <- NumEventsSub(N=N.1,s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
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
      #g0 <- NumEventsSub(N=100,s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)
      g0 <- NumEventsSub(design1 = design1.0)
      #g1 <- NumEventsSub(N=100,s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
      g1 <- NumEventsSub(design1 = design1.1)

      P0.delta.0 <-  g0$P.delta.0
      P1.delta.0 <-  g1$P.delta.0
      N.0 <- d / (P0.delta.0 + ratio * P1.delta.0)
      N.1 <- N.0 * ratio

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
                      ,P0.delta.0 = P0.delta.0
                      ,d0 = N.0 * P0.delta.0
                      ,P1.delta.0 = P1.delta.0
                      ,d1 = N.1 * P1.delta.0
                      ,d = d
      )

  }
  return(result)
}

# NumEventsSubTwoArm(N.0=100,N.1=100,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,gamma.c=1,s=5,m=4,l=6)
#
# design2 <- list(N.0=100,N.1=100,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,gamma.c=1,s=5,m=4,l=6)
# NumEventsSubTwoArm(design2=design2)

#NumEventsSubTwoArm(ratio=1,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,gamma.c=1,s=5,m=4,l=6, d=23.87)
