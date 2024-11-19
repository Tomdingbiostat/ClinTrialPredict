
#' Calculate the observation time for a two-arm clinical trial
#'
#' @inheritParams TrialPred.TwoArm
#' @inherit TrialPred.TwoArm
#' @return This function returns a list containing all design parameters, including the calculated observation time `l`
#' @export
#' @examples # calculate the observation time
#' ObsTime.TwoArm(N.0=100,N.1=100,d=10,gamma.c=1,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,s=5,m=4)
#'
ObsTime.TwoArm <- function(    N.0=NULL,
                               N.1=NULL,
                               ratio=NULL,
                               d=NULL,
                               gamma.c=NULL,
                               alpha0.t=NULL,
                               nu0.t,
                               alpha1.t,
                               nu1.t,
                               s,
                               m,
                               design2=NULL
                            ){

      if(!is.null(design2)){ for (name in names(design2)) { assign(name, design2[[name]]) } }

      P0.delta.0.s <- NumEventsSub.OneArm(N=N.0,s=s,m=m,l=s,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)$P.delta.0
      P1.delta.0.s <- NumEventsSub.OneArm(N=N.1,s=s,m=m,l=s,alpha=alpha1.t,nu=nu0.t,gamma=gamma.c)$P.delta.0
      d.s <- N.0 * P0.delta.0.s + N.1 * P1.delta.0.s


      P0.delta.0.m <- NumEventsSub.OneArm(N=N.0,s=s,m=m,l=m,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)$P.delta.0
      P1.delta.0.m <- NumEventsSub.OneArm(N=N.1,s=s,m=m,l=m,alpha=alpha1.t,nu=nu0.t,gamma=gamma.c)$P.delta.0
      d.m <- N.0 * P0.delta.0.m + N.1 * P1.delta.0.m


      P0.delta.0.sm <- NumEventsSub.OneArm(N=N.0,s=s,m=m,l=m+s,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)$P.delta.0
      P1.delta.0.sm <- NumEventsSub.OneArm(N=N.1,s=s,m=m,l=m+s,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)$P.delta.0
      d.sm <- N.0 * P0.delta.0.sm + N.1 * P1.delta.0.sm


      # Scenario 1: l<s, l<m
      if(d<d.s & d<d.m){
        of <- function(l){
          int <- N.0 * integral.s1(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c) +
                 N.1 * integral.s1(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
          abs(int - d)
        }
        l <- stats::optimize(of,interval=c(0,s))
        P0.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)
        P1.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
      }

      # Scenario 2:
      else if(d<=d.s & d>d.m){
        of <- function(l){
          int <- N.0 * integral.s2(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c) +
                 N.1 * integral.s2(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
          abs(int - d)
        }
        l <- stats::optimize(of,interval=c(0,s))
        P0.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)
        P1.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
      }

      # Scenario 3:
      else if(d>d.s & d<=d.m){
        of <- function(l){
          int <- N.0 * integral.s3(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c) +
                 N.1 * integral.s3(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
          abs(int - d)
        }
        l <- stats::optimize(of,interval=c(0,m))
        P0.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)
        P1.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
      }

      # Scenario 4:
      else if(d > d.s & d > d.m & d<=d.sm){
        of <- function(l){
          int <- N.0 * integral.s4(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c) +
                 N.1 * integral.s4(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
          abs(int - d)
        }
        l <- stats::optimize(of,interval=c(0,s+m))
        P0.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)
        P1.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
      }

      else if(d>d.sm){
        stop("Error: can not acheieve the expected number of events")
      }

      result <- list(
                     N.0 = N.0
                    ,N.1 = N.1
                    ,ratio=ratio
                    ,d=d
                    ,l=l$minimum
                    ,gamma.c = gamma.c
                    ,s=s
                    ,m=m
                    ,alpha0.t = alpha0.t
                    ,nu0.t    = nu0.t
                    ,alpha1.t = alpha1.t
                    ,nu1.t    = nu1.t
                    ,P0.delta.0 = P0.delta.0
                    ,d0 = N.0 * P0.delta.0
                    ,P1.delta.0 = P1.delta.0
                    ,d1 = N.1 * P1.delta.0
      )

      return(result)
}




