
#' Title
#'
#' @param N.0
#' @param N.1
#' @param d
#' @param alpha0.t
#' @param nu0.t
#' @param alpha1.t
#' @param nu1.t
#' @param gamma.c
#' @param s
#' @param m
#'
#' @return
#' @export
#'
#' @examples
ObsTimeTwoArm <- function(     N.0,       # Number of subjects plan to be enrolled in group 1
                               N.1,       # Number of subjects plan to be enrolled in group 2
                               d,         # Expected number of events observed
                               alpha0.t,  # shape parameter of a weibull survival distribution for group 1
                               nu0.t,     # scale parameter of a weibull survival distribution for group 1
                               alpha1.t,  # shape parameters of a weibull survival distribution for group 2
                               nu1.t,     # scale parameter of a weibull survival distribution for group 2
                               gamma.c,   # parameters of a weibull censoring distribution for both groups
                               s,         # enrollment period time
                               m,         # maximum follow-up time for a subject
                               design2
                            ){

      if(!is.null(design2)){ for (name in names(design2)) { assign(name, design2[[name]]) } }

      P0.delta.0.s <- NumEventsSub(N=N.0,s=s,m=m,l=s,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)$P.delta.0
      P1.delta.0.s <- NumEventsSub(N=N.1,s=s,m=m,l=s,alpha=alpha1.t,nu=nu0.t,gamma=gamma.c)$P.delta.0
      d.s <- N.0 * P0.delta.0.s + N.1 * P1.delta.0.s

      #print(d.s)

      P0.delta.0.m <- NumEventsSub(N=N.0,s=s,m=m,l=m,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)$P.delta.0
      P1.delta.0.m <- NumEventsSub(N=N.1,s=s,m=m,l=m,alpha=alpha1.t,nu=nu0.t,gamma=gamma.c)$P.delta.0
      d.m <- N.0 * P0.delta.0.m + N.1 * P1.delta.0.m

      #print(d.m)

      P0.delta.0.sm <- NumEventsSub(N=N.0,s=s,m=m,l=m+s,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)$P.delta.0
      P1.delta.0.sm <- NumEventsSub(N=N.1,s=s,m=m,l=m+s,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)$P.delta.0
      d.sm <- N.0 * P0.delta.0.sm + N.1 * P1.delta.0.sm
      #print(d.sm)


      # f2.0 <- function(a,t){
      #   1/s * dweibull(t,shape=alpha0.t,scale = nu0.t) * (1-pexp(t,rate=gamma.c))
      # }
      #
      # f2.1 <- function(a,t){
      #   1/s * dweibull(t,shape=alpha1.t,scale = nu1.t) * (1-pexp(t,rate=gamma.c))
      # }

      # Scenario 1: l<s, l<m
      if(d<d.s & d<d.m){
        # of <- function(l){
        #   int <- N.0 * integral2(f2.0, 0, l, 0, function(x) l-x)$Q +
        #          N.1 * integral2(f2.1, 0, l, 0, function(x) l-x)$Q
        #   abs(int - d)
        # }
        of <- function(l){
          int <- N.0 * integral.s1(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c) +
                 N.1 * integral.s1(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
          abs(int - d)
        }
        l <- optimize(of,interval=c(0,s))
        P0.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)
        P1.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
      }

      # Scenario 2:
      else if(d<=d.s & d>d.m){
        # of <- function(l){
        #   int <- N.0 * ( integral2(f2.0,0,l-m,0,m)$Q + integral2(f2.0,l-m,l,0,function(x) l-x)$Q )+
        #          N.1 * ( integral2(f2.1,0,l-m,0,m)$Q + integral2(f2.1,l-m,l,0,function(x) l-x)$Q )
        #   abs(int - d)
        # }

        of <- function(l){
          int <- N.0 * integral.s2(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c) +
                 N.1 * integral.s2(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
          abs(int - d)
        }
        l <- optimize(of,interval=c(0,s))
        P0.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)
        P1.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
      }

      # Scenario 3:
      else if(d>d.s & d<=d.m){
        # of <- function(l){
        #   int <- N.0 * ( integral2(f2.0, 0, s, 0, function(x) l-x)$Q )+
        #          N.1 * ( integral2(f2.1, 0, s, 0, function(x) l-x)$Q )
        #   abs(int - d)
        # }
        of <- function(l){
          int <- N.0 * integral.s3(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c) +
                 N.1 * integral.s3(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
          abs(int - d)
        }
        l <- optimize(of,interval=c(0,m))
        P0.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)
        P1.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
      }

      # Scenario 4:
      else if(d > d.s & d > d.m & d<=d.sm){
        # of <- function(l){
        #   int <- N.0* ( integral2(f2.0,0,l-m ,0 ,m)$Q + integral2(f2.0,l-m ,s ,0 ,function(x) l-x)$Q )+
        #          N.1* ( integral2(f2.1,0,l-m ,0 ,m)$Q + integral2(f2.1,l-m ,s ,0 ,function(x) l-x)$Q )
        #   abs(int - d)
        # }
        of <- function(l){
          int <- N.0 * integral.s4(s=s,m=m,l=l,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c) +
                 N.1 * integral.s4(s=s,m=m,l=l,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
          abs(int - d)
        }
        l <- optimize(of,interval=c(0,s+m))
        P0.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha0.t,nu=nu0.t,gamma=gamma.c)
        P1.delta.0 <- integral.s1(s=s,m=m,l=l$minimum,alpha=alpha1.t,nu=nu1.t,gamma=gamma.c)
      }

      else if(d>d.sm){
        stop("Error: can not acheieve the expected number of events")
      }

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
                    ,l=l$minimum
                    ,d=d
                    ,P0.delta.0 = P0.delta.0
                    ,d0 = N.0 * P0.delta.0
                    ,P1.delta.0 = P1.delta.0
                    ,d1 = N.1 * P1.delta.0
      )

      return(result)
}




