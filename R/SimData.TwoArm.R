
#' Title
#'
#' @param design2  list containing all the above parameters for two-arm design
#' @param seed random seed
#' @param nsim number of simulations
#'
#' @return
#' @export
#'
#' @examples
#'
#'
SimData.TwoArm <- function(
                             N.0=NULL
                            ,N.1=NULL
                            ,ratio=NULL
                            ,d=NULL
                            ,l=NULL
                            ,gamma.c=NULL
                            ,s=NULL
                            ,m=NULL
                            ,alpha0.t=NULL
                            ,nu0.t=NULL
                            ,HR = NULL
                            ,alpha1.t=NULL
                            ,nu1.t=NULL
                            ,design2=NULL
                            ,seed=NULL
                            ,nsim=NULL
                          ){

  if(!is.null(design2)){
    for (name in names(design2)) { assign(name, design2[[name]]) }
  } else{
    design2 <- list(N.0=N.0,N.1=N.1,ratio=ratio,d=d,l=l,gamma.c=gamma.c,alpha0.t=alpha0.t,nu0.t=nu0.t,alpha1.t=alpha1.t,nu1.t=nu1.t,HR=HR,s=s,m=m)
  }


  design1.0 <- list(N=N.0,d=d,l=l,gamma=gamma.c,s=s,m=m,alpha=alpha0.t,nu=nu0.t)
  design1.1 <- list(N=N.1,d=d,l=l,gamma=gamma.c,s=s,m=m,alpha=alpha1.t,nu=nu1.t)

  # design0 <- list(
  #                    N = design$N.0
  #                   ,l = design$l
  #                   ,gamma = design$gamma.c
  #                   ,alpha = design$alpha0.t
  #                   ,nu = design$nu0.t
  #                   ,s = design$s
  #                   ,m = design$m
  #                 )

  # design1 <- list(
  #                    N = design$N.1
  #                   ,l = design$l
  #                   ,gamma = design$gamma.c
  #                   ,alpha = design$alpha1.t
  #                   ,nu = design$nu1.t
  #                   ,s = design$s
  #                   ,m = design$m
  #                 )

  ds0 <- SimData(design1=design1.0,seed=seed,nsim=nsim)$ds
  ds0$arm <- 0
  ds1 <- SimData(design1=design1.1,seed=seed,nsim=nsim)$ds
  ds1$arm <- 1
  ds <- rbind(ds0,ds1)

  #return(ds)
  return( list(  ds = dataset,design2 = design2) )

}
