
#' Simulating survival dataset for a two-arm design
#'
#' @description
#' Simulating survival dataset for a two-arm design
#'
#'
#' @inheritParams TrialPred.TwoArm
#' @param design2  a list containing all the above parameters for two-arm design
#' @param seed random seed
#' @param nsim number of simulations
#'
#' @return This function will return the simulated datasets and the according design settings
#' @export
#'
#' @examples design2 <- NumEventsSub.TwoArm(N.0=100,N.1=100,l=6,gamma.c=1
#'                                          ,alpha0.t = 1,nu0.t=5,alpha1.t=2,nu1.t=4,s=5,m=4)
#' SimData.TwoArm(design2=design2,seed=1234,nsim=100)
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

  ds0 <- SimData(design1=design1.0,seed=seed,nsim=nsim)$dataset
  ds0$arm <- 0
  ds1 <- SimData(design1=design1.1,seed=seed,nsim=nsim)$dataset
  ds1$arm <- 1
  dataset <- rbind(ds0,ds1)

  return( list(  dataset = dataset,design2 = design2) )

}
