
#' Simulating survival dataset for a one-arm design
#'
#' @inheritParams TrialPred.OneArm
#' @param design1 a list containing all the above parameters for one-arm design
#' @param seed random seed number
#' @param nsim number of simulations
#'
#' @return This function will return the simulated datasets and the according design settings
#' @export
#'
#' @examples design1 <- TrialPred.OneArm(N=100,d=NULL,l=3,gamma=0.1
#'                                      ,s=12,m=6,alpha=1,nu=20)
#' # Simulate 100 datasets under design1
#' SimData.OneArm(design1=design1)
#'
SimData.OneArm <- function(
                     N=NULL
                    ,d=NULL
                    ,l=NULL
                    ,gamma=NULL
                    ,s=NULL
                    ,m=NULL
                    ,alpha=NULL
                    ,nu=NULL
                    ,design1
                    ,seed
                    ,nsim
                    ){

  if(!is.null(design1)){
    for (name in names(design1)) { assign(name, design1[[name]]) }
  }else{
    design1 <- list(N=N,d=d,l=l,gamma=gamma,s=s,m=m,alpha=alpha,nu=nu)
  }

  set.seed(seed)

  ID <- seq(1,N*nsim)
  sim <- ceiling(ID/N)
  subject <- ID %% N
  t <- stats::rweibull(N*nsim, shape = alpha, scale=nu)
  c <- stats::rexp(N*nsim, rate = gamma)
  a <- stats::runif(N*nsim, min = 0, max = s)
  event <- ifelse(t <= c & (t + a) <= l & t <=m, 1, 0)
  dataset <- data.frame(sim,subject,a, t, c, event)

  return( list(  dataset = dataset,design1 = design1) )

}
