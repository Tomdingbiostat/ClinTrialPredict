
#' Title
#'
#' @param design1 list containing all the above parameters for single-arm design
#' @param seed random seed
#' @param nsim number of simulations
#'
#' @return
#'
#' @examples
SimData <- function(
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
  t <- rweibull(N*nsim, shape = alpha, scale=nu)
  c <- rexp(N*nsim, rate = gamma)
  a <- runif(N*nsim, min = 0, max = s)
  event <- ifelse(t <= c & (t + a) <= l & t <=m, 1, 0)
  dataset <- data.frame(sim,subject,a, t, c, event)

  return( list(  dataset = dataset,design1 = design1) )

}
