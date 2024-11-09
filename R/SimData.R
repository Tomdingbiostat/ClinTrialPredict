
#' Title
#'
#' @param design1 list containing all the above parameters for single-arm design
#' @param seed random seed
#' @param nsim number of simulations
#'
#' @return
#' @export
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

  # alpha <- design$alpha
  # nu <- design$nu
  # gamma <- design$gamma
  # N <- design$N
  # s <- design$s
  # l <- design$l
  # m <- design$m

  if(!is.null(design1)){
    for (name in names(design1)) { assign(name, design1[[name]]) }
  }else{
    design1 <- list(N=N,d=d,l=l,gamma=gamma,s=s,m=m,alpha=alpha,nu=nu)
  }

  set.seed(seed)

  ID <- seq(1,N*nsim)
  sim <- ceiling(ID/N)
  t <- rweibull(N*nsim, shape = alpha, scale=nu)
  c <- rexp(N*nsim, rate = gamma)
  a <- runif(N*nsim, min = 0, max = s)
  event <- ifelse(t <= c & (t + a) <= l & t <=m, 1, 0)
  dataset <- data.frame(sim, t, c, a, event)

  return( list(  dataset = dataset,design1 = design1) )

}
