
#' Title
#'
#' @param design
#' @param seed
#' @param nsim
#'
#' @return
#' @export
#'
#' @examples
SimData <- function(design,seed,nsim){

  alpha <- design$alpha
  nu <- design$nu
  gamma <- design$gamma
  N <- design$N
  s <- design$s
  l <- design$l
  m <- design$m

  set.seed(seed)

  ID <- seq(1,N*nsim)
  sim <- ceiling(ID/N)
  t <- rweibull(N*nsim, shape = alpha,scale=nu)
  c <- rexp(N*nsim, rate = gamma)
  a <- runif(N*nsim, min = 0, max = s)
  event <- ifelse(t <= c & (t + a) < l & t <=m, 1, 0)
  dataset <- data.frame(sim, t, c, a, event)

  return( list(  ds = dataset,spec = list) )

}
