
SimData <- function(list=list,seed){

alpha <- list$alpha
nu <- list$nu
gamma <- list$gamma
N <- list$N
s <- list$s
l <- list$l
m <- list$m

set.seed(seed)

ID <- 1:N
t <- rweibull(N, shape = alpha,scale=nu)
c <- rexp(N, rate = gamma)
a <- runif(N, min = 0, max = s)
event <- ifelse(t <= c & (t + a) < l & t <=m, 1, 0)
dataset <- data.frame(ID, t, c, a, event)

return( list(  ds = dataset
              ,spec = list
  
              )
)

}