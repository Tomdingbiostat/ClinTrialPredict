
#' Calculate the expected number of events or number of subjects enrolled in a one-arm clinical trial
#'
#' @inheritParams TrialPred.OneArm
#'
#' @return This function returns a list containing all design parameters as the same with input parameters of this function.
#' @export
#'
#' @examples # Calculate the expected number of events in a one-arm clinical trial
#' NumEventsSub.OneArm(N=100,d=NULL,l=3,gamma=0.1,s=12,m=6,alpha=1,nu=20)
#'
NumEventsSub.OneArm <- function( N=NULL
                         ,d=NULL
                         ,l=NULL
                         ,gamma=NULL
                         ,s=NULL
                         ,m=NULL
                         ,alpha=NULL
                         ,nu=NULL
                         ,design1=NULL
                         ){

  if(!is.null(design1)){
    for (name in names(design1)) {
      assign(name, design1[[name]])
    }
  }

  # Scenario 1
  #
  if(l<=s & l<=m){
    P.delta.0 <- integral.s1(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
  }

  # Scenario 2
  else if(l<=s & l>m){
    P.delta.0 <- integral.s2(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)

  }

  # Scenario 3
  else if(l>s & l<=m){
    P.delta.0 <-  integral.s3(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
  }

  # Scenario 4
  else if(l>s & l>m & l <s+m){
    P.delta.0 <-  integral.s4(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
  }

  # Scenario 5
  else if(l>s & l>m & l>=s+m){
    P.delta.0 <-  integral.s5(s=s,m=m,l=l,alpha=alpha,nu=nu,gamma=gamma)
  }
  else{
    stop('wrong input')
  }

  if(is.null(N) & !is.null(d)){
    N <- d / P.delta.0
  }

  if(is.null(d) & !is.null(N)){
    d <- N * P.delta.0
  }


  res <- list(
               N=N
              ,d=d
              ,l=l
              ,gamma=gamma
              ,s=s
              ,m=m
              ,alpha=alpha
              ,nu=nu
              ,P.delta.0=P.delta.0
              )

  return(res)

}










