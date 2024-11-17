
NumEventsSub <- function( N=NULL
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


# (s1 <- NumEventsSub(N=100,s=5,m=3,l=2,  alpha=1,nu=5,gamma=1)) # l<s, l<m
#
# design1 <- list(N=100,s=5,m=3,l=2,  alpha=1,nu=5,gamma=1)
# NumEventsSub(design1=design1)
#
#
# (s2 <- NumEventsSub(N=100,s=5,m=3,l=4,  alpha=1,nu=5,gamma=1)) # l<s, l>m
# (s3 <- NumEventsSub(N=100,s=5,m=7,l=6,  alpha=1,nu=5,gamma=1)) # l>s, l<=m
# (s4 <- NumEventsSub(N=100,s=5,m=7,l=8,  alpha=1,nu=5,gamma=1)) # l>s, l>m, l < m + s
# (s5 <- NumEventsSub(N=100,s=5,m=7,l=12, alpha=1,nu=5,gamma=1)) # l>s, l>m, l > m + s









