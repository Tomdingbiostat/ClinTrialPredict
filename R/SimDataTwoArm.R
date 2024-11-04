
SimDataTwoArm <- function(design,seed,nsim){

  design0 <- list(
                     N = design$N.0
                    ,l = design$l
                    ,gamma = design$gamma.c
                    ,alpha = design$alpha0.t
                    ,nu = design$nu0.t
                    ,s = design$s
                    ,m = design$m
                  )
  #print(design0)

  design1 <- list(
                     N = design$N.1
                    ,l = design$l
                    ,gamma = design$gamma.c
                    ,alpha = design$alpha1.t
                    ,nu = design$nu1.t
                    ,s = design$s
                    ,m = design$m
                  )
  #print(design1)

  ds0 <- SimData(design=design0,seed=seed,nsim=nsim)$ds
  ds0$arm <- 0
  ds1 <- SimData(design=design1,seed=seed,nsim=nsim)$ds
  ds1$arm <- 1
  ds <- rbind(ds0,ds1)


  return(ds)

}
