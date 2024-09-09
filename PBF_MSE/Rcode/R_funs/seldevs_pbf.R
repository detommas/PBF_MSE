#' Return a set of selectivity deviations
#'
#' This function returns a set of random deviations for each of the time varying age selectivites (age 1-4) based
#' on an iteration number. Given the same iteration number the function will
#' return the same deviations, given the specified age-specific sigma.
#' @param iteration The iteration number. This is used as an ID to set the
#'   random number seed.
#' @param n The length of the vector returned.
#' @param na the number of ages with time varying selectivity
#' @param seed An integer value to pass to \code{\link[base]{set.seed}}
#' @param ctl_file specifies the ctl file to read in 
#' @return A vector of recruitment deviations.

seldevs_alb <- function(iteration, n, na, seed = 21) {
  set.seed(seed)
  x = sample(1:1e6)[iteration]
  sel_devs = matrix(0, n, na)
  sigmav = c(0.6,0.9,0.9,0.8)
  set.seed(x)
  for (age in 1:na){
    sel_devs[,age]= rnorm(n, 0, sigmav[age])
  }
   
  return(sel_devs)
}