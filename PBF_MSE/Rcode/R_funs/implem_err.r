#' Return a set of implementation errors for each fleet
#'
#' This function returns a set of one-sided implementation errors (i.e. actual catch will always be a bit more than the TAC) 
#' for each fleet (as defined by a gear/country combination) of the NPALB fishery that is dependent on an iteration number. 
#' Given the same iteration number the function will return the implementation error if the same sigma is chosen.
#' Note that the  
#' @param iteration The iteration number. This is used as an ID to set the
#'   random number seed.
#' @param n The length of the vector returned.
#' @param nf the number of fleets
#' @param sigma the standard deviation of the error distribution (here also what we want the mean error value to be)
#' @param seed An integer value to pass to \code{\link[base]{set.seed}}
#' @return A table of implmentation errors by fleets defined by gear/country.

implem_err <- function(iteration, n, nf, sigma, seed = 21) {
  set.seed(seed)
  x = sample(1:1e6)[iteration]
  imp_err <- matrix(0, n, nf)
  for (flt in 1:nf){
    set.seed(x)
    imp_err[,flt]= (1+sigma) + abs(rnorm(n, 0, sigma))
  }
  
  return(imp_err)
}