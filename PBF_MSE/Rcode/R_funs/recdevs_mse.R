#' Return a set of recruitment deviations
#' This function is based on the ss3sim get_recdevs.r by Kelli Johnson
#' This function returns a set of recruitment deviations based
#' on an iteration number and a sigma r. Given the same iteration number the function will
#' return the same recruitment deviations, given the sigma r from th 2017 albacore stock assessment, 0.5.
#' Note that the  
#' @param iteration The iteration number. This is used as an ID to set the
#'   random number seed.
#' @param n The length of the vector returned.
#' @param sigma the sigma r determining the standard deviation of the normal distribution the deviations are sampled from. Note the mean is 0.
#' @param seed An integer value to pass to \code{\link[base]{set.seed}}
#' @return A vector of recruitment deviations.

recdevs_mse <- function(iteration, n, sigma, rho, seed = 21) {
  set.seed(seed)
  x = sample(1:1e6)[iteration]
  set.seed(x)
  rec_devs = rnorm(n, 0, sigma)
   
  return(rec_devs)
}