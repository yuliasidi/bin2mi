#' @title upper bound wilson
#' @description calculates upper bound of (1-alpha)100\% confidence interval
#' using Wilson's method
#' @param z numeric, quantile of either standard normal or t distribution
#' coresponding to the desired confidence level 1- alpha
#' @param phat numeric, phat proporrtion estimate
#' @param n_obs numeric, n_obs number of observations
#' @return numeric
#' @examples
#' ub_wn(1.96, 0.6, 100)
#' @rdname ub_wn
#' @export
ub_wn <- function(z, phat, n_obs){

  (phat + z^2/(2*n_obs) +
     z*sqrt(
       phat*(1 - phat)/n_obs + (z/(2*n_obs))^2
     ))/
    (1 + z^2/n_obs)

}
