#' @title lower bound wilson
#' @description calculates lower bound of (1-alpha)100\% confidence interval
#' using Wilson's method
#' @param z numeric, quantile of either standard normal or t distribution
#' coresponding to the desired confidence level 1- alpha
#' @param phat numeric, proporrtion estimate
#' @param n_obs numeric, number of observations
#' @return numeric
#' @examples
#' lb_wn(1.96, 0.6, 100)
#' @rdname lb_wn
#' @export
lb_wn <- function(z, phat, n_obs){

  (phat + z^2/(2*n_obs) -
     z*sqrt(
       phat*(1 - phat)/n_obs + (z/(2*n_obs))^2
     ))/
    (1 + z^2/n_obs)

}
