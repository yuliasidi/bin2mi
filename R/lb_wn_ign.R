#' @title lower bound wilson, ignorable mi
#' @description calculates lower bound of (1-alpha)100\% confidence interval
#' using Wilson's method following MI assuming ignorability
#' @param z numeric, quantile of t distribution coresponding to the desired
#' confidence level 1- alpha
#' @param qhat numeric
#' @param n_obs integer, number of observations
#' @param rn interger
#' @return numeric
#' @examples
#' lb_wn_ign(1.96, 0.8, 100, 0.7)
#' @rdname lb_wn_ign
#' @export
lb_wn_ign <- function(z, qhat, n_obs, rn){
  (2*qhat + z^2/n_obs + z^2*rn/n_obs)/(2*(1 + z^2/n_obs + z^2*rn/n_obs)) -
    sqrt(
      (2*qhat + z^2/n_obs + z^2*rn/n_obs)^2/(2*(1 + z^2/n_obs + z^2*rn/n_obs))^2 -
        qhat^2/(1 + z^2/n_obs + z^2*rn/n_obs)
    )
}
