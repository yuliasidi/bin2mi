#' @title condifence interval for difference in binomial proportions
#' @description calculates (1-alpha) 100\% confidence interval for difference
#' in binomial proportions
#' @param dt_mle tibble containing mle estimated from p2_mle output
#' @param method character string indicating one of the following methods for
#' confidence interval construction: "wald", "fm", "wn".
#' @param alpha numeric, alpha level for two-sided confidence interval
#' @return tibble
#' @details DETAILS
#' @examples
#' dt <- dt_p2(100, 0.7, 0.65)
#' dtmle <- p2_mle(dt, m2 = 0.05)
#' p2_full_ci(dtmle, method = "wald", alpha = 0.05)
#' @seealso
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#'  \code{\link[stats]{Normal}}
#'  \code{\link[purrr]{map2}}
#' @rdname p2_full_ci
#' @export
#' @import dplyr
#' @importFrom stats qnorm
#' @importFrom purrr pmap_dbl
p2_full_ci <- function(dt_mle, method = c('wald', 'fm', 'wn'), alpha){

  if (method=='wald'){
    ci_out <- dt_mle%>%
      dplyr::mutate( method = 'wald',
                     lower_bound = phat_d - stats::qnorm(1 - alpha/2)*sqrt(var_d),
                     upper_bound = phat_d + stats::qnorm(1 - alpha/2)*sqrt(var_d))%>%
      dplyr::select(phat_d, c_n_obs, method, lower_bound, upper_bound)

  }

  if (method=='fm'){
    ci_out <- dt_mle%>%
      dplyr::mutate( method = 'fm',
                     lower_bound = phat_d - stats::qnorm(1 - alpha/2)*sqrt(var_dr),
                     upper_bound = phat_d + stats::qnorm(1 - alpha/2)*sqrt(var_dr))%>%
  dplyr::select(phat_d, c_n_obs, method, lower_bound, upper_bound)

  }

  if (method=='wn'){
    ci_out <- dt_mle%>%
      dplyr::mutate(method = 'wn',
                    lb_pc = purrr::pmap_dbl(list(phat = c_phat, n_obs = c_n_obs), lb_wn, z = stats::qnorm(1 - alpha/2)),
                    ub_pc = purrr::pmap_dbl(list(phat = c_phat, n_obs = c_n_obs), ub_wn, z = stats::qnorm(1 - alpha/2)),
                    lb_pt = purrr::pmap_dbl(list(phat = t_phat, n_obs = t_n_obs), lb_wn, z = stats::qnorm(1 - alpha/2)),
                    ub_pt = purrr::pmap_dbl(list(phat = t_phat, n_obs = t_n_obs), ub_wn, z = stats::qnorm(1 - alpha/2)))%>%
      dplyr::mutate(lower_bound = purrr::pmap_dbl(list(pc = c_phat, lb_pc = lb_pc, lb_pt = lb_pt,
                                                ub_pc = ub_pc, ub_pt = ub_pt, pt = t_phat),
                                          lb_wn_p2),
                    upper_bound = purrr::pmap_dbl(list(pc = c_phat, lb_pc = lb_pc, lb_pt = lb_pt,
                                                ub_pc = ub_pc, ub_pt = ub_pt, pt = t_phat),
                                          ub_wn_p2))%>%
      dplyr::select(phat_d, c_n_obs, method, lower_bound, upper_bound)

  }
return(ci_out)
}

