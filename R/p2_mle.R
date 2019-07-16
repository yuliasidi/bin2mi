#' @title maximumn likelihood estimates for proportions
#' @description calcualates maximum and resttricted maximum likelhood estimates
#' for differrence in binomial proportions
#' @param dt dataframe/tibble
#' @param m2 numeric, pre-specified margin
#' @return tibble
#' @examples
#' dt <- dt_p2(100, 0.7, 0.65)
#' p2_mle(dt, m2 = 0.05)
#' @rdname p2_mle
#' @export
#' @import dplyr
#' @import tidyr
p2_mle <- function(dt, m2){

  dt%>%
    dplyr::group_by(trt)%>%
    dplyr::summarise(phat = mean(y), n_obs = dplyr::n())%>%
    tidyr::gather('var','val',-trt)%>%
    tidyr::unite(x, trt, var)%>%
    tidyr::spread(x, val)%>%
    dplyr::mutate(phat_d = c_phat - t_phat,
                  var_d = c_phat*(1 - c_phat)/c_n_obs + t_phat*(1 - t_phat)/t_n_obs,
                  pc_rmle = p_rmle(m2 = m2, nt = t_n_obs, nc = c_n_obs, pc = c_phat, pt = t_phat),
                  pt_rmle = pc_rmle - m2,
                  var_dr = pc_rmle*(1 - pc_rmle)/c_n_obs + pt_rmle*(1 - pt_rmle)/t_n_obs)
}

