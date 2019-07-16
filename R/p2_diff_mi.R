#' @title estimates for difference in binomial proportions
#' @description calculates estiamtes for difference in proportions, normal
#' approximation variance and resticted maximum likelihood variance terms
#' @param dt tibble, output after mi
#' @param m2 numeric, pre-specified margin
#' @return tibble
#' @details DETAILS
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#' @rdname p2d_mi
#' @export
#' @import dplyr
#' @import tidyr
p2d_mi <- function(dt, m2) {
  dt%>%
    dplyr::mutate(phat_var = phat*(1 - phat)/n_obs)%>%

    dplyr::mutate(phat_var = phat*(1 - phat)/n_obs)%>%
    tidyr::gather('var','val',-c(n,trt))%>%
    tidyr::unite(x, trt, var)%>%
    tidyr::spread(x, val)%>%
    dplyr::mutate(pc_rmle = p_rmle(m2 = m2, nt = t_n_obs, nc = c_n_obs, pc = c_phat, pt = t_phat),
                  pt_rmle = pc_rmle - m2)%>%
    dplyr::mutate(phat_d = c_phat - t_phat,
                  var_d = c_phat_var + t_phat_var,
                  var_dr = pc_rmle*(1 - pc_rmle)/c_n_obs + pt_rmle*(1 - pt_rmle)/t_n_obs)
}
