#' @title combine multiple imputatiton results
#' @description combines multiple imputation results
#' @param dt dataframe/tibble of summaized estimates of parameter of interest
#' per imputation
#' @param level factor, Default: c(1, 2), specifies if the imputation was
#' performed at one-stage (level=1), or two-stages (level=2)
#' @param phat numeric, column of estimates for parameter of interest
#' @param var_phat numeric, column of estimates of variance of phat
#' @return tibble
#' @details DETAILS
#' @examples
#' dt <- tibble::tibble(y = rbinom(100,1,0.6))
#' dt$ym <- c(rep(NA, 10), dt$y[11:100])
#' dt_mi <- mi(dt, 5, ym = 'ym')
#' dt_mi$phat_var <- dt_mi$phat*(1 - dt_mi$phat)/dt_mi$n_obs
#' mi_comb(dt_mi, level = 1, 'phat', 'phat_var')
#' @seealso
#'  \code{\link[rlang]{sym}}
#'  \code{\link[stats]{cor}}
#' @rdname mi_comb
#' @export
#' @import dplyr
#' @importFrom rlang sym
#' @importFrom stats var
mi_comb <- function(dt, level = c(1,2), phat, var_phat){

  if (level==1){

    out <- dt%>%
      dplyr::summarise(qbar = mean(!!rlang::sym(phat)),
                       ubar = mean(!!rlang::sym(var_phat)),
                       b = stats::var(!!rlang::sym(phat)),
                       n = dplyr::n())%>%
      dplyr::mutate(t = (1 + 1/n)*b + ubar,
                    rn = (1 + 1/n)*b/ubar,
                    v = dplyr::case_when(b!= 0 ~ floor((n - 1)*(1 + ubar/((1+1/n)*b))^2),
                                  TRUE  ~ as.numeric(1000000000)))

  }

  if (level ==2){

    out.by.m <-
      dt%>%
      dplyr::group_by(m)%>%
      dplyr::summarise(qbar.m = mean(!!rlang::sym(phat)),
                       ubar.m = mean(!!rlang::sym(var_phat)),
                       q.m.var = stats::var(!!rlang::sym(phat)),
                       n = dplyr::n())
    out <-
      out.by.m%>%
      dplyr::summarise(qbar = mean(qbar.m),
                       ubar = mean(ubar.m),
                       b = stats::var(qbar.m),
                       w = mean(q.m.var),
                       n = mean(n),
                       m = dplyr::n())%>%
      dplyr::mutate(t = ubar + (1 + 1/m)*b + (1 - 1/n)*w,
                    rn = (1 + 1/n)*b/ubar,
                    sn = (1 - 1/n)*w/ubar,
                    v_1 = ((1 + 1/m)*b/t)^2/(m-1) + ((1 - 1/n)*w/t)^2/(m*(n-1)),
                    v = dplyr::case_when(b!=0 ~ floor(1/v_1),
                                  TRUE ~ as.numeric(1000000000)))
  }

  return(out)
}
