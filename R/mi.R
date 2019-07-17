#' @title multitple imputation for proportions
#' @description employs one/two-stage multiple imputation for proportions
#' @param dt dataframe/tibble
#' @param n_mi positive integer, number of multiply imputed subject level values
#' @param m_mi non-negative integer, Default: 0, number of muttiply imputed
#' values for multitplier k
#' @param mu_k numeric, Default: 1, mean value for normal distribution of
#' multiplier k
#' @param sd_k numeric, Default: 0, standard deviation value for normal
#' distribution of multiplier k
#' @param ym integer, incomplete binary column
#' @param phat_out logic, Default TRUE
#' @return tibble, summary per imputation or if phat_out=FALSE, returns imputed
#'  data (available only for one-stage imputation)
#' @details DETAILS
#' @examples
#' dt <- tibble::tibble(y = rbinom(100,1,0.6))
#' dt$ym <- c(rep(NA, 10), dt$y[11:100])
#'  mi(dt, 5, ym = 'ym')
#'  mi(dt, 2, 10, mu_k = 1.3, sd_k = 0.1, ym = 'ym')
#' @seealso
#'  \code{\link[bin2mi]{sf_count}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[tidyr]{unnest}}
#' @rdname mi
#' @export
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom purrr map
#' @importFrom rlang sym
#' @importFrom stats runif rbeta rnorm
#' @importFrom tidyr unnest
mi <- function(dt, n_mi, m_mi=0, mu_k=1, sd_k=0, ym, phat_out = TRUE){

  #count number of successes and failure to be used in the posterior distribution of p
  ym.sum <- sf_count(dt, ym)

  #run the subject level inputation n_mi times
  t <- tibble::tibble(n = seq(1,n_mi,1))%>%
    dplyr::mutate(dt.mi = purrr::map(n, .f = function(x){

      dt%>%
        dplyr::filter(is.na(!!rlang::sym(ym)))%>%
        dplyr::mutate(
          p.thresh = stats::runif(dplyr::n(), 0, 1),
          pstar = stats::rbeta(dplyr::n(), shape1 = ym.sum$success - 1, shape2 = ym.sum$fail - 1))%>%
        dplyr::select(p.thresh, pstar)%>%
        dplyr::bind_rows(dt%>%
                           dplyr::filter(!is.na(!!rlang::sym(ym)))%>%
                           dplyr::select(!!rlang::sym(ym)))%>%
        dplyr::mutate(y.im = dplyr::case_when(is.na(!!rlang::sym(ym)) ~ ifelse(pstar>=p.thresh, 1, 0),
                                              !is.na(!!rlang::sym(ym)) ~ as.numeric(!!rlang::sym(ym))))
    }))


  #if m=0 then determine imputed values of y and combine the result
  if (m_mi==0){

    t1 <-
      t%>%
      dplyr::mutate(dt.mi.sum = purrr::map(dt.mi, .f = function(x){
        x%>%
          dplyr::summarise(phat = mean(y.im), n_obs = dplyr::n())
      }))%>%
      dplyr::select(n, dt.mi.sum)%>%
      tidyr::unnest()

  }

  #if m>0 then the two-stage imputation is employed
  if (m_mi>0){

    k.mults <- tibble::tibble(m = seq(1, m_mi,1),
                      k = stats::rnorm(m_mi, mu_k, sd_k))

    # multiply phats for each arm separetly, set 0/1 for incomplete outcomes and combine imputed perrr arm data
    t1 <-
      k.mults%>%
      dplyr::mutate(res = purrr::map(k, .f = function(k){

        t%>%
          tidyr::unnest()%>%
          dplyr::mutate(pstar.k = k * pstar)%>%
          dplyr::select(-y.im)%>%
          dplyr::mutate(y.im = dplyr::case_when(is.na(!!rlang::sym(ym)) ~ ifelse(pstar.k>=p.thresh, 1, 0),
                                         !is.na(!!rlang::sym(ym)) ~ as.numeric(!!rlang::sym(ym))))%>%
          dplyr::group_by(n)%>%
          dplyr::summarise(phat = mean(y.im), n_obs = dplyr::n())

      }))%>%
      tidyr::unnest()


  }

  if (!phat_out){
    return(t)
  }
  else{
    return(t1)
  }

}
