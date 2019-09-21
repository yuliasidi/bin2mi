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
#' @param y.m character, incomplete binary column, Defualt = 'y.m'
#' @param phat_out logic, Default TRUE
#' @return tibble, summary per imputation or if phat_out=FALSE, returns imputed
#'  data (available only for one-stage imputation)
#' @details DETAILS
#' @examples
#' dt <- tibble::tibble(y = rbinom(100,1,0.6))
#' dt$y.m <- c(rep(NA, 10), dt$y[11:100])
#' dt$r <- ifelse(is.na(dt$y.m)==TRUE, 1, 0)
#'  mi(dt, 5, y.m = 'y.m')
#'  mi(dt, 2, 10, mu_k = 1.3, sd_k = 0.1, y.m = 'y.m')
#' @seealso
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
mi <- function(dt, n_mi, m_mi=0, mu_k=1, sd_k=0,  y.m = 'y.m', phat_out = TRUE){

  y.im <- gsub('\\.m','.im',y.m)

    #run the subject level inputation n_mi times
    t <- tibble::tibble(n = seq(1,n_mi,1))%>%
      dplyr::mutate(dt.mi = purrr::map(n, gibbs_bin, dt = dt))


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
                                k = stats::rnorm(m_mi, mu_k, sd_k))%>%
        dplyr::mutate(k = ifelse(k>0, k, 0))

      # multiply phats for each arm separetly, set 0/1 for incomplete outcomes and combine imputed perrr arm data
      t1 <-
        k.mults%>%
        dplyr::mutate(res = purrr::map(k, .f = function(k){

          tt <- t%>%
            mutate(dt.mi.new =  purrr::map(dt.mi, .f = function(dt.mi){
              pstar.k <- k * dt.mi$pstar[1]

              dt.tmp <- dt.mi
              dt.tmp[[y.im]]  <- dt.tmp[[y.m]]

              dt.tmp[dt.tmp$r==1,y.im] <- stats::rbinom(sum(dt.tmp$r==1), 1, min(pstar.k,1))
              dt.tmp$pstar.k <- min(pstar.k,1)

              dt.tmp%>%
                dplyr::summarise(phat = mean(y.im), n_obs = dplyr::n())


            }))%>%
            dplyr::select(-dt.mi)%>%
            tidyr::unnest()

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
