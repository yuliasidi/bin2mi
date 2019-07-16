#' @title success/fail counts
#' @description counts number of successes and failures for the incomplete
#' column ym
#' @param dt dataframe/tibble, includes incomplete binary column
#' @param ym integer, incomplete binary column
#' @return tibble
#' @examples
#' dt <- tibble::tibble(y = rbinom(100,1,0.6))
#' dt$ym <- c(rep(NA, 10), dt$y[11:100])
#' sf_count(dt, 'ym')
#' @seealso
#'  \code{\link[rlang]{sym}}
#'  \code{\link[tidyr]{spread}}
#' @rdname sf_count
#' @export
#' @import dplyr
#' @importFrom rlang sym
#' @importFrom tidyr spread
sf_count <- function(dt, ym){
  out <-
    dt%>%
    dplyr::filter(!is.na(!!rlang::sym(ym)))%>%
    dplyr::group_by(!!rlang::sym(ym))%>%
    dplyr::summarise(n_obs = dplyr::n())%>%
    dplyr::mutate(yname = ifelse(!!rlang::sym(ym)==0, "fail", "success"))%>%
    dplyr::select(-!!rlang::sym(ym))%>%
    tidyr::spread('yname', 'n_obs')

  return(out)
}
