#' @title propotion estimation evaluation
#' @description calculates coverage, confidence inteval length, proportion of
#' times confidence interal was outside the limits, proportions of times the
#' width of the confidence interval was zero
#' @param dt tibble which contains simulation results
#' @param trt group for which the evaluation is done
#' @param ptrt true proportion for group trt
#' @return tibble
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{summarise}}
#' @rdname p1_eval
#' @export
#' @import purrr
#' @import dplyr
p1_eval <- function(dt, trt, ptrt){

  dt%>%
    purrr::map_df(.f=function(x) x$mi_all,.id = 'sim')%>%
    dplyr::filter(method==sprintf('wn-mi for p%s', trt))%>%
                    dplyr::mutate(coverage = ifelse(ptrt<=upper_bound & ptrt>=lower_bound, 1, 0))%>%
                    dplyr::summarise(mean_cov = mean(coverage), mean_qbar = mean(qbar))%>%
    dplyr::mutate(trt = sprintf('p%s', trt))

}
