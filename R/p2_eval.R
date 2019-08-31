#' @title difference in propotions evaluation
#' @description calculates coverage, confidence inteval length, proportion of
#' times confidence interal was outside the limits, proportions of times the
#' width of the confidence interval was zero
#' @param dt tibble which contains simulation results
#' @param m2  numeric, pre-specified margin
#' @param mi_level factor, PARAM_DESCRIPTION, Default: 1
#' @return tibble
#' @details DETAILS
#' @seealso
#'  \code{\link[purrr]{map}}
#' @rdname p2_eval
#' @export
#' @importFrom purrr map_df
#' @import dplyr
p2_eval <- function(dt, m2, mi_level=1){

  dt1 <-
    dt%>%
    purrr::map_df(.f=function(x) x$mi_all,.id = 'sim')%>%
    dplyr::filter(method%in%c("wald", "fm", "wn-mi", "wn-plug"))%>%
    dplyr::mutate(coverage = ifelse(m2<=upper_bound & m2>=lower_bound, 1, 0),
                  ci_length = upper_bound - lower_bound,
                  outrange = ifelse(upper_bound>1 | lower_bound< -1, 1, 0),
                  width0 = ifelse(upper_bound==lower_bound, 1, 0)
    )

  if (mi_level==1){
    dt2 <-
      dt1%>%
      dplyr::group_by(method)%>%
      dplyr::summarise(mean_qbar = mean(qbar), mean_cov = mean(coverage), mean_length = mean(ci_length),
                       mean_outrange = mean(outrange), mean_width0 = mean(width0))

  }

  if (mi_level==2){
    dt2 <-
      dt1%>%
      dplyr::group_by(method)%>%
      dplyr::summarise(mean_qbar = mean(qbar), mean_cov = mean(coverage), mean_length = mean(ci_length),
                       mean_outrange = mean(outrange), mean_width0 = mean(width0))

  }

  return(dt2)

}

