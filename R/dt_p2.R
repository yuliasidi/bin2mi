#' @title creates dataset for two binomial proportions
#' @description creates dataset for two binomial proportions
#' @param n integer, number of observation per group, 1:1 ratio assumed
#' @param pc numeric, probability of event in group 'c'
#' @param pt numeric, probability of event in group 't'
#' @param add_x logic, Default: FALSE, specifies whether to add variable x
#' @param xs_y0 numeric, Default: 0.6, for strong correlation between x and y,
#' define conditional probability of x given y=0
#' @param xs_y1 numeric, Default: 0.2, for strong correlation between x and y,
#' define conditional probability of x given y=1
#' @param xw numeric, Default: 0.6, for weak correlation between x and y,
#' define prrobability of x (irrespective of the value of y)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' dt_p2(100, 0.7, 0.65, add_x = TRUE)
#' @seealso
#'  \code{\link[stats]{Binomial}}
#'  \code{\link[purrr]{map}}
#' @rdname dt_p2
#' @export
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom stats rbinom
#' @importFrom purrr map_df
dt_p2 <- function(n, pc, pt, add_x = FALSE, xs_y0 = 0.6, xs_y1 = 0.2, xw = 0.6){

  dtfull <-
    tibble::tibble(
      y = stats::rbinom(n = n, 1, prob = pc),
      trt = "c"
    )%>%
    dplyr::bind_rows(tibble::tibble(
      y = stats::rbinom(n = n, 1, prob = pt),
      trt = "t"
    ))

  if (add_x){

    dtfull <- dtfull%>%
      split(.$trt)%>%
      purrr::map_df(.f = function(dx){

        dx0 <- dx%>%
          dplyr::filter(y == 0)%>%
          dplyr::mutate(x = stats::rbinom(n = dplyr::n(), 1, prob = xs_y0),
                        x_desc = "strong")

        dx1 <- dx%>%
          dplyr::filter(y == 1)%>%
          dplyr::mutate(x = stats::rbinom(n = dplyr::n(), 1, prob = xs_y1),
                        x_desc = "strong")


          dplyr::bind_rows(dx0, dx1,
                           dx%>%
                             dplyr::mutate(x = stats::rbinom(n = dplyr::n(), 1, prob = xw),
                                           x_desc = "weak")
                           )

      }
      )

  }

  return(dtfull)

}
