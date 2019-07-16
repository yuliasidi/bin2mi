#' @title conditional missing probability for missing not at random
#' @description calculates probability of missing conditional on y=0
#' @param do_tar numeric, target dro-out rate
#' @param mp_y1 numeric, missing probability conditional on y=1
#' @param p_y1 numeric, probability of y=1
#' @return numeric
#' @details the aim of this function is to calculate probability of missing
#' conditional on y=0, in order to impose mnar missingness structure
#' @examples
#' mp_y0(0.1, 0.14, 0.65)
#' @rdname mp_y0
#' @export

mp_y0 <- function(do_tar, mp_y1, p_y1){

  (do_tar - mp_y1*p_y1)/(1 - p_y1)

  }
