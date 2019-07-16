#' @title upper bound newcombe
#' @description calculates upper bound of (1-alpha)100\% confidence interval
#' using Newcombe's method
#' @param pc numeric, probability in group 'c'
#' @param lb_pc numeric, lower bound of probability in group 'c', Wilson's
#' method
#' @param ub_pc numeric, upper bound of probability in group 'c', Wilson's
#' method
#' @param pt numeric, probability in group 't'
#' @param lb_pt numeric, lower bound of probability in group 't', Wilson's
#' method
#' @param ub_pt numeric, upper bound of probability in group 't', Wilson's
#' method
#' @return numeric
#' @examples
#' lb1 <- lb_wn(1.96, 0.6, 100)
#' ub1 <- ub_wn(1.96, 0.6, 100)
#' lb2 <- lb_wn(1.96, 0.8, 100)
#' ub2 <- ub_wn(1.96, 0.8, 100)
#' ub_wn_p2(0.6, lb1, ub1, 0.8, lb2, ub2)
#' @rdname ub_wn_p2
#' @export
ub_wn_p2 <- function (pc, lb_pc, ub_pc, pt, lb_pt, ub_pt){
  pc - pt + sqrt( (ub_pc - pc)^2 + (pt - lb_pt)^2 )
}
