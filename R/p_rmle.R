#' @title restricted maximum likelihood estimate: proportion difference
#' @description calculates restricted maximum likelihood estimate under
#' the null hypothesis of difference in binomial proportions
#' @param m2 numeric, difference in binomial proportions under the null
#' @param nt integer, number of observations in group 't'
#' @param nc integer, number rof observation in group 'c'
#' @param pc numeric, estimated binomial proportion in group 'c'
#' @param pt numeric, estimated binomial proportion in group 't't
#' @return numeric
#' @details returns resticted maximum likelihood estimate for binomial
#' proportion in group C
#' @examples
#' p_rmle(0.1, 100, 100, 0.65, 0.57)
#' @rdname p_rmle
#' @export
p_rmle <- function(m2, nt, nc, pc, pt){

  theta <- nt/nc
  a <- 1 + theta
  b <- -1*(1 + theta + pc + theta*pt + m2*(2 + theta))
  c <- m2^2 + m2*(2*pc + theta + 1) + pc + theta*pt
  d <- -pc*m2*(1 + m2)
  v <- (b/(3*a))^3 - b*c/(6*a^2) + d/(2*a)
  u <- sign(v)*sqrt((b/(3*a))^2 - c/(3*a))
  u = ifelse(u!=0,u,0.00001)
  w <- (pi+acos(v/(u^3)))/3
  pc.rmle <- 2*u*cos(w) - b/(3*a)

  return(pc.rmle)

}


