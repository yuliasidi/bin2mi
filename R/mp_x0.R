#' @title conditional missing probability for missing at random
#' @description calculates probability of missing conditional on x=0
#' @param do_tar numeric, target dro-out rate
#' @param p_y1 numeric, probability of y=1
#' @param mp_x1 numeric, missing probability conditional on x=1
#' @param xs_ass character, Default: c("strong", "weak")
#' @param xs_y0 numeric, Default: 0.6, for strong correlation between x and y,
#' define conditional probability of x given y=0
#' @param xs_y1 numeric, Default: 0.2, for strong correlation between x and y,
#' define conditional probability of x given y=1
#' @param xw numeric, Default: 0.6, for weak correlation between x and y,
#' define prrobability of x (irrespective of the value of y)
#' @return nymeric, conditional probability of missing when x=1
#' @details DETAILS
#' @examples
#' mp_x0(do_tar = 0.3, p_y1 = 0.65, mp_x1 = 0.4, xs_ass = 'strong')
#' @rdname mp_x0
#' @export
mp_x0 <- function(do_tar, p_y1, mp_x1, xs_ass = c('strong', 'weak'), xs_y0 = 0.6, xs_y1 = 0.2, xw = 0.6){

  if (xs_ass == 'strong'){
    out <- (do_tar - mp_x1*(xs_y1*p_y1 + xs_y0*(1 - p_y1)))/((1 - xs_y1)*p_y1 + (1 - xs_y0)*(1 - p_y1))

  }

  if (xs_ass == 'weak'){
    out <- (do_tar - mp_x1*xw)/(1 - xw)

  }

  if (out < 0) {
    print("Negative conditional probability")
  }
  else{
    return(out)
  }
}
