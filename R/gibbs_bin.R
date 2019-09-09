#' @title Gibbs sampling for beta-binomial distribution
#' @description performs Gibbs sampling for beta-binomial distribution
#' @param dt tibble
#' @param B numeric, Default: 1000, number of iterations
#' @param y.m string, Default: 'y.m', column with incomplete binary data
#' @return complete dataset after B iterations of Gibbs sampler
#' @seealso
#'  \code{\link[stats]{Beta}},\code{\link[stats]{Binomial}}
#' @rdname gibbs_bin
#' @export
#' @importFrom stats rbeta rbinom
gibbs_bin <- function(dt, B = 1000, y.m = 'y.m'){

  pstar <- vector('numeric',length = B)

  y.im <- gsub('\\.m','.im',y.m)

  for(i in 1:B){

    if(i==1){
      v <- dt[dt$r==0,y.m]
      pstar.old <- Inf
    }else{
      v <- dt[[y.im]]
    }

    vec <- table(v) + 1
    names(vec) <- c('fail','success')
    pstar[i] <- stats::rbeta(n = 1, shape1 = vec[['success']], shape2 = vec[['fail']])

    dt[[y.im]]  <- dt[[y.m]]
    dt[dt$r==1,y.im] <- stats::rbinom(sum(dt$r==1), 1, pstar[i])

  }

  dt$pstar <- pstar[B]
  dt
}
