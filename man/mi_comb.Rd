% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mi_comb.R
\name{mi_comb}
\alias{mi_comb}
\title{combine multiple imputatiton results}
\usage{
mi_comb(dt, level = c(1, 2), phat, var_phat)
}
\arguments{
\item{dt}{dataframe/tibble of summaized estimates of parameter of interest
per imputation}

\item{level}{factor, Default: c(1, 2), specifies if the imputation was
performed at one-stage (level=1), or two-stages (level=2)}

\item{phat}{numeric, column of estimates for parameter of interest}

\item{var_phat}{numeric, column of estimates of variance of phat}
}
\value{
tibble
}
\description{
combines multiple imputation results
}
\details{
DETAILS
}
\examples{
dt <- tibble::tibble(y = rbinom(100,1,0.6))
dt$y.m <- c(rep(NA, 10), dt$y[11:100])
dt$r <- ifelse(is.na(dt$y.m)==TRUE, 1, 0)
dt_mi <- mi(dt, 5)
dt_mi$phat_var <- dt_mi$phat*(1 - dt_mi$phat)/dt_mi$n_obs
mi_comb(dt_mi, level = 1, 'phat', 'phat_var')
}
\seealso{
\code{\link[rlang]{sym}}
 \code{\link[stats]{cor}}
}
