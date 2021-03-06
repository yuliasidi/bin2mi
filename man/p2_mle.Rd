% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/p2_mle.R
\name{p2_mle}
\alias{p2_mle}
\title{maximumn likelihood estimates for proportions}
\usage{
p2_mle(dt, m2)
}
\arguments{
\item{dt}{dataframe/tibble}

\item{m2}{numeric, pre-specified margin}
}
\value{
tibble
}
\description{
calcualates maximum and resttricted maximum likelhood estimates
for differrence in binomial proportions
}
\examples{
dt <- dt_p2(100, 0.7, 0.65)
p2_mle(dt, m2 = 0.05)
}
