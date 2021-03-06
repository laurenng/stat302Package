% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_t.test.R
\name{my_t.test}
\alias{my_t.test}
\title{T-test function}
\usage{
my_t.test(x, alternative, mu)
}
\arguments{
\item{x}{a numeric vector of data}

\item{alternative}{a character string specifying the alternative hypothesis.}

\item{mu}{a number indicating the null hypothesis value of the mean.}
}
\value{
list with the following elements: numeric test statistic,
degree of freedom, value of alternative parameter, and p-value
}
\description{
This function performs a t-test with the given parameters:
vector, alternative hypothesis, and mu number.
}
\examples{
my_t.test(stat302Package::my_gapminder$lifeExp, alternative = "two.sided", mu = 2)

}
\keyword{inference}
\keyword{t-test,}
