% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hoCoef.R
\name{hoCoef}
\alias{hoCoef}
\title{Performs a hypothesis test that a linear model parameter is equal to a specific value.}
\usage{
hoCoef(object, term = 2, bo = 0, alt = c("two.sided", "less", "greater"))
}
\arguments{
\item{object}{A \code{lm} object.}

\item{term}{A single numeric that indicates which term in the model to use in the hypothesis test.}

\item{bo}{The null hypothesized parameter value.}

\item{alt}{A string that identifies the \dQuote{direction} of the alternative hypothesis. The strings may be \code{"less"} for a \dQuote{less than} alternative, \code{"greater"} for a \dQuote{greater than} alternative, or \code{"two.sided"} (DEFAULT) for a \dQuote{not equals} alternative.}
}
\value{
A matrix that contains the term number, hypothesized value, parameter estimate, standard error of the parameter estimate, t test statistic, degrees-of-freedom, and corresponding p-value.
}
\description{
Performs a hypothesis test that a linear model parameter is equal to a specific value. Useful for testing that a parameter is equal to a value other than 0.
}
\details{
The \dQuote{direction} of the alternative hypothesis is identified by a string in the \code{alt} argument.

If the \code{lm} object is from a simple linear regression with an intercept then \code{term=1} will use the intercept and \code{term=2} will use the slope in the hypothesis test.
}
\examples{
data(Mirex,package="FSA")
# Simple linear regression test HA:slope!=0.1
lm1 <- lm(mirex~weight, data=Mirex)
hoCoef(lm1,2,0.1)

}
\seealso{
\code{\link{htest.nlsBoot}}.
}
\author{
Derek H. Ogle, \email{derek@derekogle.com}
}
\keyword{htest}
