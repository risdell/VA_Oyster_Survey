\name{accel.est}
\alias{accel.est}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of acceleration estimate}
\description{
 Calculation of acceleration estimate for the BCa bootstrap confidence intervals as per Efron and Tibshirani (1993).
}
\usage{
accel.est(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{List containing observations in each stratum (as a list object, one element per stratum), number of 
observations per stratum and Stratum labels}
}
\details{
 This is an internal function called by \code{accel.str} and is not intended for stand-alone use.
}


\references{ 
Efron, B., and Tibshirani, R.J. (1993). \emph{An introduction to the bootstrap.}
Chapman & Hall, London.
}
\author{Stephen J. Smith, \email{smithsj@mar.dfo-mpo.gc.ca}}

\seealso{\code{\link{accel.str}}, \code{\link{summary.boot}}}

\keyword{internal}% 
