\name{accel.str}
\alias{accel.str}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate stratified acceleration estimate}
\description{
  Calculate stratified acceleration estimate for BCa confidence interval as per Smith (1997).
}
\usage{
accel.str(x.str)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x.str}{List containing observations in each stratum (as a list object, one element per stratum), number of 
observations per stratum and Stratum labels}
}
\details{
  This is an internal function called by \code{summary.boot} and is not intended for stand-alone use.
}

\references{Smith, S.J. (1997). 
Bootstrap confidence limits for groundfish trawl survey estimates of  
mean abundance. \emph{Can. J. Fish. Aquat. Sci.}  \bold{54}: 616--630.}

\author{Stephen J. Smith, \email{smithsj@mar.dfo-mpo.gc.ca}}

\seealso{\code{\link{summary.boot}}, \code{\link{accel.est}}}



\keyword{internal}% 
