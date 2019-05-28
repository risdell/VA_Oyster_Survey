\name{summary.boot}
\alias{summary.boot}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summary of boot strata object}
\description{This function is a method for the generic function summary for class boot.}  

\usage{
\method{summary}{boot}(object, CI.method = c("Percentile", "BC", "BCa"), alpha.b = 0.05, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{boot object created by \code{\link{boot.strata}}.}
  \item{CI.method}{choose one of three confidence interval methods (Efron and Tibshirani, 1993) for the bootstrap estimate of the stratifed mean. Percentile = empirical
percentiles; BC = bias-corrected; BCa = bias corrected and adjusted.
  The BCa method requires the acceleration estimation provided in boot.strata. This confidence interval method will produce an error statement if accel=NA.}
  \item{alpha.b}{alpha level for confidence interval.}
	\item{...}{further arguments passed to or from other methods.}
}
\details{
  This function is a method for the generic function summary for class "boot".
}
\value{
This function summarizes a boot object by printing out the original stratified mean, variance of stratified mean, bootstrap stratified mean and the variance of the bootstrap estimate. 
In addition to the upper and lower confidence limits, the length and shape of the confidence interval are also given.  The shape is calculated as the natural log of 
the ratio of the upper limit minus the median to the median minus the lower limit (Efron, 1992). A summary of the bootstrap estimates is also provided using \code{quantile}.
}
\references{
Efron, B. (1992). 
Jackknife-after-bootstrap standard errors and influence functions (with discussion). \emph{J. Royal Stat. Soc. Ser. B}  \bold{54}: 83--127.

Efron, B. and  R.J. Tibshirani.  (1993). 
\emph{An introduction to the bootstrap}. Chapman & Hall, London.

Smith, S.J. (1997). Bootstrap confidence limits for groundfish trawl survey estimates of  mean abundance. 
\emph{Can. J. Fish. Aquat. Sci.}  \bold{54}: 616--630.
}

 \author{Stephen J. Smith \email{smithsj@eastlink.ca}}


\seealso{\code{\link{boot.strata}}}
\examples{
\dontrun{summary.boot(Scallop.numbers.boot, CI.method="Percentile", alpha.b=0.10)}
}
\keyword{htest}
\concept{Stratified estimates}
\concept{Bootstrap}
\concept{Confidence Intervals}
