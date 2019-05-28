\name{summary.strata}
\alias{summary.strata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summary method for strata objects}
\description{
  Summary of strata object including stratified mean and total, respective standard error and conventional confidence intervals.
}
\usage{
\method{summary}{strata}(object, alpha.t = 0.05, effic = FALSE, nopt = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{strata object created by \link{Stratify}}
  \item{alpha.t}{the alpha level for the student's-t confidence intervals. Default is 0.05.  Confidence intervals also provided.}
  \item{effic}{logical flag: if TRUE, then design-efficiency estimates for the allocation and stratification components are calculated. Estimated maximum efficiency for optimal allocation
  for current stratification also calculated.}
  \item{nopt}{logical flag: if TRUE, then optimal sample sizes per stratum are calculated based on current estimates of strata variances.  Compromise allocation for case where there 
  must be a minimum of 2 samples per strata also calculated. This option can only be calculated if sample size greater than 1 in all strata.}
	\item{...}{further arguments passed to or from other methods.}
}
\details{
  Student's-t confidence intervals calculated using Satterthwaite's approximation (Cochran 1977).  Efficiency estimates based on Sukhatme and Sukhatme's (1953) 
  approximation (Smith and Gavaris 1993).  This function is a method for the generic function summary for class "strata".
}
\value{
  A data.frame containing the following:
\item{yst}{estimate of the stratified mean.}
\item{se.yst}{estimate of standard error of mean.}
\item{Yst}{estimate of stratified total.}
\item{df.yst}{effective degrees of freedom.}
\item{alpha}{alpha level for confidence interval.}
\item{ci.yst}{lower and upper limits for confidence interval.}
\item{effic.alloc}{allocation component of estimated efficiency as a percentage of simple random sampling variance.}
\item{effic.str}{strata component of estimated efficiency as a percentage of simple random sampling variance.}
\item{var.ran}{estimated variance of the mean for a simple random sample.}
\item{max.eff}{estimated maximum efficiency for optimal allocation  for current stratification.}
\item{nopt}{A matrix of the optimal and compromise sample sizes (minimum sample size = 2) for each stratum.}
\item{descrip}{Simple label ``Stratified Analysis'' for summary object.}
}
\references{

Cochran, W.G. (1977). \emph{Sampling Techniques.} John Wiley & Sons. New York, N.Y.

Smith, S.J. and S. Gavaris. (1993). 
Improving the precision of fish abundance estimates of Eastern Scotian Shelf cod from bottom trawl surveys. 
\emph{North American Journal of Fisheries Management}. \bold{13}: 35--47.

Sukhatme, R.V., and B.V. Sukhatme.  (1970). \emph{Sampling theory of surveys
with applications.} Iowa State University Press, Ames, Iowa.
 }
 
 \author{Stephen J. Smith \email{smithsj@eastlink.ca}}


\seealso{\code{\link{Stratify}}, \code{\link{print.strata}}}
\examples{\dontrun{summary(Scallop.numbers,effic=TRUE)}}
\keyword{methods}
\concept{Stratified estimates}
\concept{Design efficiency}
\concept{Optimal sample allocation}
\concept{Confidence Intervals}
