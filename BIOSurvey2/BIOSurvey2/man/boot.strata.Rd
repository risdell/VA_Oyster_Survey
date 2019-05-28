\name{boot.strata}
\alias{boot.strata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bootstrap confidence intervals for stratified mean estimates.}
\description{
 Returns an object of class boot that contains bootstrap estimates of the stratified mean.
}
\usage{
boot.strata(object, nless = 0, nresamp = 1, method = c("RESCALE", "MIRROR-MATCH", "BWR", "NAIVE"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{strata object created by \code{\link{Stratify}}.}
  \item{nless}{parameter to set resample sample size for RESCALE method (nless=1,2,3).}
  \item{nresamp}{the number of bootstrap resamples to be calculated.}
  \item{method}{choose one of four methods of resampling from survey data which incorporate the survey design. RESCALE: Method of Rao and Wu (1988) which resamples with 
  replacement either 1,2 or 3 less than the number of samples within each stratum. MIRROR-MATCH:  Method of Sitter (1992). Not implemented yet. 
  BWR (Bootstrap with replacement): Special case  of MIRROR-MATCH when the number of population sample units is much larger than the number of sampled units.
  NAIVE: Simple method of resampling the n sample units with replacement from the n available in each stratum.  
  This method is known to provide biased estimates of the variance of the stratified mean (Rao and Wu 1988).}
}
\details{
  WARNING!  Although, the bootstrap algorithms has been written as efficiently as possible (using modified versions of the sapply() function) they are still
  written in R code and may take a few minutes. An implementation using C code with a dynamic load will be considered for a future version.
}
\value{ A list object with class boot containing
\item{orig.mean}{original stratified mean.}
\item{orig.var}{original variance of stratified mean.}
\item{boot.means}{vector of length=nresamp containing bootstrap estimates of stratified mean.}
\item{boot.vars}{vector of length=nresamp containing estimate of variance of stratified mean from each bootstrap resample.}
\item{accel}{acceleration constant needed for BCa confidence interval (see Smith 1997, for details). accel=NA when any of the sample sizes within strata are less than or equal to 1.}
\item{call}{copy of the function call to document what methods were chosen.}
}
\references{
Kovar, J.G., Rao, J.N.K. and C.F.J. Wu. (1988). 
Bootstrap and other methods to measure errors in survey estimates.  \emph{Can. J. Stat}. \bold{16} (supplement): 25--45.

Rao, J.N.K. and C.F.J. Wu.  (1988). 
Resampling inference with complex survey data. \emph{J. Amer. Stat. Assoc}. \bold{83}: 231--241.

Sitter, R.R. (1992). 
A resampling procedure for complex survey data. \emph{J. Amer. Stat. Assoc}. \bold{87}: 755--765.

Smith, S.J. (1997). 
Bootstrap confidence limits for groundfish trawl survey estimates of  mean abundance. \emph{Can. J. Fish. Aquat. Sci.}  \bold{54}: 616--630.
}

\author{Stephen J. Smith \email{smithsj@eastlink.ca}}


\seealso{\code{\link{Stratify}}}
\examples{

\dontshow{


Scallop.numbers<-Stratify(BoFSPA4.data,BoFSPA4.strata,"STRATA.ID",species="Commercial")

}

Scallop.numbers.boot<-boot.strata(Scallop.numbers,nresamp=1000, method="BWR")
}
\keyword{htest}
\concept{bootstrap estimates}
\concept{Stratified estimates}
\concept{Confidence Intervals}
