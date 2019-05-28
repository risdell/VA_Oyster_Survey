\name{print.boot}
\alias{print.boot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Print method for boot objects}
\description{
  This function provides a basic summary of a bootstrap object for stratified means.
}
\usage{
\method{print}{boot}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{boot object created by \code{\link{boot.strata}}.}
	\item{...}{further arguments passed to or from other methods.}
}
\details{
  this function is a method for the generic function print for class "boot".
}
\value{
  
\item{orig.mean}{original stratified mean.}
\item{orig.var}{original variance of stratified mean.}
\item{Number of bootstraps}{number of replications.}
\item{Bootstrap mean}{mean of bootstrap stratified means.}
\item{Average Bootstrap Variance}{mean variance calculated from each bootstrap resample.}
}

 \author{Stephen J. Smith \email{smithsj@eastlink.ca}}

\seealso{\code{\link{boot.strata}}}
\examples{
\dontrun{
print(Scallop.numbers.boot)}
}
\keyword{print}
\concept{Stratified estimates}
