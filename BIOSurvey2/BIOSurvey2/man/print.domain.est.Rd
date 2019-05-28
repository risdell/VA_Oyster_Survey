\name{print.domain.est}
\alias{print.domain.est}
\alias{print}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print method for domain.est objects
}
\description{
Provides details on means and variances for each domain.
}
\usage{
\method{print}{domain.est}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object created by \code{\link{Domain.est}}
}
	\item{...}{further arguments passed to or from other methods.}
}


\details{
Print method for class "domain.est".
}
\value{
\item{Strata}{Name of the original stratification variable.}
 \item{Domain}{Name of the new stratification variable.}
\item{ybd}{Mean for each new strata (domain).}
 \item{var.ybd}{Variance of the mean for each new strata (domain).}
\item{var.diffdomain}{Penalty term in varance for differences between the strata means in the domain.}
  \item{nsdh}{Sample size for each intersection of original stratum and domain.}
}

\author{
Stephen J. Smith \email{smithsj@eastlink.ca}
}

\seealso{
See also  \code{\link{Domain.est}}, \code{\link{summary.domain.est}}
}
\examples{

SFA29D.domain<-Domain.est(SFA29D.data.2013,"GEOPHYS_ID","SDM",SFA29D.Geophysical,SFA29D.Habitat,
                 Species="Commercial")
                 
print(SFA29D.domain)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
\keyword{method}
\concept{Stratified domain estimates}
