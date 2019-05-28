\name{summary.domain.est}
\alias{summary.domain.est}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary  method for domain.est objects
}
\description{
Provides stratified domain mean and variance estimates
}
\usage{
\method{summary}{domain.est}(object,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object created by \code{\link{Domain.est}}
}
	\item{...}{further arguments passed to or from other methods.}
}


\details{
Summary method for class "domain.est".
}
\value{
 \item{[1]}{Estimates by domain.}
  \item{sum.Nd}{Total number of sample units overall domain levels.}
    \item{yst}{Stratified domain mean.}
    \item{var.yst}{Variance of stratified domain mean.}


}

\author{
Stephen J. Smith \email{smithsj@eastlink.ca}
}

\seealso{
See also  \code{\link{Domain.est}}, \code{\link{print.domain.est}}
}
\examples{
SFA29D.domain<-Domain.est(SFA29D.data.2013,"GEOPHYS_ID","SDM",SFA29D.Geophysical,SFA29D.Habitat,
                 Species="Commercial")
                 
summary(SFA29D.domain)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{summary}
\keyword{methods}
\concept{Stratified domain estimates}
