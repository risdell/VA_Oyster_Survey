\name{qqnorm.boot}
\alias{qqnorm.boot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Quantile-quantile plot for bootstrap data}
\description{
  Produce a quantile-quantile plot for bootstrap data from boot objects.
}
\usage{
\method{qqnorm}{boot}(y, ylab = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{boot object created by \code{\link{boot.strata}}}
  \item{ylab}{text for ylab of plot}
	\item{...}{further arguments passed to or from other methods.}
}
\details{
 Quantile-quantile plot of bootstrap estimates is produced on current graphics device.
}

\references{
Smith, S.J. (1996). 
Analysis of data from bottom trawl surveys. In Assessment of groundfish stocks based on bottom trawl survey results. 
Lassen, H. (ed.). \emph{NAFO Scientific Council Studies.} \bold{28}: 25--53.
}

 \author{Stephen J. Smith \email{smithsj@eastlink.ca}}

\seealso{\code{\link{boot.strata}}
}
\examples{\dontrun{
qqnorm.boot(Scallop.numbers.boot,ylab="Stratified mean")
}}
\keyword{hplot}
\concept{Bootstrap}
\concept{Stratified estimates}
\concept{Quantile plots}
