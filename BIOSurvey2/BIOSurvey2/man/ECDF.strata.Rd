\name{ECDF.strata}
\alias{ECDF.strata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot empirical cumulative distribution plot}
\description{
Constructs a empirical cumulative distribution plot for a strata object
}
\usage{
ECDF.strata(object, Plot = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{strata object created by \code{\link{Stratify}}.}
  \item{Plot}{logical flag: if TRUE, then cumulative distribution is plotted on current graphics device.  Otherwise x and y coordinates for plot provided.}
  \item{\dots}{arguments to be passed onto plot function, such as xlab, xlim, etc.  The labels and limits for the y-axis are already set.}
}

\value{
  if plot is FALSE, quantile.strata returns a list with components x and y, giving coordinates of the points that would have been plotted. 
  }
  \details{This function uses the generic function plot to produce this special plot for class strata.  
  The empirical cumulative distribution function is derived using the ratio of the stratum weight and sample size as the weight for each observation.}
  
  \references{
  Chambers, R. L. and R. Dunstan. (1986). 
  Estimating distribution functions from survey data. 
  \emph{Biometrika}. \bold{73}: 597--604.
  }

\author{Stephen J. Smith \email{smithsj@eastlink.ca}}


\seealso{\code{\link{Stratify}}}
\examples{
\dontshow{
Scallop.numbers<-Stratify(BoFSPA4.data,BoFSPA4.strata,"STRATA.ID",species="Commercial")
}
ECDF.strata(Scallop.numbers,Plot=TRUE,xlab="Scallop numbers")
}
\keyword{hplot}
\concept{Stratified estimates}
\concept{diagnostics}
