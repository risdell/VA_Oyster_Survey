\name{SFA29.Habitat}
\alias{SFA29.Habitat}
\docType{data}
\title{
Scallop Fishing Area 29 2002 survey details (habitat)
}
\description{
Contains details on the habitat strata used for the scallop survey dataset.
}
\usage{data("SFA29.Habitat")}
\format{
  A data frame with 12 observations on the following 4 variables.
  \describe{
    \item{\code{STRATA}}{Names of the original strata based on management subareas.}
    \item{\code{Area.sqkm}}{Area in square kilometers for each habitat strata within each subarea strata.}
    \item{\code{NH}}{The number of sample unit size (drag width times distance towed) for each habitat strata within each subarea strata.}
    \item{\code{SDM}}{Names of the habitat strata in the survey database (see \code{\link{SFA29.data.2002}} reflecting scallop habitat suitability as determined by species distributional modelling (SDM).}
  }
}
\details{
The details in this file correspond to the Scallop Fishing Area 29 (SFA 29) strata used to develop post-stratification
estimates using habitat based strata for the 2001 to 2004 surveys which were originally designed with subarea strata.
}

\references{

Brown, C. J., Sameoto, J. A. and Smith, S. J. 
2012. Multiple methods, maps, and management applications: purpose
made seafloor maps in support of ocean management. \emph{Journal of Sea Research}. \bold{72}:
1-13.

Sameoto, J.A., Smith, S.J., Nasmith, L.E., Glass, A., and Denton, C. 2015. Scallop Fishing
Area 29: Stock Status and Update for 2015. \emph{DFO Can. Sci. Advis. Sec. Res. Doc}. \bold{2015/067}:
v + 69 p.
}

\examples{
data(SFA29.Habitat)

str(SFA29.Habitat)
}
\keyword{datasets}
