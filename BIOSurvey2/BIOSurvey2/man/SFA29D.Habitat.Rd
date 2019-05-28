\name{SFA29D.Habitat}
\alias{SFA29D.Habitat}
\docType{data}
\title{
Scallop Fishing Area 29 2013 survey details (habitat)
}
\description{
Contains details on the habitat strata used for the scallop survey dataset.
}
\usage{data("SFA29D.Habitat")}
\format{
  A data frame with 3 observations on the following 4 variables.
  \describe{
    \item{\code{STRATA}}{Names of the management subareas}
    \item{\code{Area.sqkm}}{Area in square kilometers for each habitat strata within each subarea strata.}
    \item{\code{NH}}{The number of sample unit size (drag width times distance towed) for each habitat strata within each subarea strata.}
    \item{\code{SDM}}{Names of the habitat strata in the survey database (see \code{\link{SFA29D.data.2013}} reflecting scallop habitat suitability as determined by species distributional modelling (SDM).}}
}
\details{
The details in this file correspond to the Scallop Fishing Area 29D (SFA 29D) habitat strata for strata designed for the 2014 to 2016 surveys. These details are used to develop domain estimates (see \cite{\link{Domain.est}}) for surveys conducted in 2005 to 2013 which were designed using geophysical strata.
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
data(SFA29D.Habitat)

str(SFA29D.Habitat) 
}
\keyword{datasets}
