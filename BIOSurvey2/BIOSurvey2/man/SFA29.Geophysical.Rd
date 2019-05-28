\name{SFA29.Geophysical}
\alias{SFA29.Geophysical}
\docType{data}
\title{
Scallop Fishing Area 29 2002 survey details (geophysical)
}
\description{
Contains details on the geophysical strata used for the scallop survey dataset.
}
\usage{data("SFA29.Geophysical")}
\format{
  A data frame with 25 observations on the following 5 variables.
  \describe{
    \item{\code{STRATA}}{Names of the original strata based on management subareas.}
    \item{\code{Bottom.type}}{a factor with levels \code{Cambro-Ordovician} \code{Devonian} \code{Glaciomarine-silt} \code{Ice-contact sediment} \code{Ice-contact sediment.moraine} \code{Postglacial sand and gravel} describing the 
    bottom type based on geophysical characteristics.}
    \item{\code{Area.sq.km}}{Area in square kilometers for each geophysical strata within each subarea strata.}
    \item{\code{NH}}{The number of sample unit size (drag width times distance towed) for each geophysical strata within each subarea strata.}
    \item{\code{GEOPHYS_ID}}{Names of the geophysical strata in the survey database (see \code{\link{SFA29.data.2002}}).}
  }
}
\details{
The details in this file correspond to the Scallop Fishing Area 29 (SFA 29) strata used to develop post-stratification
estimates using geophysical based strata for the 2001 to 2004 surveys which were originally designed with subarea strata.
}

\references{
Sameoto, J.A., Smith, S.J., Nasmith, L.E., Glass, A., and Denton, C. 2015. Scallop Fishing
Area 29: Stock Status and Update for 2015. \emph{DFO Can. Sci. Advis. Sec. Res. Doc}. \bold{2015/067}:
v + 69 p.
}
\examples{
data(SFA29.Geophysical)

str(SFA29.Geophysical) 
}

\keyword{datasets}
\concept{post-stratification}
