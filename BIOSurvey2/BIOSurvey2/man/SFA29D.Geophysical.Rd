\name{SFA29D.Geophysical}
\alias{SFA29D.Geophysical}
\docType{data}
\title{
Scallop Fishing Area 29D 2013 survey details (geophysical)
}
\description{
Contains details on the geophysical strata used in the scallop survey dataset.
}
\usage{data("SFA29D.Geophysical")}
\format{
  A data frame with 5 observations on the following 5 variables.
  \describe{
    \item{\code{SUBAREA}}{Names of the management subarea.}
    \item{\code{Bottom.type}}{a factor with levels \code{Cambro-Ordovician} \code{Devonian} \code{Glaciomarine-silt} \code{Ice-contact sediment} \code{Ice-contact sediment.moraine} \code{Postglacial sand and gravel} describing the 
    bottom type based on geophysical characteristics.}
    \item{\code{Area.sq.km}}{Area in square kilometers for each geophysical strata within each subarea strata.}
    \item{\code{NH}}{The number of sample unit size (drag width times distance towed) for each geophysical strata within each subarea strata.}
    \item{\code{GEOPHYS_ID}}{Names of the geophysical strata in the survey database (see \code{\link{SFA29D.data.2013}}).}
  }
}
\details{
The details in this file correspond to the Scallop Fishing Area 29D (SFA 29D) strata using geophysical based strata designed for the 2005 to 2013 surveys.
}

\references{
Sameoto, J.A., Smith, S.J., Nasmith, L.E., Glass, A., and Denton, C. 2015. Scallop Fishing
Area 29: Stock Status and Update for 2015. \emph{DFO Can. Sci. Advis. Sec. Res. Doc}. \bold{2015/067}:
v + 69 p.
}
\examples{
data(SFA29D.Geophysical)

str(SFA29D.Geophysical) 
}
\keyword{datasets}
