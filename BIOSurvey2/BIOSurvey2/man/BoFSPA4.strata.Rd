\name{BoFSPA4.strata}
\alias{BoFSPA4.strata}
\docType{data}
\title{
Bay of Fundy scallops
}
\description{
Contains details on the strata used in the scallop survey dataset.
}
\usage{data("BoFSPA4.strata")}
\format{
  A data frame with 8 observations on the following 3 variables.
  \describe{
    \item{\code{Strata}}{Strata identification number.}
    \item{\code{NH}}{The number of sample unit size (drag width times distance towed) in each stratum.}
    \item{\code{STRATA.NAMES}}{a factor with levels \code{Centreville} \code{CV to GH} \code{Delaps Cove} \code{DG to DC} \code{Digby Gut} \code{GH to DG} \code{Gulliver's Head} \code{Parker's Cove}. These correspond to the strata names.}
  }
}
\details{
The details in this file correspond to the Scallop Producion Area 4 (SPA 4) strata used in the Bay of Fundy scallop survey.
}

\references{
 Nasmith, L., Hubley, B., Smith, S.J., and Glass, A. 2014.  Scallop Production Areas in the Bay of Fundy: Stock Status for 2013 and Forecast for 2014. \emph{DFO Can. Sci. Advis. Sec. Res. Doc}. \bold{2014/016}. vi + 139 p.
}
\examples{
data(BoFSPA4.strata)
str(BoFSPA4.strata) 
}
\keyword{datasets}
