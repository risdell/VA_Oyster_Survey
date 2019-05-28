\name{BoFSPA4.data}
\alias{BoFSPA4.data}
\docType{data}
\title{
Bay of Fundy Scallop Survey Data
}
\description{
Data from the 2008 Bay of Fundy scallop survey
}
\usage{data("BoFSPA4.data")}
\format{
  A data frame with 105 observations on the following 10 variables.
  \describe{
    \item{\code{CRUISE}}{cruise identification.}
    \item{\code{YEAR}}{year of survey.}
    \item{\code{TOW.NO}}{tow number.}
    \item{\code{TOW.DATE}}{dates for the individual tows.}
    \item{\code{STRATA.ID}}{strata identification number.}
    \item{\code{STRATA}}{a factor with levels \code{CV to GH}, \code{Centreville}, \code{DG to DC}, \code{Delaps Cove}, \code{Digby Gut}, \code{GH to DG}, \code{Gulliver's Head}, \code{Parker's Cove}. These correspond to the strata names.}
    \item{\code{DEPTH}}{depth of tow in metres.}
    \item{\code{Pre.recruits}}{numbers of scallops with shell heights less than 65 mm.}
    \item{\code{Recruits}}{numbers of scallops with shell heights between 65 and 80 mm.}
    \item{\code{Commercial}}{numbers of scallops with shell heights greater than or equal to 80 mm.}
  }
}
\details{
The details in this file correspond to the Scallop Production Area 4 (SPA 4) survey data from the 2008 Bay of Fundy scallop survey.
}

\references{
 Nasmith, L., Hubley, B., Smith, S.J., and Glass, A. 2014.  Scallop Production Areas in the Bay of Fundy: Stock Status for 2013 and Forecast for 2014. \emph{DFO Can. Sci. Advis. Sec. Res. Doc}. \bold{2014/016}. vi + 139 p.
}
\examples{
\dontrun{
data(BoFSPA4.data)

str(BoFSPA4.data)
}}
\keyword{datasets}
