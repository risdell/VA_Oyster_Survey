\name{SFA29.data.2002}
\alias{SFA29.data.2002}
\docType{data}
\title{
Scallop Fishing Area 29 2002 Survey Data 
}
\description{
Data from the 2002 Scallop Fishing Area 29 scallop survey
}
\usage{data("SFA29.data.2002")}
\format{
  A data frame with 125 observations on the following 15 variables.
  \describe{
    \item{\code{CRUISE}}{cruise identification.}
    \item{\code{VESSEL_NAME}}{name of fishing vessel used for survey.}
    \item{\code{YEAR}}{a character vector indicating year of survey.}
        \item{\code{TOW_NO}}{tow number.}
    \item{\code{TOW_DATE}}{dates for the individual tows.}
   \item{\code{STRATA_ID}}{numerical code for strata.}
    \item{\code{STRATA}}{a factor with levels \code{SFA29A}, \code{SFA29B}, \code{SFA29C,} \code{SFA29D} corresponding to strata names based on management subareas.}
    \item{\code{GEOPHYS_ID}}{a factor with levels \code{COM}, \code{DG}, \code{GM}, \code{IC1}, \code{PG} corresponding to strata based on geophysical characteristics.}
    \item{\code{DEPTH}}{depth of tow in metres.}
    \item{\code{SDM}}{an ordered factor with levels \code{Low} < \code{Medium} < \code{High} corresponding to strata based on scallop habitat characteristics predicted by species distribution modelling (SDM).}
    \item{\code{Commercial}}{numbers of scallops with shell heights greater than or equal to 100 mm.}
    \item{\code{Recruits}}{numbers of scallops with shell heights between 90 and 99 mm.}
    \item{\code{Pre.recruits}}{numbers of scallops with shell heights less than 90 mm.}

  }
}
\details{
The details in this file correspond to the Scallop Fishing Area 29 (SFA 29) survey data from the 2002 scallop survey
originally designed using subarea strata.
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
data(SFA29.data.2002)

str(SFA29.data.2002) 
}
\keyword{datasets}
