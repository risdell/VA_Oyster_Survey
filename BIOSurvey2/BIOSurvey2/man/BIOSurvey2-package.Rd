\name{BIOSurvey2-package}
\alias{BIOSurvey2-package}
\alias{BIOSurvey2}
\docType{package}
\title{
\packageTitle{BIOSurvey2}
}
\description{
\packageDescription{BIOSurvey2}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{BIOSurvey2}
\packageIndices{BIOSurvey2}
This package contains estimation and analysis functions for stratified random survey data based on an SPLUS library described in Smith (1996).  These functions were converted to R for the package NMFSsurvey which was developed for NOAA staff in Woods Hole, MA in 2006 and subsequently modified for the package BIOSurvey used by myself and others in DFO. The current package retains all of the functions for BIOSurvey and adds to them functions for post-stratification and domain estimates.
}
\author{
\packageAuthor{BIOSurvey2}

Maintainer: \packageMaintainer{BIOSurvey2}
}
\references{
Cochran, W.G. (1977). \emph{Sampling Techniques.} John Wiley & Sons. New York, N.Y.

Smith, S. J. (1996).  Analysis of bottom trawl survey data, In  Assessment of groundfish
stocks based on bottom trawl survey results. H. Lassen (Ed.) \emph{NAFO
Scientific Council Studies}. \bold{28}:~25--53.

Smith, S.J. and S. Gavaris. (1993). 
Improving the precision of fish abundance estimates of Eastern Scotian Shelf cod from bottom trawl surveys. 
\emph{North American Journal of Fisheries Management}. \bold{13}: 35--47.


Sukhatme, R.V., and B.V. Sukhatme.  (1970). \emph{Sampling theory of surveys
with applications.} Iowa State University Press, Ames, Iowa.
}

\keyword{ package }
\seealso{
\code{\link{Stratify}},
 \code{\link{print.strata}},
\code{\link{summary.strata}}.
}
\examples{
Scallop.numbers<-Stratify(BoFSPA4.data,BoFSPA4.strata,"STRATA.ID",species="Commercial")
}
