\name{Stratify}
\alias{Stratify}
\title{
  Calculate stratified estimates
}
\description{
Selects data and calculates within strata estimates of mean and variance.
}
\usage{
Stratify(x, strata.group, strata.name, species, Subset)
}
\arguments{
\item{x }{
a data.frame containing the survey data with each row giving the information for each tow.
}
\item{strata.group }{
List object with a one vector element called Strata providing labels for stratum numbers (or names) in x and a corresponding vector element called NH providing the number of sample units by stratum.
}

\item{strata.name}{Name of the variable in x containing strata labels/numbers}

\item{species}{
name of element in x specifying what species' catches are to be analysed.

}

\item{Subset}{
expression specifying which subset of observations should be used in the fit.
This can be a logical vector (which is replicated to have length equal to the 
number of observations), a numeric vector indicating the observation numbers to be included, or a 
character vector of the observation names that should be included. All observations are included by default.
}
}
\value{
stratify returns a list of class "strata"" with required components for further analyses of the data.  If the result is unassigned, print.strata will automatically
 give means, variances, sample sizes and the stratum weight for each stratum.

\item{yhi}{the number of animals caught in tow i in strata h.}

\item{Strata}{the stratum label for each tow.}

\item{Nh}{the total number of population sample units in stratum h.}

\item{Wh}{the proportion of the area in stratum h.}

\item{nh}{the total number of sampled units in stratum h.}

\item{descrip}{the total number of sampled units in stratum h.}
}

\details{
The data object can contain survey data for more than just the strata of interest.
Selection of which strata are to be included are specified with strata.group.

Missing values (NA's) are allowed for all arguments and are automatically removed from the analysis.
}
\section{References}{
Cochran, W.G. (1977). \emph{Sampling Techniques.} John Wiley & Sons. New York, N.Y.

Smith, S.J. (1996).
Analysis of bottom trawl survey data, In  Assessment of groundfish
stocks based on bottom trawl survey results. H. Lassen (Ed.)
\emph{NAFO Scientific Council Studies} \bold{28}: 25--53.
}

\author{Stephen J. Smith, \email{smithsj@eastlink.ca}}

\seealso{
\code{\link{print.strata}},
\code{\link{summary.strata}}.
}
\examples{

Scallop.numbers<-Stratify(BoFSPA4.data,BoFSPA4.strata,"STRATA.ID",species="Commercial")

}
\keyword{htest}
\concept{Stratified estimates}
% docclass is function
% Converted by Sd2Rd version 1.21.
