\name{Association.plot}
\alias{Association.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots association between catch and covariate}
\description{
  Produces an association plot using one environmental variable for stratified random survey data.
}
\usage{
Association.plot(x, hydro, strata.group, strata.name, species = NULL,Subset, plot = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
\item{x }{
a data.frame containing the survey data with each row giving the information for each tow.
}

\item{hydro}{character name of vector specifying hydrographic variable to be analysed}

\item{strata.group }{
List object with a one vector element called Strata providing labels for stratum numbers (or names) in data.obj and a corresponding vector element called NH providing the number of sample units by stratum.
}

\item{strata.name}{Name of the variable in x containing strata labels/numbers}

\item{species}{
name of element in x specifying what species' catches are to be analysed. The default is the hydrographic variable named in hydro= argument.}



\item{Subset}{expression specifying which subset of observations should be used in the fit.
This can be a logical vector (which is replicated to have length equal to the 
number of observations), a numeric vector indicating the observation numbers to be included, or a 
character vector of the observation names that should be included. All observations are included by default.
}
  \item{plot}{logical flag: if TRUE, then quantile-quantile plot is plotted on current graphics device.  Otherwise x and y coordinates for plot provided.}
  \item{\dots}{arguments to be passed onto plot function, such as xlab, xlim, etc.}
}


\details{
  See example below for producing an association plot comparing fish catch and a hydrographic variable.
}
\value{
   if plot is FALSE, quantile.strata returns a list with components x and y, giving coordinates of the points that would have been plotted. 
}
\references{

Perry, R.I. and S.J. Smith. (1994). 
Identifying habitat associations of marine fishes using survey data:  an application to the Northwest Atlantic. 
\emph{Can. J. Fish. Aquat. Sci.}  \bold{51:} 589--602.

Smith, S.J., R.L. Losier, F.H. Page and K. Hatt. (1994).  
Associations between haddock, and temperature, salinity and depth  within the Canadian groundfish bottom trawl surveys (1970--1993) conducted in  
NAFO Divisions 4VWX and 5Z. Can.  \emph{Tech. Rep. Fish. Aquat. Sci.} \bold{1959:} vi + 70 p.

Smith, S.J. (1996). Analysis of data from bottom trawl surveys. In Assessment of groundfish stocks based on bottom trawl survey results. 
Lassen, H. (ed.). \emph{NAFO Scientific Council Studies.} \bold{28:} 25--53.
 }
 
 \author{Stephen J. Smith \email{smithsj@eastlink.ca}}


\seealso{\code{\link{Stratify}}, \code{\link{Association.test}}}
\examples{
#plot cumulative frequency curve for Depth.


Association.plot(BoFSPA4.data, hydro="DEPTH", strata.group=BoFSPA4.strata, strata.name="STRATA.ID",
xlab="Depth (m)",ylim=c(0,1))

#add line for Commercial size scallops and Depth.

lines(Association.plot(BoFSPA4.data, hydro="DEPTH", strata.group=BoFSPA4.strata, 
strata.name="STRATA.ID",species="Commercial",plot=FALSE),lty=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\concept{Stratified estimates}
\concept{Association with environmental data}
