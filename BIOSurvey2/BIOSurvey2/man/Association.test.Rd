\name{Association.test}
\alias{Association.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test association between catch and covariate}
\description{
  This function produces a randomization test incorporating strata weights for associating survey catches with coincidental observations on environmental variables.
}
\usage{
Association.test(x, hydro, strata.group, strata.name, species, nreps = 0, 
method = c("KS-Test", "CVM-Test")[1],Subset)
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
  \item{nreps}{number of replications used to form null hypothesis distribution.}
  \item{method}{test method to be used.  Choices are modifications of Kolmogorov-Smirnov type test or Cramer von-Mises type test. The former test is the default.
}
  \item{Subset}{expression specifying which subset of observations should be used in the fit. This can be a logical vector (which is replicated to have length equal to the 
number of observations), a numeric vector indicating the observation numbers to be included, or a character vector of the observation names that should be included. 
All observations are included by default.}
}
\details{
  WARNING!  Although, the randomization algorithms has been written as efficiently as possible they are still written in R code and may take a few minutes. 
  An implementation using C code with a dynamic load will be considered for a future version.
}
\value{
an object of class assoc is produced which contains the following items.

\item{test.stat}{test statistic for method chosen.}
\item{test.stat.rep}{vector containing a test statistic value for each replication of the randomization test.  These values comprise the null hypothesis distribution.}
\item{p.level}{p level for the test statistic compared to the distribution in test.stat.rep.}
\item{prefer.lab}{label containing the type of test chosen.}
\item{prefer2.lab}{label for summary.assoc.} 
\item{prefer3.lab}{label for summary.assoc.}
\item{descrip}{label indicating that this object contains the result of an ``Association Analysis''.}
}

\references{
Perry, R.I. and S.J. Smith. (1994). 
Identifying habitat associations of marine fishes using survey data:  an application to the Northwest Atlantic. 
\emph{Can. J. Fish. Aquat. Sci.}  \bold{51:} 589--602.

Smith, S.J. (1996). Analysis of data from bottom trawl surveys. In Assessment of groundfish stocks based on bottom trawl survey results. 
Lassen, H. (ed.). \emph{NAFO Scientific Council Studies.} \bold{28:} 25--53.
}
\author{Stephen J. Smith, \email{stephen.smith@dfo-mpo.gc.ca}}



\seealso{\code{\link{Stratify}}, \code{\link{summary.assoc}}}
\examples{


Scallop.numbers.assoc<-Association.test(BoFSPA4.data,hydro="DEPTH",strata.group=BoFSPA4.strata, 
strata.name="STRATA.ID", species="Commercial", nreps=1000, method="CVM-Test")

}
\keyword{htest}
\concept{Stratified estimates}
\concept{Association with environmental data}
