\name{Domain.est}
\alias{Domain.est}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stratified domain estimates
}
\description{
Calculates domain mean and variances for stratified design
}
\usage{
Domain.est(x, Strata, Domain, strata.obj, domain.obj, Species)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a data.frame containing the survey data with each row giving the information for each tow including old and new strata.

}
  \item{Strata}{
Name of the variable in x containing labels/numbers for original strata.
}
  \item{Domain}{
Name of the variable in x containing labels/numbers for new strata (domain).
}
  \item{strata.obj}{
List object with a one vector element providing labels for original stratum numbers (or names) in x and a corresponding vector element called NH providing the number of sample units for each of the original strata. See \code{\link{SFA29D.Geophysical}}
}
  \item{domain.obj}{
List object with a one vector element providing labels for new stratum numbers (or names) in x and a corresponding vector element called NH providing the number of sample units for each of the new strata. See \code{\link{SFA29D.Habitat}}
}
  \item{Species}{
name of element in x specifying what species' catches are to be analysed.
}

}
\details{
Domain estimates are used when calculating estimates of mean and variance for data originally collected with one stratification scheme with a new stratification scheme where old and new strata overlap. If all of the original observations have the same probability of being included in the sample and the old and new strata boundaries do not overlap then a post-stratification estimate should be used.  See \code{\link{post.stratify}}.
}
\value{
\item{Strata}{Name of the original stratification variable.}
 \item{Domain}{Name of the new stratification variable.}
\item{ybd}{Mean for each new strata (domain).}
 \item{var.ybd}{Variance of the mean for each new strata (domain).}
\item{var.diffdomain}{Penalty term in varance for differences between the strata means in the domain.}
 \item{se.ybd}{Standard error of the mean for each new strata (domain).}
\item{Nd}{Number of sample units for each domain.}
 \item{nsdh}{Sample size for each intersection of original stratum and domain.}
\item{nh}{Sample size for each original stratum.}
 \item{pdh}{Ratio of nsdh to nh.}
\item{Nh}{Number of sample units for each original stratum}
\item{domain.obj}{Strata details for domain. See Arguments above.}
}
\references{
Sarndal, C. E., Swenson, B., and Wretman, J. 1992. Model assisted
survey sampling. Springer, New York.
}
\author{
Stephen J. Smith \email{smithsj@eastlink.ca}
}

\seealso{
See also  \code{\link{print.domain.est}}, \code{\link{summary.domain.est}}
}
\examples{
SFA29D.domain<-Domain.est(SFA29D.data.2013,"GEOPHYS_ID","SDM",SFA29D.Geophysical,SFA29D.Habitat,
                 Species="Commercial")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}% use one of  RShowDoc("KEYWORDS")
\concept{domain estimate}% __ONLY ONE__ keyword per line
