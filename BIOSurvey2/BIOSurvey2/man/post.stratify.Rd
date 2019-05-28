\name{post.stratify}
\alias{post.stratify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Post stratification estimate
}
\description{
Calculates post-stratified mean and variance
}
\usage{
post.stratify(x, Strata.old, Strata.new, strata.new.obj, species)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a data.frame containing the survey data with each row giving the information for each tow including old and new strata.
}
  \item{Strata.old}{
Name of the variable in x containing labels/numbers for old strata.
}
  \item{Strata.new}{
Name of the variable in x containing labels/numbers for new strata.

}
  \item{strata.new.obj}{
List object with a one vector element providing labels for old stratum numbers (or names) in x and a corresponding vector element called NH providing the number of sample units by old stratum for each of the new strata. See \code{\link{SFA29.Habitat}}.

}
  \item{species}{
name of element in x specifying what species' catches are to be analysed.
}
}
\details{
Post-stratification is valid if all of the original observations have the same probability of being included in the sample and the old and new strata boundaries do not overlap. If strata boundaries overlap resulting different probabilities of being included then a domain estimate should be used.  See \code{\link{Domain.est}}.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{Strata}{Name of old strata.}
\item{Nh}{Number sample units for old strata}
\item{ybpos}{Post-stratification estimate of mean.}
\item{Vst.ybpos}{Variance of post-stratification estimate of mean.}
\item{Random.n.penalty}{Amount variance increased due to random sample size.}
%% ...
}
\references{
Cochran, W.G. (1977). \emph{Sampling Techniques.} John Wiley & Sons. New York, N.Y.

}
\author{
Stephen J. Smith \email{smithsj@eastlink.ca}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{summary.post.stratify}}
}
\examples{
SFA292002<-post.stratify(SFA29.data.2002,"STRATA","GEOPHYS_ID",SFA29.Geophysical,species="Commercial")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}% use one of  RShowDoc("KEYWORDS")
\concept{post-stratification}% __ONLY ONE__ keyword per line
