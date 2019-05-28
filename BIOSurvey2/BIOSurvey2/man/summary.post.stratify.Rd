\name{summary.post.stratify}
\alias{summary.post.stratify}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary  method for post.stratify objects
}
\description{
Provides postratified mean and variance over old strata
}
\usage{
\method{summary}{post.stratify}(object,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object created by \code{\link{post.stratify}}
}
	\item{...}{further arguments passed to or from other methods.}
}


\details{
Summary method for class "post.stratify".
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
 \item{object}{Post-stratification estimates for each of the old strata.}
 \item{ystr.mean}{Stratified mean over all of old strata.}
  \item{V.mean}{Variance of stratified mean over all of old strata.}
%% ...
}

\author{
Stephen J. Smith \email{smithsj@eastlink.ca}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \code{\link{post.stratify}}
}
\examples{
\dontshow{
SFA292002<-post.stratify(SFA29.data.2002,"STRATA","GEOPHYS_ID",SFA29.Geophysical,species="Commercial")

}

summary(SFA292002)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}% use one of  RShowDoc("KEYWORDS")
\concept{post-stratification}% __ONLY ONE__ keyword per line
