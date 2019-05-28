\name{identify.influential.plot}
\alias{identify.influential.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Indentifies influential observations}
\description{
  Identifies observations that contribute disproportionately to the stratified mean.
}
\usage{
\method{identify}{influential.plot}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{object produced by \code{influential.strata}}
\item{\dots}{further arguments passed to \code{\link{par}} such as \code{cex}, \code{col} and \code{font}.}
}
\details{
  This function uses \code{identify} to label points using the curser. Point labels refers to Strata where 
observation is located.
}
\value{
  Points labelled on plot. An integer vector containing the indexes of
     the identified points.
}

\author{Stephen J. Smith \email{smithsj@eastlink.ca}}


\seealso{\code{\link{influential.strata}}, \code{\link{identify}}}
\examples{
\dontrun{identify.influential.plot(scallop.influential)}
  }

\keyword{hplot}
\concept{Stratified estimates}
\concept{diagnostics}
