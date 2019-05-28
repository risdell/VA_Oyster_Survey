\name{print.assoc}
\alias{print.assoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Print method for class assoc}
\description{
  Print method for result from \code{Association.test}
}
\usage{
\method{print}{assoc}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{assoc object created by \code{Association.test}.}
	\item{...}{further arguments passed to or from other methods.}
}
\value{
  Prints out details of association test contained in class "assoc" object result.
}

\author{Stephen J. Smith \email{smithsj@eastlink.ca}}


\seealso{\code{\link{Association.test}}}
\examples{
\dontrun{print(Scallop.numbers.assoc)}

\dontrun{Scallop.numbers.assoc}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
\concept{Stratified estimates}
\concept{Association with environmental data}
