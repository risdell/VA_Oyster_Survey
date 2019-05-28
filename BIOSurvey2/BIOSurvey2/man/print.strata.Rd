\name{print.strata}
\alias{print.strata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Print method for strata object}

\description{
Prints out stratum details such as sample size, stratum weight, mean and standard error.
}
\usage{
\method{print}{strata}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{strata object created by \code{\link{Stratify}}}
	\item{...}{further arguments passed to or from other methods.}
}

\details{
  this function is a method for the generic function print for class "strata".
}
\value{
  Table of summary details for each stratum.
}


 \author{Stephen J. Smith \email{smithsj@eastlink.ca}}
 
\seealso{
\code{\link{Stratify}},
\code{\link{summary.strata}}.
}
\examples{
\dontrun{print(Scallop.numbers)}

\dontrun{Scallop.numbers}
}
\keyword{print}
\concept{Stratified estimates}
