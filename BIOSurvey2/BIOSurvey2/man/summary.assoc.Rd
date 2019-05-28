\name{summary.assoc}
\alias{summary.assoc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summary of an assoc object}
\description{
  This function is a method for the generic function summary for class assoc.
  }
\usage{
\method{summary}{assoc}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{assoc object created by \code{Association.test}.}
	\item{...}{further arguments passed to or from other methods.}
}

\value{
  Prints out details of association test contained in class "assoc" object result.
}
\references{
Perry, R.I. and S.J. Smith. (1994). 
Identifying habitat associations of marine fishes using survey data:  an application to the Northwest Atlantic. 
\emph{Can. J. Fish. Aquat. Sci.}  \bold{51:} 589--602.

Smith, S.J. (1996). Analysis of data from bottom trawl surveys. In Assessment of groundfish stocks based on bottom trawl survey results. 
Lassen, H. (ed.). \emph{NAFO Scientific Council Studies.} \bold{28:} 25--53.
}

 \author{Stephen J. Smith \email{smithsj@eastlink.ca}}

\seealso{\code{\link{Association.test}}}
\examples{\dontrun{summary{Scallop.numbers.assoc}}
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
\concept{Stratified estimates}
\concept{Association with environmental data}
