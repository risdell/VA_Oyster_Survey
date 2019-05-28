\name{influential.strata}
\alias{influential.strata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Diagnostic plot for influential observations}
\description{
Produces a plot showing the proportion that each catch (on abscissa) contributes to the stratified mean (on ordinate).
}
\usage{
influential.strata(object, Plot = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{strata object created by \code{\link{Stratify}}.}
  \item{Plot}{logical flag: if TRUE, then influential plot is plotted on current graphics device.  Otherwise x and y coordinates for plot provided.}
  \item{\dots}{arguments to be passed onto plot function, such as xlab, xlim, etc.  The labels and limits for the y-axis are already set.}
}
\details{
 This function uses the generic function plot to produce this special plot for class strata.
}
\value{
if plot is FALSE, influential.strata returns a list with components x and y, giving coordinates of the points that would have been plotted. 
Strata labels are also included.
}
\references{
Smith, S.J. (1996).
Analysis of bottom trawl survey data, In  Assessment of groundfish
stocks based on bottom trawl survey results. H. Lassen (Ed.)
\emph{NAFO Scientific Council Studies} \bold{28}: 25--53.
}

\author{Stephen J. Smith \email{smithsj@eastlink.ca}}


\seealso{\code{\link{Stratify}},\code{\link{identify.influential.plot}}}
\examples{

\dontshow{
Scallop.numbers<-Stratify(BoFSPA4.data,BoFSPA4.strata,"STRATA.ID",species="Commercial")

}
influential.strata(Scallop.numbers,Plot=TRUE,xlab="Scallop numbers")

scallop.influential<-influential.strata(Scallop.numbers,Plot=FALSE)
}
\keyword{hplot}
\concept{Stratified estimates}
\concept{diagnostics}
