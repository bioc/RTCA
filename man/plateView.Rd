\name{plateView}
\Rdversion{1.1}
\alias{plateView}
\title{
  PLATE VIEW OF RTCA DATA
}
\description{
  Plots a \emph{E-plate} in RTCA assays in one plot to convey an
  overview of the plate
}
\usage{
plateView(rtca, ylim, titles,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rtca}{An object of \code{\linkS4class{RTCA}}}
  \item{ylim}{ylab lim}
  \item{titles}{Titles of sub-figures representing each well. If
    missing, the function seeks whether a \emph{Well} column is
    available in the pData of the RTCA object, and if so, its value will
    be used. If not, the sample names (by \code{sampleNames} function)
    will be used as titles.}
  \item{\dots}{Other parameters passed to the \code{\link{plot}} function. Currently options \code{col}, \code{lty} and \code{lwd} are supported. See
      details below.}
}
\details{
  For now the function only supports the visualization of a 96-well
  \emph{E-plate}.

  The plate view plot draws lines indicating cell index (or its
  transformations) in a birdview. When \code{...} are not specified,
  default color, line style and width are used. \code{col},\code{lty}
    and \code{lwd} can be a vector, and if needed they will be expanded
    to have the same length as wells.
}
\value{\code{NULL}, the function is called for the side effect}
\author{
  Jitao David Zhang \email{jitao_david.zhang@roche.com}
}
\seealso{
  \code{\linkS4class{RTCA}} for data structure, \code{\link{plot}} for
  the basic plot function.
}
\examples{
require(RTCA)
  
ofile <- system.file("extdata/testOutput.csv", package="RTCA")
rtca <- parseRTCA(ofile)

## Not run automatically, because of 'margin too large'
## plateView(rtca)
## plateView(rtca, lty=2)
## plateView(rtca, col=rep(1:8, each=12))

rtca.skip <- parseRTCA(ofile, maskWell="H[0-9]{2}")
## plateView(rtca.skip)
}

\keyword{hplot}
