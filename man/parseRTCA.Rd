\name{parseRTCA}
\Rdversion{1.1}
\alias{parseRTCA}
\title{
  PARSE RTCA OUTPUT FILE
}
\description{
  The function parses RTCA output file into \code{RTCA} object
}
\usage{
parseRTCA(file, dec = ".", phenoData, maskWell, ...)
}
\arguments{
  \item{file}{character, name of the RTCA output file}
  \item{dec}{decimal sign of the file}
  \item{phenoData}{phenoData}
  \item{maskWell}{character, either names or regular expression
    pattern(s) for well(s) to mask}
  \item{\dots}{other parameters passed to \code{\link{read.table}}}
}
\details{
  A csv-like format file can be exported from the RTCA device, which can
  be fed into this function to set up an instance of
  \code{\linkS4class{RTCA}} object.

  In the \emph{/extdata/} directory of the package, such a file is
  provided as an example. The first line contains the experiment ID,
  which is followed by a matrix of recorded data in the tabular
  form. The first and second column records the time-interval in the
  unit of hour and hour-minute-second format respectively. The rest
  columns then record the read-out (\sQuote{Cell-Index}, or \sQuote{CI})
  of the device, with each well a role.

  \code{phenoData} allows user to annotate the wells.Its usage mimicks
  the \code{ExpressionSet} object in the \code{Biobase} package.

  \code{maskWell} allows to mask wells in case, for example, they are known to be
  contaminated. The values can be either a vector of well names, or a
  regular expression pattern for wells to be masked. To learn
  regular expression patterns see \code{\link{grep}}.

}
\value{
  An object of \code{RTCA-class}
}
\references{
\url{http://www.roche-applied-science.com/proddata/gpip/3_8_9_1_1_1.html}
}
\author{
  Jitao David Zhang \email{jitao_david.zhang@roche.com}
}
\examples{
require(RTCA)
  
ofile <- system.file("extdata/testOutput.csv", package="RTCA")
pfile <- system.file("extdata/testOutputPhenoData.csv", package="RTCA")

pData <- read.csv(pfile, sep="\t", row.names="Well")
metaData <- data.frame(labelDescription=c(
"Rack number",
"siRNA catalogue number",
"siRNA gene symbol",
"siRNA EntrezGene ID",
"siRNA targeting accession"
))

phData <- new("AnnotatedDataFrame", data=pData, varMetadata=metaData)
x <- parseRTCA(ofile, phenoData=phData)

print(x)

## mask wells, e.g. due to unusual values
x.skip <- parseRTCA(ofile, phenoData=phData, maskWell=c("D09"))
x.skip.multiWells <- parseRTCA(ofile, phenoData=phData, maskWell=c("A01", "B01",
"C02"))
## skip the last row
x.skip.pattern <- parseRTCA(ofile, phenoData=phData,
maskWell=c("H[0-9]{2}"))

## check the number of masked wells
noMasked <- function(x) sum(apply(x, 2, function(x) all(is.na(x))))
noMasked(exprs(x))
noMasked(exprs(x.skip))
noMasked(exprs(x.skip.multiWells))
noMasked(exprs(x.skip.pattern))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
\keyword{ file }

