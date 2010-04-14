\name{spectramaxImport}
\alias{spectramaxImport}
\title{
  Import output files from Spectramax spectrophotometer
}
\description{
   Import output files from Spectramax spectrophotometer (plate reader)
   into the list format compatible with the cellHTS2 package.
 }
\usage{
spectramaxImport(file, encoding="latin1")
}
\arguments{
  \item{file}{A Spectramax file}
  \item{encoding}{File character encoding, by default \dQuote{latin1}}
}
\details{
  The function imports output files from Spectramax plate reader, with
  which single-channel cell-based assays could be performed. Such assay
  includes WST-1 viability assay, which can be used to validate RTCA
  assay results.
}
\value{
A list of two items: one data frame (no name) and one character vector (\emph{txt}). The data frame contains following columns:
\item{well}{Well indices ([A-Z][0-9][0-9] format) on the microtitre plate}
\item{val}{Value of each well}
The character vector \emph{txt} contains a copy of the file contents.
}
\author{
  Jitao David Zhang <j.zhang@dkfz.de>
}
\seealso{
  cellHTS2 package documentation.
}
\examples{
wstFiles <- dir(system.file("extdata", package="RTCA"),
pattern="^WST.*csv$", full.names=TRUE)
spectramaxImport(wstFiles[1])

## NOT RUN
## spectramaxImport also supports multiple files, in which case the
## result is a list of individual lists
spectramaxImport(wstFiles)
## END NOT RUN
}
