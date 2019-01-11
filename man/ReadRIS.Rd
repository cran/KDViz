\name{ReadRIS}
\alias{ReadRIS}
\title{
Function to read RIS files
}
\description{
Read the entire info from a RIS file and convert it in a dataframe object.
}
\usage{
ReadRIS(risFile, saveCSV = FALSE, exportName)
}
\arguments{
  \item{risFile}{
a \code{RIS} extension file.
}
  \item{saveCSV}{
logical. If \code{TRUE}, a \code{CSV} file of the obtained dataframe will be saved.
}
  \item{exportName}{
a character string naming the \code{CSV} file that is going to be saved.
}
}
\value{
a data frame of article and its bibliographic information.
}
\author{
Andres Palacios \email{anfpalacioscl@unal.edu.co}
}
\examples{
risData <- ReadRIS(risFile = system.file("extdata", "ScienceDirectRIS.ris",
  package = "KDViz"), saveCSV = FALSE)
}
\keyword{ ris file }
\keyword{ read ris }
\keyword{ file extension }