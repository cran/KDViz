\name{KDSummary}
\alias{KDSummary}
\title{
Knowledge domain summary
}
\description{
Shows the main information about the grouping of words found through mpa method. 
}
\usage{
KDSummary(matrix.mpa, groupSize = 10, graph = TRUE)
}
\arguments{
  \item{matrix.mpa}{
a list of co-occurrences and associations matrices obtained with \code{\link{matrix.corpus.mpa}} function.
}
  \item{groupSize}{
an integer that indicates the desired number of words that should belong to each group (may vary when grouping the words).
}
  \item{graph}{
logical. If \code{TRUE}, the main graph of mpa method will be plotted using \code{diagram.mpa} function from \code{mpa} package.
}
}
\details{
\code{groupSize} argument corresponds to the size when create the groups, but it can vary according to the level of association and the amount of co-occurrences of the words.
}
\value{
A list containing:
\item{clustTable}{a dataframe with the main information of the groups found.}
\item{wordClasses}{a dataframe with words whose frequency is greater than or equal to that indicated in \code{matrix.mpa} argument and an indicator of the group of words to which it belongs.}
}
\author{
Andres Palacios \email{anfpalacioscl@unal.edu.co}
}
\seealso{
\code{mpa} function from \code{mpa} package provides the grouping of words.
}
\examples{
data("KDCorpus")
mpaWords <- matrix.corpus.mpa(KDCorpus, fmin = 10, cmin = 1)

myKDSummary <- KDSummary(matrix.mpa = mpaWords, groupSize = 10)
}
\keyword{ kdviz }
\keyword{ knowledge domain }
\keyword{ mpa matrix }