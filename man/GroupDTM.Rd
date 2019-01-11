\name{GroupDTM}
\alias{GroupDTM}
\title{
Grouping of words with co-word method
}
\description{
Make partitions of an entire document-term matrix depending on the group class of words found using the KDSummary function.
}
\usage{
GroupDTM(dtm, kdSummary)
}
\arguments{
  \item{dtm}{
a document-term matrix.
}
  \item{kdSummary}{
a list resulting from \code{\link{KDSummary}} function.
}
}
\value{
a list of matrices derivative from an entire document-term matrix.
}
\author{
Andres Palacios \email{anfpalacioscl@unal.edu.co}
}
\seealso{
For more information, see \code{\link{KDSummary}}.
}
\examples{
data("KDCorpus")
data("KDDTM")
mpaWords <- matrix.corpus.mpa(KDCorpus, fmin = 10, cmin = 1)
myKDSummary <- KDSummary(matrix.mpa = mpaWords, groupSize = 10)

myWordGroupDTM <- GroupDTM(dtm = KDDTM, kdSummary = myKDSummary)
}
\keyword{ dtm }
\keyword{ word grouping }
