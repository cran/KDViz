\name{DTMFromCorpus}
\alias{DTMFromCorpus}
\title{
Obtain a document-term matrix from corpus
}
\description{
Obtain a matrix, better known as document-term matrix (DTM), where rows correspond to documents and rows to terms.
}
\usage{
DTMFromCorpus(corpus, rowNames)
}
\arguments{
  \item{corpus}{
a corpus obtained from a bibliographic database.
}
  \item{rowNames}{
a list of row names for the resulting document-term matrix to bring traceability of the names of the articles from the initial database.
}
}
\details{
A quick process for obtaining a document-term matrix from a text corpus. The chosen method for weighting this matrix is the binary method, so entries of this matrix are \code{1} if the \code{i-th} term belongs to the \code{j-th} document and \code{zero} otherwise.
}
\value{
a \code{matrix} object i.e. a document-term matrix, weighted by the binary method.
}
\author{
Andres Palacios \email{anfpalacioscl@unal.edu.co}
}
\note{
If \code{rowNames} argument is not provided, article indexes inside document-term matrix are going to be renumbered.
}
\examples{
data("KDVizData")
data("KDCorpus")

myDTM <- DTMFromCorpus(corpus = KDCorpus, rowNames = row.names(KDVizData))
}
\keyword{ dtm }
\keyword{ corpus }
\keyword{ document-term matrix }