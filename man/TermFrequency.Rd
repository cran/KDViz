\name{TermFrequency}
\alias{TermFrequency}
\title{
Table of word frequencies inside a text corpus or a document-term matrix
}
\description{
Function to see the total frecuencies of terms inside a text corpus or a document-term matrix object.
}
\usage{
TermFrequency(x)
}
\arguments{
  \item{x}{
a \code{VCorpus}, \code{Corpus} or \code{DocumentTermMatrix}.
}
}
\value{
a dataframe containing a list of words and its frecuencies inside a text corpus or a document-term matrix.
}
\author{
Andres Palacios \email{anfpalacioscl@unal.edu.co}
}
\examples{
data("KDCorpus")

termFreqTable <- TermFrequency(KDCorpus)
head(termFreqTable, 100)
}
\keyword{ term frequency }
\keyword{ dtm frequency }
\keyword{ corpus frequency }