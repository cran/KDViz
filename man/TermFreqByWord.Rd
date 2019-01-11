\name{TermFreqByWord}
\alias{TermFreqByWord}
\title{
Search a specific term inside a table of word frequencies
}
\description{
Search a specific term inside a table of word frequencies.
}
\usage{
TermFreqByWord(termFreqTable, word)
}
\arguments{
  \item{termFreqTable}{
a table of words and frequencies.
}
  \item{word}{
a string you want to search within the table of words and frequencies.
}
}
\value{
a dataframe containing a list of words which matches with the word on the search, along with its frecuencies.
}
\author{
Andres Palacios \email{anfpalacioscl@unal.edu.co}
}
\seealso{
A table of words and its frequencies inside a text corpus or a document-term matrix can be obtained through \code{\link{TermFrequency}} function.
}
\examples{
data("KDCorpus")
termFreqTable <- TermFrequency(KDCorpus)

TermFreqByWord(termFreqTable = termFreqTable, word = "graph")
}
\keyword{ word frequency }