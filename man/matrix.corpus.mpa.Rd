\name{matrix.corpus.mpa}
\alias{matrix.corpus.mpa}
\title{
Calulation of co-occurrences matrix and matrix associations from a corpus
}
\description{
Complement for calculating co-ocurrences matrix and association matrix from a text corpus, using internally the function \code{matriz.mpa} from \code{mpa} package.
}
\usage{
matrix.corpus.mpa(corpus, fmin = 3, cmin = 3)
}
\arguments{
  \item{corpus}{
a corpus.
}
  \item{fmin}{
minimum frequency of appearance of key words within the corpus.
}
  \item{cmin}{
minimum match amount between words.
}
}
\value{
A list containing:
\item{MatrixA}{Associations matrix.}
\item{MatrixC}{Co-occurrence matrix.}
\item{Words}{Vector from the different words that appears in the corpus (dictionary).}
\item{lt}{Lexical table.}
}
\author{
Andres Palacios \email{anfpalacioscl@unal.edu.co}
}
\seealso{
\code{matriz.mpa} function from mpa is used to obtain the composition of word clusters.
}
\examples{
data("KDCorpus")

mpaWords <- matrix.corpus.mpa(KDCorpus, fmin = 10, cmin = 1)
}
\keyword{ mpa }
\keyword{ corpus }