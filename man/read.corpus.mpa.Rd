\name{read.corpus.mpa}
\alias{read.corpus.mpa}
\title{
Reading corpus object
}
\description{
Reads a corpus object and transforms it to be able to use \code{matriz.mpa} from \code{mpa} package.
}
\usage{
read.corpus.mpa(corpus)
}
\arguments{
  \item{corpus}{
a corpus.
}
}
\value{
An object inheriting from \code{VCorpus} and \code{Corpus}.
}
\author{
Andres Palacios \email{anfpalacioscl@unal.edu.co}
}
\examples{
data("KDCorpus")

corpusMPA <- read.corpus.mpa(KDCorpus)
}
\keyword{ mpa }
\keyword{ corpus }
