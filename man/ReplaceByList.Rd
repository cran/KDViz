\name{ReplaceByList}
\alias{ReplaceByList}
\title{
Replace a list of words by another
}
\description{
Optimized replacement of words inside a text corpus.
}
\usage{
ReplaceByList(corpus, wordsFile)
}
\arguments{
  \item{corpus}{
a \code{VCorpus} or \code{Corpus} object.
}
  \item{wordsFile}{
a \code{TXT} file (two columns separated by tab). One column containing the final word to be in the corpus and a second, containing the word to replace.
Example:
clustering	cluster_analysis
clustering	cluster
}
}
\details{
This function is provided as a replacing words step in a corpus composition process, this list must have a .txt file extension made by two columns, one for words of replacement and other for words to be replaced.
}
\value{
An object inheriting from \code{VCorpus} and \code{Corpus}.
}
\author{
Camila Gongora \email{mcgongoraa@unal.edu.co},
Andres Palacios \email{anfpalacioscl@unal.edu.co}
}
\seealso{
See also \code{\link{BibToCorpus}} where this function is used internally.
}
\examples{
data("KDVizData")
wordsToReplace <- system.file("extdata", "KDReplaceWords.txt", package = "KDViz")
myCorpus <- BibToCorpus(bibData = KDVizData, bibUnits = "Keywords",
  controlList = c("stripWhitespace", "removeNumbers"), stopWords = TRUE)

myNewCorpus <- ReplaceByList(myCorpus, wordsFile = wordsToReplace)
}
\keyword{ replace words }
\keyword{ replacement }
\keyword{ corpus }
