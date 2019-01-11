\name{BibToCorpus}
\alias{BibToCorpus}
\title{
Convert a bibliographic database into a text corpus
}
\description{
Get a text corpus from a bibliographic database with a control list and help options that allow you to run a faster process of composition of corpus.
}
\usage{
BibToCorpus(bibData, bibUnits = "Keywords", controlList, stopWords = TRUE,
  wordsToRemove, replaceWords)
}
\arguments{
  \item{bibData}{
a dataframe containing information about a bibliographic database.
}
  \item{bibUnits}{
a string, the bibliographic unit to be analyzed e.g. "Title", "Keywords", "Abstract". This string must match the column name from the "bibData" dataframe.
}
  \item{controlList}{
a vector indicating the transformations and processes that will be performed during the corpus composition process. Available options: \code{stripWhitespace} for collapsing white spaces; \code{removeNumbers} for removing numbers inside texts in corpus; 
}
  \item{stopWords}{
logical. If \code{TRUE}, a list of stop words will be removed from the composed corpus.
}
  \item{wordsToRemove}{
a vector of words that are desired to be removed from the composed corpus.
}
  \item{replaceWords}{
a \code{TXT} file (two columns separated by tab). One column containing the final word to be in the corpus and a second, containing the word to replace.
Example:
clustering	cluster_analysis
clustering	cluster
}
}
\details{
A list of stop words is provided inside the package for English language, if necessary, please visit https://sites.google.com/site/kevinbouge/stopwords-lists for a complete list of stop words in many other language, available thanks to Kevin Bouge (kevin.bouge@gmail.com)
}
\value{
An object inheriting from \code{VCorpus} and \code{Corpus}.
}
\author{
Andres Palacios \email{anfpalacioscl@unal.edu.co}
}
\seealso{
\code{\link{ArticleSearch}} can be useful for creating a bibliographic information dataframe if starting from scratch.
}
\examples{
data("KDVizData")
wordsToReplace <- system.file("extdata", "KDReplaceWords.txt", package = "KDViz")
wordsToRemove <- c("analysis", "data", "text", "review", "topic", "theory", "system", "protein")

myCorpus <- BibToCorpus(bibData = KDVizData, bibUnits = "Keywords",
  controlList = c("stripWhitespace", "removeNumbers"), stopWords = TRUE,
  wordsToRemove = wordsToRemove, replaceWords = wordsToReplace)
}
\keyword{ corpus }