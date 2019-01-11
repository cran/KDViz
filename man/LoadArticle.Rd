\name{LoadArticle}
\alias{LoadArticle}
\title{
Load the information stored in a bibliographic database
}
\description{
Load the main informaion of an article or navigate the website contained in a bibliographic database specifying the article row name.
}
\usage{
LoadArticle(articleData, articleName, browser = TRUE)
}
\arguments{
  \item{articleData}{
a dataframe containing the information of every article in the database.
}
  \item{articleName}{
a string indicating the name of the article inside the bibliographic database, this must coincide with thw row name from the article.
}
  \item{browser}{
logical. If \code{TRUE}, a web browser will open to visualize the article's website.
}
}
\value{
information about the selected article and, possibly, the website view from it.
}
\author{
Andres Palacios \email{anfpalacioscl@unal.edu.co}
}
\note{
Knowledge domain maps and original dataframe names should match, if this is not the case, please check the document-term matrix naming step in \code{DTMFromCorpus} step.
}
\examples{
data("KDVizData")

LoadArticle(articleData = KDVizData, articleName = "A251")
}
\keyword{ load website }